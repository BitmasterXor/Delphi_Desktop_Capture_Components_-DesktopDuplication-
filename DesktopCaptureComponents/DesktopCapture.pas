unit DesktopCapture;

{
  ========================================================================
  TDesktopCapture - DIRTY MODE FIXED
  ========================================================================

  FIXED: Dirty pixel detection and frame creation
}

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, Types,
  System.Threading, System.SyncObjs, ActiveX, ComObj,
  DX12.D3D11, DX12.D3DCommon, DX12.DXGI, DX12.DXGI1_2,Math,
  DesktopCaptureTypes;

const
  DXGI_ERROR_WAIT_TIMEOUT = HRESULT($887A0025);
  DXGI_ERROR_ACCESS_LOST = HRESULT($887A0026);

type
  TDesktopCapture = class(TComponent)
  private
    // Core properties
    FActive: Boolean;
    FMethod: TCaptureMethod;
    FMode: TCaptureMode;
    FTargetFPS: Integer;
    FIncludeCursor: Boolean;

    // Internal state
    FCaptureThread: TThread;
    FFirstFrameSent: Boolean;
    FPreviousFrame: TBytes;
    FPreviousWidth, FPreviousHeight: Integer;

    // Current frame dimensions
    FCurrentWidth, FCurrentHeight: Integer;

    // DXGI objects
    FD3DDevice: ID3D11Device;
    FD3DContext: ID3D11DeviceContext;
    FOutputDuplication: IDXGIOutputDuplication;
    FDXGIInitialized: Boolean;

    // Events
    FOnFrameCaptured: TOnFrameCaptured;

    // Property setters
    procedure SetActive(const Value: Boolean);
    procedure SetMethod(const Value: TCaptureMethod);
    procedure SetMode(const Value: TCaptureMode);
    procedure SetTargetFPS(const Value: Integer);

    // Internal methods
    procedure StartCapture;
    procedure StopCapture;
    procedure InitializeDXGI;
    procedure CleanupDXGI;
    function CaptureGDI: TBytes;
    function CaptureDXGI: TBytes;
    function CreateFrameBytes(const PixelData: TBytes; Width, Height: Integer; IsFullFrame: Boolean; const DirtyRegions: TArray<TDirtyRegion>): TBytes;
    function DetectDirtyRegions(const Current, Previous: TBytes; Width, Height: Integer): TArray<TDirtyRegion>;
    procedure DrawCursor(var PixelData: TBytes; Width, Height: Integer);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Active: Boolean read FActive write SetActive default False;
    property Method: TCaptureMethod read FMethod write SetMethod default cmDXGI;
    property Mode: TCaptureMode read FMode write SetMode default cmDirtyOnly;
    property TargetFPS: Integer read FTargetFPS write SetTargetFPS default 30;
    property IncludeCursor: Boolean read FIncludeCursor write FIncludeCursor default True;

    property OnFrameCaptured: TOnFrameCaptured read FOnFrameCaptured write FOnFrameCaptured;
  end;

type
  TCaptureThread = class(TThread)
  private
    FOwner: TDesktopCapture;
    FInterval: Integer;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TDesktopCapture);
  end;

procedure Register;

implementation

{ TDesktopCapture }

constructor TDesktopCapture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FMethod := cmDXGI;
  FMode := cmDirtyOnly;
  FTargetFPS := 30;
  FIncludeCursor := True;
  FFirstFrameSent := False;
  FDXGIInitialized := False;
  FPreviousWidth := 0;
  FPreviousHeight := 0;
  FCurrentWidth := GetSystemMetrics(SM_CXSCREEN);
  FCurrentHeight := GetSystemMetrics(SM_CYSCREEN);
end;

destructor TDesktopCapture.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TDesktopCapture.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if FActive then
      StartCapture
    else
      StopCapture;
  end;
end;

procedure TDesktopCapture.SetMethod(const Value: TCaptureMethod);
begin
  if not FActive then
    FMethod := Value;
end;

procedure TDesktopCapture.SetMode(const Value: TCaptureMode);
begin
  FMode := Value;
  // Reset first frame flag when mode changes
  FFirstFrameSent := False;
  SetLength(FPreviousFrame, 0);
end;

procedure TDesktopCapture.SetTargetFPS(const Value: Integer);
begin
  if (Value > 0) and (Value <= 120) then
    FTargetFPS := Value;
end;

procedure TDesktopCapture.StartCapture;
begin
  if Assigned(FCaptureThread) then Exit;

  FFirstFrameSent := False;
  SetLength(FPreviousFrame, 0);

  if FMethod = cmDXGI then
  begin
    try
      InitializeDXGI;
    except
      // Fall back to GDI if DXGI fails
      FMethod := cmGDI;
    end;
  end;

  FCaptureThread := TCaptureThread.Create(Self);
  FCaptureThread.Start;
end;

procedure TDesktopCapture.StopCapture;
begin
  if Assigned(FCaptureThread) then
  begin
    FCaptureThread.Terminate;
    FCaptureThread.WaitFor;
    FCaptureThread.Free;
    FCaptureThread := nil;
  end;

  CleanupDXGI;
  SetLength(FPreviousFrame, 0);
end;

procedure TDesktopCapture.InitializeDXGI;
var
  hr: HRESULT;
  FeatureLevel: TD3D_FEATURE_LEVEL;
  DXGIDevice: IDXGIDevice;
  DXGIAdapter: IDXGIAdapter;
  DXGIOutput: IDXGIOutput;
  DXGIOutput1: IDXGIOutput1;
  FeatureLevels: array[0..1] of TD3D_FEATURE_LEVEL;
begin
  FDXGIInitialized := False;

  CoInitializeEx(nil, COINIT_APARTMENTTHREADED);

  FeatureLevels[0] := D3D_FEATURE_LEVEL_11_1;
  FeatureLevels[1] := D3D_FEATURE_LEVEL_11_0;

  hr := D3D11CreateDevice(nil, D3D_DRIVER_TYPE_HARDWARE, 0,
    UINT(D3D11_CREATE_DEVICE_BGRA_SUPPORT), @FeatureLevels[0], 2, D3D11_SDK_VERSION,
    FD3DDevice, FeatureLevel, FD3DContext);
  if FAILED(hr) then
    raise Exception.Create('Failed to create D3D11 device');

  hr := FD3DDevice.QueryInterface(IID_IDXGIDevice, DXGIDevice);
  if FAILED(hr) then
    raise Exception.Create('Failed to get DXGI device');

  hr := DXGIDevice.GetAdapter(DXGIAdapter);
  if FAILED(hr) then
    raise Exception.Create('Failed to get DXGI adapter');

  hr := DXGIAdapter.EnumOutputs(0, DXGIOutput);
  if FAILED(hr) then
    raise Exception.Create('Failed to get DXGI output');

  hr := DXGIOutput.QueryInterface(IID_IDXGIOutput1, DXGIOutput1);
  if FAILED(hr) then
    raise Exception.Create('Failed to get DXGI Output1');

  hr := DXGIOutput1.DuplicateOutput(FD3DDevice, FOutputDuplication);
  if FAILED(hr) then
    raise Exception.Create('Failed to create desktop duplication');

  FDXGIInitialized := True;
end;

procedure TDesktopCapture.CleanupDXGI;
begin
  if Assigned(FOutputDuplication) then
  begin
    try
      FOutputDuplication.ReleaseFrame;
    except
    end;
  end;

  FOutputDuplication := nil;
  FD3DContext := nil;
  FD3DDevice := nil;
  FDXGIInitialized := False;
end;

function TDesktopCapture.CaptureGDI: TBytes;
var
  ScreenDC, MemDC: HDC;
  Bitmap: HBITMAP;
  OldBitmap: HBITMAP;
  BitmapInfo: TBitmapInfo;
  Width, Height: Integer;
  PixelData: TBytes;
begin
  SetLength(Result, 0);

  Width := GetSystemMetrics(SM_CXSCREEN);
  Height := GetSystemMetrics(SM_CYSCREEN);
  FCurrentWidth := Width;
  FCurrentHeight := Height;

  ScreenDC := GetDC(0);
  try
    MemDC := CreateCompatibleDC(ScreenDC);
    try
      Bitmap := CreateCompatibleBitmap(ScreenDC, Width, Height);
      OldBitmap := SelectObject(MemDC, Bitmap);

      if BitBlt(MemDC, 0, 0, Width, Height, ScreenDC, 0, 0, SRCCOPY) then
      begin
        ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
        BitmapInfo.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
        BitmapInfo.bmiHeader.biWidth := Width;
        BitmapInfo.bmiHeader.biHeight := -Height; // Top-down
        BitmapInfo.bmiHeader.biPlanes := 1;
        BitmapInfo.bmiHeader.biBitCount := 32;
        BitmapInfo.bmiHeader.biCompression := BI_RGB;

        SetLength(PixelData, Width * Height * 4);
        if GetDIBits(MemDC, Bitmap, 0, Height, @PixelData[0], BitmapInfo, DIB_RGB_COLORS) > 0 then
        begin
          if FIncludeCursor then
            DrawCursor(PixelData, Width, Height);
          Result := PixelData;
        end;
      end;

      SelectObject(MemDC, OldBitmap);
      DeleteObject(Bitmap);
    finally
      DeleteDC(MemDC);
    end;
  finally
    ReleaseDC(0, ScreenDC);
  end;
end;

function TDesktopCapture.CaptureDXGI: TBytes;
var
  hr: HRESULT;
  FrameInfo: TDXGI_OUTDUPL_FRAME_INFO;
  DesktopResource: IDXGIResource;
  DesktopTexture: ID3D11Texture2D;
  StagingTexture: ID3D11Texture2D;
  Desc: TD3D11_TEXTURE2D_DESC;
  MappedSubresource: TD3D11_MAPPED_SUBRESOURCE;
  y: Integer;
  SrcPtr, DestPtr: PByte;
  PixelData: TBytes;
  FrameAcquired: Boolean;
begin
  SetLength(Result, 0);
  FrameAcquired := False;

  if not FDXGIInitialized then Exit;

  try
    hr := FOutputDuplication.AcquireNextFrame(1000, FrameInfo, DesktopResource);

    if hr = DXGI_ERROR_WAIT_TIMEOUT then
    begin
      // No new frame, fall back to GDI for static content
      Result := CaptureGDI;
      Exit;
    end;

    if FAILED(hr) then
    begin
      if hr = DXGI_ERROR_ACCESS_LOST then
      begin
        CleanupDXGI;
        InitializeDXGI;
      end;
      Exit;
    end;

    FrameAcquired := True;

    hr := DesktopResource.QueryInterface(IID_ID3D11Texture2D, DesktopTexture);
    if FAILED(hr) then Exit;

    DesktopTexture.GetDesc(Desc);
    FCurrentWidth := Desc.Width;
    FCurrentHeight := Desc.Height;

    // Create staging texture
    Desc.Usage := D3D11_USAGE_STAGING;
    Desc.CPUAccessFlags := Ord(D3D11_CPU_ACCESS_READ);
    Desc.BindFlags := 0;
    Desc.MiscFlags := 0;

    hr := FD3DDevice.CreateTexture2D(@Desc, nil, StagingTexture);
    if FAILED(hr) then Exit;

    FD3DContext.CopyResource(StagingTexture, DesktopTexture);

    hr := FD3DContext.Map(StagingTexture, 0, D3D11_MAP_READ, 0, MappedSubresource);
    if FAILED(hr) then Exit;

    try
      SetLength(PixelData, Desc.Width * Desc.Height * 4);
      SrcPtr := PByte(MappedSubresource.pData);
      DestPtr := @PixelData[0];

      for y := 0 to Integer(Desc.Height) - 1 do
      begin
        Move(SrcPtr^, DestPtr^, Desc.Width * 4);
        Inc(SrcPtr, MappedSubresource.RowPitch);
        Inc(DestPtr, Desc.Width * 4);
      end;

      if FIncludeCursor then
        DrawCursor(PixelData, Desc.Width, Desc.Height);

      Result := PixelData;
    finally
      FD3DContext.Unmap(StagingTexture, 0);
    end;

  finally
    if FrameAcquired then
    begin
      try
        FOutputDuplication.ReleaseFrame;
      except
      end;
    end;
  end;
end;

procedure TDesktopCapture.DrawCursor(var PixelData: TBytes; Width, Height: Integer);
var
  CursorInfo: TCursorInfo;
  IconInfo: TIconInfo;
  CursorPos: TPoint;
  DC: HDC;
  Bitmap: HBITMAP;
  OldBitmap: HBITMAP;
  BitmapInfo: TBitmapInfo;
  CursorBits: TBytes;
  CursorSize, x, y, fx, fy: Integer;
begin
  CursorInfo.cbSize := SizeOf(CursorInfo);
  if not GetCursorInfo(CursorInfo) or (CursorInfo.flags <> CURSOR_SHOWING) then
    Exit;

  if not GetIconInfo(CursorInfo.hCursor, IconInfo) then
    Exit;

  try
    CursorPos := CursorInfo.ptScreenPos;
    CursorSize := 32; // Standard cursor size

    DC := CreateCompatibleDC(0);
    try
      Bitmap := CreateCompatibleBitmap(GetDC(0), CursorSize, CursorSize);
      OldBitmap := SelectObject(DC, Bitmap);

      // Draw cursor to bitmap
      FillRect(DC, Rect(0, 0, CursorSize, CursorSize), GetStockObject(BLACK_BRUSH));
      DrawIconEx(DC, 0, 0, CursorInfo.hCursor, CursorSize, CursorSize, 0, 0, DI_NORMAL);

      // Get cursor pixel data
      ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
      BitmapInfo.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
      BitmapInfo.bmiHeader.biWidth := CursorSize;
      BitmapInfo.bmiHeader.biHeight := -CursorSize;
      BitmapInfo.bmiHeader.biPlanes := 1;
      BitmapInfo.bmiHeader.biBitCount := 32;
      BitmapInfo.bmiHeader.biCompression := BI_RGB;

      SetLength(CursorBits, CursorSize * CursorSize * 4);
      if GetDIBits(DC, Bitmap, 0, CursorSize, @CursorBits[0], BitmapInfo, DIB_RGB_COLORS) > 0 then
      begin
        // Blend cursor onto frame
        for y := 0 to CursorSize - 1 do
        begin
          fy := CursorPos.Y - Integer(IconInfo.yHotspot) + y;
          if (fy >= 0) and (fy < Height) then
          begin
            for x := 0 to CursorSize - 1 do
            begin
              fx := CursorPos.X - Integer(IconInfo.xHotspot) + x;
              if (fx >= 0) and (fx < Width) then
              begin
                // Simple alpha blend (cursor pixels with alpha > 128 are drawn)
                if (CursorBits[(y * CursorSize + x) * 4 + 3] > 128) then
                begin
                  Move(CursorBits[(y * CursorSize + x) * 4], PixelData[(fy * Width + fx) * 4], 4);
                end;
              end;
            end;
          end;
        end;
      end;

      SelectObject(DC, OldBitmap);
      DeleteObject(Bitmap);
    finally
      DeleteDC(DC);
    end;
  finally
    if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
    if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
  end;
end;

// FIXED: Improved dirty region detection
function TDesktopCapture.DetectDirtyRegions(const Current, Previous: TBytes; Width, Height: Integer): TArray<TDirtyRegion>;
const
  BLOCK_SIZE = 32; // Smaller blocks for better detection
var
  BlocksX, BlocksY: Integer;
  x, y, px, py: Integer;
  IsDirty: Boolean;
  Count: Integer;
  CurrentPixel, PreviousPixel: PCardinal;
begin
  SetLength(Result, 0);

  if (Length(Current) <> Length(Previous)) or (Length(Current) = 0) or
     (Width <= 0) or (Height <= 0) then
    Exit;

  BlocksX := (Width + BLOCK_SIZE - 1) div BLOCK_SIZE;
  BlocksY := (Height + BLOCK_SIZE - 1) div BLOCK_SIZE;

  SetLength(Result, BlocksX * BlocksY);
  Count := 0;

  for y := 0 to BlocksY - 1 do
  begin
    for x := 0 to BlocksX - 1 do
    begin
      IsDirty := False;

      // Check every 4th pixel in the block for performance
      for py := y * BLOCK_SIZE to Min((y + 1) * BLOCK_SIZE - 1, Height - 1) do
      begin
        for px := x * BLOCK_SIZE to Min((x + 1) * BLOCK_SIZE - 1, Width - 1) do
        begin
          if ((py mod 2) = 0) and ((px mod 2) = 0) then // Sample every 2nd pixel
          begin
            CurrentPixel := PCardinal(@Current[(py * Width + px) * 4]);
            PreviousPixel := PCardinal(@Previous[(py * Width + px) * 4]);

            if CurrentPixel^ <> PreviousPixel^ then
            begin
              IsDirty := True;
              Break;
            end;
          end;
        end;
        if IsDirty then Break;
      end;

      if IsDirty then
      begin
        Result[Count].Left := x * BLOCK_SIZE;
        Result[Count].Top := y * BLOCK_SIZE;
        Result[Count].Right := Min((x + 1) * BLOCK_SIZE, Width);
        Result[Count].Bottom := Min((y + 1) * BLOCK_SIZE, Height);
        Inc(Count);
      end;
    end;
  end;

  SetLength(Result, Count);
end;

// FIXED: Simplified and corrected frame creation
function TDesktopCapture.CreateFrameBytes(const PixelData: TBytes; Width, Height: Integer; IsFullFrame: Boolean; const DirtyRegions: TArray<TDirtyRegion>): TBytes;
var
  Header: TFrameHeader;
  HeaderSize, RegionsSize: Integer;
  Offset: Integer;
  i, x, y: Integer;
  SrcOffset: Integer;
begin
  // Calculate sizes
  HeaderSize := SizeOf(TFrameHeader);
  RegionsSize := Length(DirtyRegions) * SizeOf(TDirtyRegion);

  // Fill header
  Header.IsFullFrame := IsFullFrame;
  Header.Width := Width;
  Header.Height := Height;
  Header.DirtyRegionCount := Length(DirtyRegions);

  if IsFullFrame then
  begin
    Header.DataSize := Length(PixelData);
    SetLength(Result, HeaderSize + RegionsSize + Header.DataSize);

    // Copy header
    Move(Header, Result[0], HeaderSize);
    Offset := HeaderSize;

    // Copy regions (none for full frame)
    Inc(Offset, RegionsSize);

    // Copy full pixel data
    Move(PixelData[0], Result[Offset], Header.DataSize);
  end
  else
  begin
    // Calculate dirty data size
    Header.DataSize := 0;
    for i := 0 to High(DirtyRegions) do
      Header.DataSize := Header.DataSize + ((DirtyRegions[i].Right - DirtyRegions[i].Left) * (DirtyRegions[i].Bottom - DirtyRegions[i].Top) * 4);

    SetLength(Result, HeaderSize + RegionsSize + Header.DataSize);

    // Copy header
    Move(Header, Result[0], HeaderSize);
    Offset := HeaderSize;

    // Copy regions
    if Length(DirtyRegions) > 0 then
    begin
      Move(DirtyRegions[0], Result[Offset], RegionsSize);
    end;
    Inc(Offset, RegionsSize);

    // Copy only dirty pixels
    for i := 0 to High(DirtyRegions) do
    begin
      for y := DirtyRegions[i].Top to DirtyRegions[i].Bottom - 1 do
      begin
        for x := DirtyRegions[i].Left to DirtyRegions[i].Right - 1 do
        begin
          SrcOffset := (y * Width + x) * 4;
          if (SrcOffset + 4 <= Length(PixelData)) and (Offset + 4 <= Length(Result)) then
          begin
            Move(PixelData[SrcOffset], Result[Offset], 4);
            Inc(Offset, 4);
          end;
        end;
      end;
    end;
  end;
end;

{ TCaptureThread }

constructor TCaptureThread.Create(AOwner: TDesktopCapture);
begin
  FOwner := AOwner;
  FInterval := 1000 div AOwner.FTargetFPS;
  inherited Create(True);
end;

// FIXED: Better dirty mode logic
procedure TCaptureThread.Execute;
var
  LastCapture: Cardinal;
  PixelData: TBytes;
  Width, Height: Integer;
  DirtyRegions: TArray<TDirtyRegion>;
  FrameBytes: TBytes;
  IsFullFrame: Boolean;
begin
  LastCapture := 0;

  while not Terminated do
  begin
    if GetTickCount - LastCapture < Cardinal(FInterval) then
    begin
      Sleep(5);
      Continue;
    end;

    try
      // Capture screen
      case FOwner.FMethod of
        cmGDI: PixelData := FOwner.CaptureGDI;
        cmDXGI: PixelData := FOwner.CaptureDXGI;
      end;

      if Length(PixelData) = 0 then
      begin
        Sleep(50);
        Continue;
      end;

      // Get frame dimensions from last capture
      Width := FOwner.FCurrentWidth;
      Height := FOwner.FCurrentHeight;

      // Always send full frame first
      if not FOwner.FFirstFrameSent then
      begin
        IsFullFrame := True;
        SetLength(DirtyRegions, 0);
        FOwner.FFirstFrameSent := True;

        // Store for next comparison
        FOwner.FPreviousFrame := Copy(PixelData);
        FOwner.FPreviousWidth := Width;
        FOwner.FPreviousHeight := Height;
      end
      else
      begin
        // Determine frame type based on mode
        case FOwner.FMode of
          cmFullFrame:
          begin
            IsFullFrame := True;
            SetLength(DirtyRegions, 0);
          end;
          cmDirtyOnly:
          begin
            // Detect dirty regions
            if (Length(FOwner.FPreviousFrame) = Length(PixelData)) and
               (FOwner.FPreviousWidth = Width) and (FOwner.FPreviousHeight = Height) then
            begin
              DirtyRegions := FOwner.DetectDirtyRegions(PixelData, FOwner.FPreviousFrame, Width, Height);
              IsFullFrame := Length(DirtyRegions) = 0;

              // If no changes, skip this frame
              if Length(DirtyRegions) = 0 then
              begin
                Sleep(10);
                Continue;
              end;
            end
            else
            begin
              // Resolution changed - send full frame
              IsFullFrame := True;
              SetLength(DirtyRegions, 0);
            end;
          end;
        end;

        // Update previous frame
        FOwner.FPreviousFrame := Copy(PixelData);
        FOwner.FPreviousWidth := Width;
        FOwner.FPreviousHeight := Height;
      end;

      // Create frame bytes
      FrameBytes := FOwner.CreateFrameBytes(PixelData, Width, Height, IsFullFrame, DirtyRegions);

      // Fire event in main thread
      TThread.Synchronize(nil, procedure
      begin
        if Assigned(FOwner.FOnFrameCaptured) then
          FOwner.FOnFrameCaptured(FOwner, FrameBytes, IsFullFrame);
      end);

      LastCapture := GetTickCount;

    except
      on E: Exception do
      begin
        // Handle errors silently, continue capturing
        Sleep(100);
      end;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('Desktop Capture', [TDesktopCapture]);
end;

end.
