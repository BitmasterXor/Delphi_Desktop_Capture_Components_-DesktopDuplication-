unit DesktopCapture;

interface

uses
  Windows, Classes, SysUtils, Forms, Controls, ExtCtrls, Graphics, Types,
  System.Threading, System.SyncObjs, ActiveX, ComObj, MultiMon,
  DX12.D3D11, DX12.D3DCommon, DX12.DXGI, DX12.DXGI1_2, Math,
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
    FMonitorSelection: TMonitorSelection;
    FMonitorIndex: Integer;

    // Internal state
    FCaptureThread: TThread;
    FFirstFrameSent: Boolean;
    FPreviousFrame: TBytes;
    FPreviousWidth, FPreviousHeight: Integer;

    // Current frame dimensions and coordinates
    FCurrentWidth, FCurrentHeight: Integer;
    FCaptureLeft, FCaptureTop: Integer; // Screen coordinates of capture area

    // DXGI objects
    FD3DDevice: ID3D11Device;
    FD3DContext: ID3D11DeviceContext;
    FOutputDuplication: IDXGIOutputDuplication;
    FDXGIInitialized: Boolean;
    FDXGIOutputIndex: Integer; // Which output we're duplicating

    // Multi-monitor support
    FMonitors: array of TMonitorInfo;

    // Cursor handling - FIXED
    FLastCursorPos: TPoint;
    FLastCursorVisible: Boolean;

    // Events
    FOnFrameCaptured: TOnFrameCaptured;

    // Property setters
    procedure SetActive(const Value: Boolean);
    procedure SetMethod(const Value: TCaptureMethod);
    procedure SetMode(const Value: TCaptureMode);
    procedure SetTargetFPS(const Value: Integer);
    procedure SetMonitorSelection(const Value: TMonitorSelection);
    procedure SetMonitorIndex(const Value: Integer);

    // Internal methods
    procedure StartCapture;
    procedure StopCapture;
    procedure EnumerateMonitors;
    procedure InitializeDXGI;
    procedure CleanupDXGI;
    function GetTargetMonitor: TMonitorInfo;
    function CaptureGDI: TBytes;
    function CaptureDXGI: TBytes;
    function CreateFrameBytes(const PixelData: TBytes; Width, Height: Integer;
      IsFullFrame: Boolean; const DirtyRegions: TArray<TDirtyRegion>): TBytes;
    function DetectDirtyRegions(const Current, Previous: TBytes; Width, Height: Integer): TArray<TDirtyRegion>;

    // FIXED cursor methods
    procedure DrawCursorFixed(var PixelData: TBytes; Width, Height: Integer);
    procedure DrawArrowCursor(var PixelData: TBytes; Width, Height, X, Y: Integer);
    function HasCursorMoved: Boolean;
    procedure SetPixelSafe(var PixelData: TBytes; Width, Height, X, Y: Integer; Color: Cardinal);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Public methods
    function GetMonitorCount: Integer;
    function GetMonitorInfo(Index: Integer): TMonitorInfo;

  published
    property Active: Boolean read FActive write SetActive default False;
    property Method: TCaptureMethod read FMethod write SetMethod default cmDXGI;
    property Mode: TCaptureMode read FMode write SetMode default cmDirtyOnly;
    property TargetFPS: Integer read FTargetFPS write SetTargetFPS default 30;
    property IncludeCursor: Boolean read FIncludeCursor write FIncludeCursor default True;
    property MonitorSelection: TMonitorSelection read FMonitorSelection write SetMonitorSelection default msDefault;
    property MonitorIndex: Integer read FMonitorIndex write SetMonitorIndex default 0;

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

// Monitor enumeration callback
function MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect; Data: LPARAM): Boolean; stdcall;

procedure Register;

implementation

uses
  DesktopCaptureUtils;

var
  GlobalMonitors: ^TArray<TMonitorInfo>;

function MonitorEnumProc(hm: HMONITOR; dc: HDC; r: PRect; Data: LPARAM): Boolean; stdcall;
var
  MonitorInfo: TMonitorInfoEx;
  Count: Integer;
begin
  Result := True;

  MonitorInfo.cbSize := SizeOf(TMonitorInfoEx);
  if GetMonitorInfo(hm, @MonitorInfo) then
  begin
    Count := Length(GlobalMonitors^);
    SetLength(GlobalMonitors^, Count + 1);

    GlobalMonitors^[Count].MonitorIndex := Count;
    GlobalMonitors^[Count].Handle := hm;
    GlobalMonitors^[Count].Left := MonitorInfo.rcMonitor.Left;
    GlobalMonitors^[Count].Top := MonitorInfo.rcMonitor.Top;
    GlobalMonitors^[Count].Right := MonitorInfo.rcMonitor.Right;
    GlobalMonitors^[Count].Bottom := MonitorInfo.rcMonitor.Bottom;
    GlobalMonitors^[Count].IsPrimary := (MonitorInfo.dwFlags and MONITORINFOF_PRIMARY) <> 0;
  end;
end;

{ TDesktopCapture }

constructor TDesktopCapture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FActive := False;
  FMethod := cmDXGI;
  FMode := cmDirtyOnly;
  FTargetFPS := 30;
  FIncludeCursor := True;
  FMonitorSelection := msDefault;
  FMonitorIndex := 0;
  FFirstFrameSent := False;
  FDXGIInitialized := False;
  FPreviousWidth := 0;
  FPreviousHeight := 0;
  FDXGIOutputIndex := 0;

  // Initialize cursor tracking
  FLastCursorPos := Point(-1, -1);
  FLastCursorVisible := False;

  EnumerateMonitors;

  // Set defaults based on primary monitor
  FCurrentWidth := GetSystemMetrics(SM_CXSCREEN);
  FCurrentHeight := GetSystemMetrics(SM_CYSCREEN);
  FCaptureLeft := 0;
  FCaptureTop := 0;
end;

destructor TDesktopCapture.Destroy;
begin
  Active := False;
  inherited Destroy;
end;

procedure TDesktopCapture.EnumerateMonitors;
begin
  SetLength(FMonitors, 0);
  GlobalMonitors := @FMonitors;
  EnumDisplayMonitors(0, nil, @MonitorEnumProc, 0);
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

procedure TDesktopCapture.SetMonitorSelection(const Value: TMonitorSelection);
begin
  if not FActive then
  begin
    FMonitorSelection := Value;
    FFirstFrameSent := False;
    SetLength(FPreviousFrame, 0);
  end;
end;

procedure TDesktopCapture.SetMonitorIndex(const Value: Integer);
begin
  if not FActive and (Value >= 0) then
  begin
    FMonitorIndex := Value;
    FFirstFrameSent := False;
    SetLength(FPreviousFrame, 0);
  end;
end;

function TDesktopCapture.GetTargetMonitor: TMonitorInfo;
var
  i: Integer;
begin
  EnumerateMonitors; // Refresh monitor list

  case FMonitorSelection of
    msPrimary:
    begin
      for i := 0 to High(FMonitors) do
        if FMonitors[i].IsPrimary then
        begin
          Result := FMonitors[i];
          Exit;
        end;
      // Fallback to first monitor
      if Length(FMonitors) > 0 then
        Result := FMonitors[0]
      else
      begin
        // No monitors found, use desktop
        Result.MonitorIndex := 0;
        Result.Handle := 0;
        Result.Left := 0;
        Result.Top := 0;
        Result.Right := GetSystemMetrics(SM_CXSCREEN);
        Result.Bottom := GetSystemMetrics(SM_CYSCREEN);
        Result.IsPrimary := True;
      end;
    end;

    msSpecific:
    begin
      if (FMonitorIndex >= 0) and (FMonitorIndex < Length(FMonitors)) then
        Result := FMonitors[FMonitorIndex]
      else if Length(FMonitors) > 0 then
        Result := FMonitors[0]
      else
      begin
        // Fallback to desktop
        Result.MonitorIndex := 0;
        Result.Handle := 0;
        Result.Left := 0;
        Result.Top := 0;
        Result.Right := GetSystemMetrics(SM_CXSCREEN);
        Result.Bottom := GetSystemMetrics(SM_CYSCREEN);
        Result.IsPrimary := True;
      end;
    end;

  else // msDefault
    begin
      // Use virtual desktop (all monitors)
      Result.MonitorIndex := -1;
      Result.Handle := 0;
      Result.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
      Result.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
      Result.Right := Result.Left + GetSystemMetrics(SM_CXVIRTUALSCREEN);
      Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVIRTUALSCREEN);
      Result.IsPrimary := False;
    end;
  end;
end;

procedure TDesktopCapture.StartCapture;
var
  Monitor: TMonitorInfo;
begin
  if Assigned(FCaptureThread) then Exit;

  FFirstFrameSent := False;
  SetLength(FPreviousFrame, 0);

  // Reset cursor tracking
  FLastCursorPos := Point(-1, -1);
  FLastCursorVisible := False;

  // Get target monitor info
  Monitor := GetTargetMonitor;
  FCaptureLeft := Monitor.Left;
  FCaptureTop := Monitor.Top;
  FCurrentWidth := Monitor.Right - Monitor.Left;
  FCurrentHeight := Monitor.Bottom - Monitor.Top;

  if FMethod = cmDXGI then
  begin
    try
      // Set the output index for DXGI based on monitor selection
      case FMonitorSelection of
        msDefault, msPrimary: FDXGIOutputIndex := 0;
        msSpecific: FDXGIOutputIndex := FMonitorIndex;
      else
        FDXGIOutputIndex := 0;
      end;

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
  OutputIndex: Integer;
  MaxRetries: Integer;
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

  // Try to get the correct output for multi-monitor support
  OutputIndex := FDXGIOutputIndex;
  MaxRetries := 3; // Try a few outputs in case the index is wrong

  repeat
    hr := DXGIAdapter.EnumOutputs(OutputIndex, DXGIOutput);
    if SUCCEEDED(hr) then
      Break;
    Inc(OutputIndex);
    Dec(MaxRetries);
  until MaxRetries <= 0;

  if FAILED(hr) then
  begin
    // Final fallback to output 0
    hr := DXGIAdapter.EnumOutputs(0, DXGIOutput);
    if FAILED(hr) then
      raise Exception.Create('Failed to get any DXGI output');
  end;

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
  Monitor: TMonitorInfo;
begin
  SetLength(Result, 0);

  Monitor := GetTargetMonitor;
  Width := Monitor.Right - Monitor.Left;
  Height := Monitor.Bottom - Monitor.Top;
  FCaptureLeft := Monitor.Left;
  FCaptureTop := Monitor.Top;
  FCurrentWidth := Width;
  FCurrentHeight := Height;

  ScreenDC := GetDC(0);
  try
    MemDC := CreateCompatibleDC(ScreenDC);
    try
      Bitmap := CreateCompatibleBitmap(ScreenDC, Width, Height);
      OldBitmap := SelectObject(MemDC, Bitmap);

      if BitBlt(MemDC, 0, 0, Width, Height, ScreenDC, FCaptureLeft, FCaptureTop, SRCCOPY) then
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
            DrawCursorFixed(PixelData, Width, Height);
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

{
  FIXED DXGI Frame Capture Function - I have applied c-michail's AccumulatedFrames fix!
  =================================================================================
}
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

    // An AccumulatedFrames value of 0 indicates no new desktop image was presented
    // and we're dealing with a null/empty surface that should not be processed
    if FrameInfo.AccumulatedFrames = 0 then
    begin
      FOutputDuplication.ReleaseFrame;
      Exit;
    end;

    FrameAcquired := True;

    hr := DesktopResource.QueryInterface(IID_ID3D11Texture2D, DesktopTexture);
    if FAILED(hr) then Exit;

    DesktopTexture.GetDesc(Desc);
    FCurrentWidth := Integer(Desc.Width);
    FCurrentHeight := Integer(Desc.Height);

    // Create staging texture for CPU access
    Desc.Usage := D3D11_USAGE_STAGING;
    Desc.CPUAccessFlags := UINT(D3D11_CPU_ACCESS_READ);
    Desc.BindFlags := 0;
    Desc.MiscFlags := 0;

    hr := FD3DDevice.CreateTexture2D(@Desc, nil, StagingTexture);
    if FAILED(hr) then Exit;

    // Copy desktop texture to staging texture
    FD3DContext.CopyResource(StagingTexture, DesktopTexture);

    // Map the staging texture
    hr := FD3DContext.Map(StagingTexture, 0, D3D11_MAP_READ, 0, MappedSubresource);
    if SUCCEEDED(hr) then
    begin
      try
        SetLength(PixelData, FCurrentWidth * FCurrentHeight * 4);

        SrcPtr := PByte(MappedSubresource.pData);
        DestPtr := @PixelData[0];

        // Copy row by row (handle pitch)
        for y := 0 to FCurrentHeight - 1 do
        begin
          Move(SrcPtr^, DestPtr^, FCurrentWidth * 4);
          Inc(SrcPtr, MappedSubresource.RowPitch);
          Inc(DestPtr, FCurrentWidth * 4);
        end;

        if FIncludeCursor then
          DrawCursorFixed(PixelData, FCurrentWidth, FCurrentHeight);

        Result := PixelData;
      finally
        FD3DContext.Unmap(StagingTexture, 0);
      end;
    end;

  finally
    if FrameAcquired then
      FOutputDuplication.ReleaseFrame;
  end;
end;

function TDesktopCapture.CreateFrameBytes(const PixelData: TBytes; Width, Height: Integer; IsFullFrame: Boolean; const DirtyRegions: TArray<TDirtyRegion>): TBytes;
var
  Header: TFrameHeader;
  HeaderSize, RegionsSize: Integer;
  Offset, SrcOffset: Integer;
  i, x, y: Integer;
begin
  HeaderSize := SizeOf(TFrameHeader);
  RegionsSize := Length(DirtyRegions) * SizeOf(TDirtyRegion);

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

    // Copy regions (empty for full frame)
    Inc(Offset, RegionsSize);

    // Copy full frame data
    Move(PixelData[0], Result[Offset], Header.DataSize);
  end
  else
  begin
    // Calculate dirty pixels size
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

function TDesktopCapture.DetectDirtyRegions(const Current, Previous: TBytes; Width, Height: Integer): TArray<TDirtyRegion>;
var
  x, y, i: Integer;
  BlockSize: Integer;
  CurrentPixel, PreviousPixel: PLongWord;
  RegionCount: Integer;
const
  BLOCK_SIZE = 16; // Check in 16x16 pixel blocks for performance
begin
  SetLength(Result, 0);

  if (Length(Current) <> Length(Previous)) or (Length(Current) = 0) then
  begin
    // Full frame update if sizes don't match or no previous frame
    SetLength(Result, 1);
    Result[0].Left := 0;
    Result[0].Top := 0;
    Result[0].Right := Width;
    Result[0].Bottom := Height;
    Exit;
  end;

  RegionCount := 0;

  // Simple dirty region detection - check blocks
  for y := 0 to (Height div BLOCK_SIZE) do
  begin
    for x := 0 to (Width div BLOCK_SIZE) do
    begin
      // Check if this block has changes
      for i := 0 to (BLOCK_SIZE * BLOCK_SIZE - 1) do
      begin
        if ((y * BLOCK_SIZE + (i div BLOCK_SIZE)) < Height) and
           ((x * BLOCK_SIZE + (i mod BLOCK_SIZE)) < Width) then
        begin
          CurrentPixel := @Current[((y * BLOCK_SIZE + (i div BLOCK_SIZE)) * Width +
                                   (x * BLOCK_SIZE + (i mod BLOCK_SIZE))) * 4];
          PreviousPixel := @Previous[((y * BLOCK_SIZE + (i div BLOCK_SIZE)) * Width +
                                     (x * BLOCK_SIZE + (i mod BLOCK_SIZE))) * 4];

          if CurrentPixel^ <> PreviousPixel^ then
          begin
            // Add dirty region
            SetLength(Result, RegionCount + 1);
            Result[RegionCount].Left := x * BLOCK_SIZE;
            Result[RegionCount].Top := y * BLOCK_SIZE;
            Result[RegionCount].Right := Min((x + 1) * BLOCK_SIZE, Width);
            Result[RegionCount].Bottom := Min((y + 1) * BLOCK_SIZE, Height);
            Inc(RegionCount);
            Break;
          end;
        end;
      end;
    end;
  end;
end;

// FIXED: Draw cursor properly
procedure TDesktopCapture.DrawCursorFixed(var PixelData: TBytes; Width, Height: Integer);
var
  CursorInfo: TCursorInfo;
  IconInfo: TIconInfo;
  CursorPos: TPoint;
  CursorX, CursorY: Integer;
begin
  try
    CursorInfo.cbSize := SizeOf(TCursorInfo);
    if not GetCursorInfo(CursorInfo) or (CursorInfo.flags <> CURSOR_SHOWING) then
      Exit;

    if not GetCursorPos(CursorPos) then
      Exit;

    // Convert screen coordinates to capture area coordinates
    CursorX := CursorPos.X - FCaptureLeft;
    CursorY := CursorPos.Y - FCaptureTop;

    // Check if cursor is within capture area
    if (CursorX < -32) or (CursorY < -32) or (CursorX > Width + 32) or (CursorY > Height + 32) then
      Exit;

    if GetIconInfo(CursorInfo.hCursor, IconInfo) then
    begin
      try
        // Adjust cursor position by hotspot
        CursorX := CursorX - Integer(IconInfo.xHotspot);
        CursorY := CursorY - Integer(IconInfo.yHotspot);

        // Draw proper white arrow cursor
        DrawArrowCursor(PixelData, Width, Height, CursorX, CursorY);

      finally
        if IconInfo.hbmMask <> 0 then DeleteObject(IconInfo.hbmMask);
        if IconInfo.hbmColor <> 0 then DeleteObject(IconInfo.hbmColor);
      end;
    end
    else
    begin
      // Fallback - draw arrow cursor at position
      DrawArrowCursor(PixelData, Width, Height, CursorX, CursorY);
    end;

  except
    // Silently ignore cursor drawing errors
  end;
end;

// Helper function to safely set a pixel
procedure TDesktopCapture.SetPixelSafe(var PixelData: TBytes; Width, Height, X, Y: Integer; Color: Cardinal);
var
  PixelOffset: Integer;
begin
  if (X >= 0) and (X < Width) and (Y >= 0) and (Y < Height) then
  begin
    PixelOffset := (Y * Width + X) * 4;
    if PixelOffset + 3 < Length(PixelData) then
      PLongWord(@PixelData[PixelOffset])^ := Color;
  end;
end;

// FIXED: Proper white arrow cursor drawing
procedure TDesktopCapture.DrawArrowCursor(var PixelData: TBytes; Width, Height, X, Y: Integer);
var
  i, j: Integer;
  WhiteColor, BlackColor: Cardinal;
begin
  WhiteColor := $FFFFFFFF; // White in BGRA format (FF alpha, FF blue, FF green, FF red)
  BlackColor := $FF000000; // Black in BGRA format (FF alpha, 00 blue, 00 green, 00 red)

  // Draw arrow cursor with black outline and white fill
  // This creates a proper looking mouse cursor

  // Draw the main arrow shape (white fill)
  for i := 0 to 15 do
  begin
    // Left edge of arrow
    SetPixelSafe(PixelData, Width, Height, X, Y + i, WhiteColor);

    // Fill the arrow body
    for j := 1 to (i div 2) + 1 do
    begin
      if i <= 10 then
        SetPixelSafe(PixelData, Width, Height, X + j, Y + i, WhiteColor);
    end;

    // Special case for arrow point area
    if i <= 10 then
      SetPixelSafe(PixelData, Width, Height, X + (i div 2) + 1, Y + i, WhiteColor);
  end;

  // Draw the arrow tail (bottom part)
  for i := 11 to 15 do
  begin
    for j := 1 to 3 do
    begin
      SetPixelSafe(PixelData, Width, Height, X + j, Y + i, WhiteColor);
    end;
  end;

  // Draw black outline for better visibility
  // Top edge
  for i := 0 to 11 do
  begin
    SetPixelSafe(PixelData, Width, Height, X + (i div 2) + 2, Y + i, BlackColor);
  end;

  // Right edge of main body
  for i := 0 to 10 do
  begin
    SetPixelSafe(PixelData, Width, Height, X + (i div 2) + 2, Y + i, BlackColor);
  end;

  // Bottom edge
  for i := 11 to 15 do
  begin
    SetPixelSafe(PixelData, Width, Height, X + 4, Y + i, BlackColor);
  end;

  // Left edge outline
  SetPixelSafe(PixelData, Width, Height, X - 1, Y, BlackColor);
  for i := 1 to 15 do
  begin
    SetPixelSafe(PixelData, Width, Height, X - 1, Y + i, BlackColor);
  end;

  // Bottom outline
  for j := 0 to 4 do
  begin
    SetPixelSafe(PixelData, Width, Height, X + j, Y + 16, BlackColor);
  end;
end;

function TDesktopCapture.HasCursorMoved: Boolean;
var
  CurrentPos: TPoint;
  CurrentVisible: Boolean;
  CursorInfo: TCursorInfo;
begin
  Result := False;

  try
    if GetCursorPos(CurrentPos) then
    begin
      CursorInfo.cbSize := SizeOf(TCursorInfo);
      CurrentVisible := GetCursorInfo(CursorInfo) and (CursorInfo.flags = CURSOR_SHOWING);

      Result := (CurrentPos.X <> FLastCursorPos.X) or
                (CurrentPos.Y <> FLastCursorPos.Y) or
                (CurrentVisible <> FLastCursorVisible);

      FLastCursorPos := CurrentPos;
      FLastCursorVisible := CurrentVisible;
    end;
  except
    // Ignore errors
  end;
end;

function TDesktopCapture.GetMonitorCount: Integer;
begin
  EnumerateMonitors;
  Result := Length(FMonitors);
end;

function TDesktopCapture.GetMonitorInfo(Index: Integer): TMonitorInfo;
begin
  EnumerateMonitors;
  if (Index >= 0) and (Index < Length(FMonitors)) then
    Result := FMonitors[Index]
  else
  begin
    // Return default monitor info
    Result.MonitorIndex := 0;
    Result.Handle := 0;
    Result.Left := 0;
    Result.Top := 0;
    Result.Right := GetSystemMetrics(SM_CXSCREEN);
    Result.Bottom := GetSystemMetrics(SM_CYSCREEN);
    Result.IsPrimary := True;
  end;
end;

{ TCaptureThread }

constructor TCaptureThread.Create(AOwner: TDesktopCapture);
begin
  FOwner := AOwner;
  FInterval := 1000 div AOwner.FTargetFPS;
  inherited Create(True);
end;

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

      Width := FOwner.FCurrentWidth;
      Height := FOwner.FCurrentHeight;

      // Check if dimensions changed
      if (Width <> FOwner.FPreviousWidth) or (Height <> FOwner.FPreviousHeight) then
      begin
        FOwner.FFirstFrameSent := False;
        SetLength(FOwner.FPreviousFrame, 0);
        FOwner.FPreviousWidth := Width;
        FOwner.FPreviousHeight := Height;
      end;

      // Determine frame type and regions
      case FOwner.FMode of
        cmFullFrame:
        begin
          IsFullFrame := True;
          SetLength(DirtyRegions, 0);
        end;

        cmDirtyOnly:
        begin
          if not FOwner.FFirstFrameSent or (Length(FOwner.FPreviousFrame) = 0) then
          begin
            IsFullFrame := True;
            SetLength(DirtyRegions, 0);
            FOwner.FFirstFrameSent := True;
          end
          else
          begin
            DirtyRegions := FOwner.DetectDirtyRegions(PixelData, FOwner.FPreviousFrame, Width, Height);
            IsFullFrame := (Length(DirtyRegions) = 1) and
                          (DirtyRegions[0].Left = 0) and (DirtyRegions[0].Top = 0) and
                          (DirtyRegions[0].Right = Width) and (DirtyRegions[0].Bottom = Height);

            // Skip if no changes
            if Length(DirtyRegions) = 0 then
            begin
              Sleep(10);
              Continue;
            end;
          end;
        end;
      end;

      // Create frame data
      FrameBytes := FOwner.CreateFrameBytes(PixelData, Width, Height, IsFullFrame, DirtyRegions);

      // Store current frame for next comparison
      if FOwner.FMode = cmDirtyOnly then
      begin
        SetLength(FOwner.FPreviousFrame, Length(PixelData));
        Move(PixelData[0], FOwner.FPreviousFrame[0], Length(PixelData));
      end;

      // Fire event
      if Assigned(FOwner.FOnFrameCaptured) and (Length(FrameBytes) > 0) then
      begin
        FOwner.FOnFrameCaptured(FOwner, FrameBytes, IsFullFrame);
      end;

      LastCapture := GetTickCount;

    except
      on E: Exception do
      begin
        // Log error and continue
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
