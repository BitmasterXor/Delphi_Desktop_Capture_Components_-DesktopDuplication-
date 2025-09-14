unit DesktopCaptureReceiver;

{
  ========================================================================
  TDesktopCaptureReceiver - DIRTY MODE FIXED
  ========================================================================

  FIXED: Dirty region reconstruction and frame handling
}

interface

uses
  Windows, Classes, SysUtils, Controls, ExtCtrls, Graphics, Types,
  System.SyncObjs, DesktopCaptureTypes,Math;

type
  TDesktopCaptureReceiver = class(TComponent)
  private
    // Core properties
    FActive: Boolean;
    FTargetImage: TImage;

    // Frame buffer
    FBaseFrame: TBytes;
    FBaseWidth: Integer;
    FBaseHeight: Integer;
    FHasBaseFrame: Boolean;

    // Display bitmap
    FDisplayBitmap: TBitmap;

    // Thread safety
    FCriticalSection: TCriticalSection;

    // Events
    FOnFrameReceived: TOnReceiverUpdate;

    // Internal methods
    procedure SetTargetImage(const Value: TImage);
    procedure SetActive(const Value: Boolean);
    procedure UpdateDisplay;
    procedure ApplyDirtyRegions(const FrameData: TBytes; const Regions: array of TDirtyRegion);

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Main method - call this with received TBytes
    procedure ReceiveFrameData(const FrameData: TBytes);

    // Utility methods
    procedure ClearDisplay;
    function HasValidFrame: Boolean;

    // Read-only properties
    property CurrentWidth: Integer read FBaseWidth;
    property CurrentHeight: Integer read FBaseHeight;

  published
    property Active: Boolean read FActive write SetActive default True;
    property TargetImage: TImage read FTargetImage write SetTargetImage;

    property OnFrameReceived: TOnReceiverUpdate read FOnFrameReceived write FOnFrameReceived;
  end;

procedure Register;

implementation

{ TDesktopCaptureReceiver }

constructor TDesktopCaptureReceiver.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FActive := True;
  FTargetImage := nil;
  FBaseWidth := 0;
  FBaseHeight := 0;
  FHasBaseFrame := False;

  FDisplayBitmap := TBitmap.Create;
  FDisplayBitmap.PixelFormat := pf32bit;

  FCriticalSection := TCriticalSection.Create;
end;

destructor TDesktopCaptureReceiver.Destroy;
begin
  if Assigned(FDisplayBitmap) then
    FDisplayBitmap.Free;

  if Assigned(FCriticalSection) then
    FCriticalSection.Free;

  inherited Destroy;
end;

procedure TDesktopCaptureReceiver.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FTargetImage) then
    FTargetImage := nil;
end;

procedure TDesktopCaptureReceiver.SetTargetImage(const Value: TImage);
begin
  if FTargetImage <> Value then
  begin
    if Assigned(FTargetImage) then
      FTargetImage.RemoveFreeNotification(Self);

    FTargetImage := Value;

    if Assigned(FTargetImage) then
    begin
      FTargetImage.FreeNotification(Self);
      UpdateDisplay; // Update if we have data
    end;
  end;
end;

procedure TDesktopCaptureReceiver.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TDesktopCaptureReceiver.ReceiveFrameData(const FrameData: TBytes);
var
  Header: TFrameHeader;
  Offset: Integer;
  Regions: array of TDirtyRegion;
  PixelData: TBytes;
begin
  if not FActive or (Length(FrameData) < SizeOf(TFrameHeader)) then
    Exit;

  FCriticalSection.Enter;
  try
    // Read header
    Move(FrameData[0], Header, SizeOf(TFrameHeader));
    Offset := SizeOf(TFrameHeader);

    // Read dirty regions if any
    if Header.DirtyRegionCount > 0 then
    begin
      SetLength(Regions, Header.DirtyRegionCount);
      if Header.DirtyRegionCount * SizeOf(TDirtyRegion) <= Length(FrameData) - Offset then
      begin
        Move(FrameData[Offset], Regions[0], Header.DirtyRegionCount * SizeOf(TDirtyRegion));
        Inc(Offset, Header.DirtyRegionCount * SizeOf(TDirtyRegion));
      end
      else
      begin
        // Corrupted data
        Exit;
      end;
    end;

    // Read pixel data
    if Header.DataSize > 0 then
    begin
      if Header.DataSize <= Length(FrameData) - Offset then
      begin
        SetLength(PixelData, Header.DataSize);
        Move(FrameData[Offset], PixelData[0], Header.DataSize);
      end
      else
      begin
        // Corrupted data
        Exit;
      end;
    end;

    if Header.IsFullFrame then
    begin
      // Full frame update
      FBaseFrame := Copy(PixelData);
      FBaseWidth := Header.Width;
      FBaseHeight := Header.Height;
      FHasBaseFrame := True;
    end
    else
    begin
      // Dirty frame update
      if FHasBaseFrame and (FBaseWidth = Header.Width) and (FBaseHeight = Header.Height) then
      begin
        ApplyDirtyRegions(PixelData, Regions);
      end
      else
      begin
        // Can't apply dirty update without matching base frame
        Exit;
      end;
    end;

  finally
    FCriticalSection.Leave;
  end;

  // Update display
  UpdateDisplay;

  // Fire event
  if Assigned(FOnFrameReceived) then
    FOnFrameReceived(Self, FBaseWidth, FBaseHeight);
end;

// FIXED: Better dirty region application
procedure TDesktopCaptureReceiver.ApplyDirtyRegions(const FrameData: TBytes; const Regions: array of TDirtyRegion);
var
  i, x, y: Integer;
  DataOffset: Integer;
  BaseOffset: Integer;
  RegionWidth, RegionHeight: Integer;
begin
  if not FHasBaseFrame or (Length(Regions) = 0) or (Length(FrameData) = 0) then
    Exit;

  DataOffset := 0;

  // Apply each dirty region
  for i := 0 to High(Regions) do
  begin
    RegionWidth := Regions[i].Right - Regions[i].Left;
    RegionHeight := Regions[i].Bottom - Regions[i].Top;

    // Bounds check
    if (Regions[i].Left < 0) or (Regions[i].Top < 0) or
       (Regions[i].Right > FBaseWidth) or (Regions[i].Bottom > FBaseHeight) then
      Continue;

    // Copy region pixel by pixel
    for y := 0 to RegionHeight - 1 do
    begin
      for x := 0 to RegionWidth - 1 do
      begin
        if DataOffset + 4 <= Length(FrameData) then
        begin
          BaseOffset := ((Regions[i].Top + y) * FBaseWidth + (Regions[i].Left + x)) * 4;
          if BaseOffset + 4 <= Length(FBaseFrame) then
          begin
            // Copy 4 bytes (BGRA pixel)
            Move(FrameData[DataOffset], FBaseFrame[BaseOffset], 4);
            Inc(DataOffset, 4);
          end
          else
          begin
            // Skip this pixel if out of bounds
            Inc(DataOffset, 4);
          end;
        end
        else
        begin
          // Not enough data
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TDesktopCaptureReceiver.UpdateDisplay;
var
  BitmapInfo: TBitmapInfo;
  DIBBits: Pointer;
begin
  if not FHasBaseFrame or not Assigned(FTargetImage) or (Length(FBaseFrame) = 0) then
    Exit;

  try
    // Create display bitmap
    FDisplayBitmap.Width := FBaseWidth;
    FDisplayBitmap.Height := FBaseHeight;

    // Setup bitmap info for DIB
    ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
    BitmapInfo.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
    BitmapInfo.bmiHeader.biWidth := FBaseWidth;
    BitmapInfo.bmiHeader.biHeight := -FBaseHeight; // Top-down
    BitmapInfo.bmiHeader.biPlanes := 1;
    BitmapInfo.bmiHeader.biBitCount := 32;
    BitmapInfo.bmiHeader.biCompression := BI_RGB;

    // Create DIB section
    DIBBits := nil;
    FDisplayBitmap.Handle := CreateDIBSection(0, BitmapInfo, DIB_RGB_COLORS, DIBBits, 0, 0);

    if DIBBits <> nil then
    begin
      // Copy frame data to bitmap
      Move(FBaseFrame[0], DIBBits^, Min(Length(FBaseFrame), FBaseWidth * FBaseHeight * 4));

      // Update target image
      FTargetImage.Picture.Assign(FDisplayBitmap);
    end;

  except
    on E: Exception do
    begin
      // Silently handle display errors
    end;
  end;
end;

procedure TDesktopCaptureReceiver.ClearDisplay;
begin
  FCriticalSection.Enter;
  try
    SetLength(FBaseFrame, 0);
    FBaseWidth := 0;
    FBaseHeight := 0;
    FHasBaseFrame := False;

    if Assigned(FTargetImage) then
    begin
      FTargetImage.Picture.Bitmap.Width := 1;
      FTargetImage.Picture.Bitmap.Height := 1;
    end;

  finally
    FCriticalSection.Leave;
  end;
end;

function TDesktopCaptureReceiver.HasValidFrame: Boolean;
begin
  Result := FHasBaseFrame and (FBaseWidth > 0) and (FBaseHeight > 0) and (Length(FBaseFrame) > 0);
end;

procedure Register;
begin
  RegisterComponents('Desktop Capture', [TDesktopCaptureReceiver]);
end;

end.
