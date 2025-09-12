unit DesktopCaptureUtils;

{
  ========================================================================
  Desktop Capture Utility Functions - SIMPLIFIED & FIXED
  ========================================================================
  Basic utility functions for the simplified desktop capture components
}

interface

uses
  Windows, SysUtils, Classes, Types, Graphics,
  DesktopCaptureTypes;

// System information functions
function GetScreenResolution: TPoint;
function GetVirtualDesktopRect: TRect;
function IsWindowsVistaOrLater: Boolean;
function IsWindows8OrLater: Boolean;
function HasDXGISupport: Boolean;

// Memory and data conversion functions
function CreateBitmapFromBytes(const Data: TBytes; Width, Height: Integer): TBitmap;
function BytesToBitmap(const PixelData: TBytes; Width, Height: Integer): TBitmap;

// String and formatting utilities
function CaptureMethodToString(Method: TCaptureMethod): string;
function CaptureModeToString(Mode: TCaptureMode): string;
function FormatBytesSize(Bytes: Int64): string;

// Error handling utilities
function GetLastWindowsError: string;

// Debug utilities
procedure LogDebugMessage(const Message: string);
function GetSystemInfo: string;

implementation

uses
  Math;

// =============================================================================
// SYSTEM INFORMATION FUNCTIONS
// =============================================================================

function GetScreenResolution: TPoint;
begin
  Result.X := GetSystemMetrics(SM_CXSCREEN);
  Result.Y := GetSystemMetrics(SM_CYSCREEN);
end;

function GetVirtualDesktopRect: TRect;
begin
  Result.Left := GetSystemMetrics(SM_XVIRTUALSCREEN);
  Result.Top := GetSystemMetrics(SM_YVIRTUALSCREEN);
  Result.Right := Result.Left + GetSystemMetrics(SM_CXVIRTUALSCREEN);
  Result.Bottom := Result.Top + GetSystemMetrics(SM_CYVIRTUALSCREEN);
end;

function IsWindowsVistaOrLater: Boolean;
begin
  Result := Win32MajorVersion >= 6;
end;

function IsWindows8OrLater: Boolean;
begin
  Result := (Win32MajorVersion > 6) or ((Win32MajorVersion = 6) and (Win32MinorVersion >= 2));
end;

function HasDXGISupport: Boolean;
begin
  // DXGI Desktop Duplication requires Windows 8 or later
  Result := IsWindows8OrLater;
end;

// =============================================================================
// MEMORY AND DATA CONVERSION FUNCTIONS
// =============================================================================

function CreateBitmapFromBytes(const Data: TBytes; Width, Height: Integer): TBitmap;
var
  BitmapInfo: TBitmapInfo;
  DIBBits: Pointer;
begin
  Result := TBitmap.Create;

  if Length(Data) = 0 then
    Exit;

  Result.Width := Width;
  Result.Height := Height;
  Result.PixelFormat := pf32bit;

  ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
  BitmapInfo.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
  BitmapInfo.bmiHeader.biWidth := Width;
  BitmapInfo.bmiHeader.biHeight := -Height; // Top-down DIB
  BitmapInfo.bmiHeader.biPlanes := 1;
  BitmapInfo.bmiHeader.biBitCount := 32;
  BitmapInfo.bmiHeader.biCompression := BI_RGB;

  DIBBits := nil;
  Result.Handle := CreateDIBSection(0, BitmapInfo, DIB_RGB_COLORS, DIBBits, 0, 0);

  if DIBBits <> nil then
    Move(Data[0], DIBBits^, Min(Length(Data), Width * Height * 4));
end;

function BytesToBitmap(const PixelData: TBytes; Width, Height: Integer): TBitmap;
begin
  Result := CreateBitmapFromBytes(PixelData, Width, Height);
end;

// =============================================================================
// STRING AND FORMATTING UTILITIES
// =============================================================================

function CaptureMethodToString(Method: TCaptureMethod): string;
begin
  case Method of
    cmGDI: Result := 'GDI';
    cmDXGI: Result := 'DXGI';
  else
    Result := 'Unknown';
  end;
end;

function CaptureModeToString(Mode: TCaptureMode): string;
begin
  case Mode of
    cmFullFrame: Result := 'Full Frame';
    cmDirtyOnly: Result := 'Dirty Only';
  else
    Result := 'Unknown';
  end;
end;

function FormatBytesSize(Bytes: Int64): string;
const
  Units: array[0..4] of string = ('B', 'KB', 'MB', 'GB', 'TB');
var
  Size: Double;
  UnitIndex: Integer;
begin
  Size := Bytes;
  UnitIndex := 0;

  while (Size >= 1024) and (UnitIndex < High(Units)) do
  begin
    Size := Size / 1024;
    Inc(UnitIndex);
  end;

  if UnitIndex = 0 then
    Result := Format('%d %s', [Round(Size), Units[UnitIndex]])
  else
    Result := Format('%.1f %s', [Size, Units[UnitIndex]]);
end;

// =============================================================================
// ERROR HANDLING UTILITIES
// =============================================================================

function GetLastWindowsError: string;
var
  ErrorCode: DWORD;
begin
  ErrorCode := GetLastError;
  Result := SysErrorMessage(ErrorCode);
  if Result = '' then
    Result := Format('Windows Error %d', [ErrorCode]);
end;

// =============================================================================
// DEBUG UTILITIES
// =============================================================================

procedure LogDebugMessage(const Message: string);
begin
  {$IFDEF DEBUG}
  OutputDebugString(PChar('[DesktopCapture] ' + Message));
  {$ENDIF}
end;

function GetSystemInfo: string;
var
  Resolution: TPoint;
begin
  Resolution := GetScreenResolution;
  Result := Format('Windows %d.%d, Screen: %dx%d, DXGI: %s',
    [Win32MajorVersion, Win32MinorVersion, Resolution.X, Resolution.Y,
     BoolToStr(HasDXGISupport, True)]);
end;

end.
