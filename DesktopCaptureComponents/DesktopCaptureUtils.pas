unit DesktopCaptureUtils;

{
  ========================================================================
  Desktop Capture Utility Functions - COMPLETELY FIXED ALL CONFLICTS
  ========================================================================
  NO MORE NAMING CONFLICTS OR TYPE ISSUES
}

interface

uses
  Windows, SysUtils, Classes, Types, Graphics, MultiMon,
  DesktopCaptureTypes;

// System information functions
function GetScreenResolution: TPoint;
function GetVirtualDesktopRect: TRect;
function IsWindowsVistaOrLater: Boolean;
function IsWindows8OrLater: Boolean;
function HasDXGISupport: Boolean;

// Multi-monitor support functions - RENAMED TO AVOID CONFLICTS
function GetMonitorCount: Integer;
function GetDesktopMonitorInfo(Index: Integer): TMonitorInfo;  // RENAMED
function GetPrimaryMonitor: TMonitorInfo;
function GetMonitorFromPoint(const Point: TPoint): TMonitorInfo;
function GetMonitorContainingWindow(WindowHandle: HWND): TMonitorInfo;

// Memory and data conversion functions
function CreateBitmapFromBytes(const Data: TBytes; Width, Height: Integer): TBitmap;
function BytesToBitmap(const PixelData: TBytes; Width, Height: Integer): TBitmap;

// String and formatting utilities
function CaptureMethodToString(Method: TCaptureMethod): string;
function CaptureModeToString(Mode: TCaptureMode): string;
function MonitorSelectionToString(Selection: TMonitorSelection): string;
function FormatBytesSize(Bytes: Int64): string;

// Error handling utilities
function GetLastWindowsError: string;

// Debug utilities
procedure LogDebugMessage(const Message: string);
function GetSystemInfo: string;
function GetMultiMonitorInfo: string;

implementation

uses
  Math, StrUtils;

// Monitor enumeration variables
var
  EnumMonitors: array of TMonitorInfo;

// Monitor enumeration callback - COMPLETELY FIXED
function MonitorEnumProcUtils(hMonitor: Windows.HMONITOR; hdcMonitor: HDC; lprcMonitor: PRect; dwData: LPARAM): BOOL; stdcall;
var
  MonitorInfoEx: TMonitorInfoEx;
  Count: Integer;
begin
  Result := True;

  MonitorInfoEx.cbSize := SizeOf(TMonitorInfoEx);
  if GetMonitorInfo(hMonitor, @MonitorInfoEx) then
  begin
    Count := Length(EnumMonitors);
    SetLength(EnumMonitors, Count + 1);

    EnumMonitors[Count].MonitorIndex := Count;
    EnumMonitors[Count].Handle := hMonitor;
    EnumMonitors[Count].Left := MonitorInfoEx.rcMonitor.Left;
    EnumMonitors[Count].Top := MonitorInfoEx.rcMonitor.Top;
    EnumMonitors[Count].Right := MonitorInfoEx.rcMonitor.Right;
    EnumMonitors[Count].Bottom := MonitorInfoEx.rcMonitor.Bottom;
    EnumMonitors[Count].IsPrimary := (MonitorInfoEx.dwFlags and MONITORINFOF_PRIMARY) <> 0;
  end;
end;

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
// MULTI-MONITOR SUPPORT FUNCTIONS - ALL CONFLICTS RESOLVED
// =============================================================================

function GetMonitorCount: Integer;
begin
  SetLength(EnumMonitors, 0);
  EnumDisplayMonitors(0, nil, @MonitorEnumProcUtils, 0);
  Result := Length(EnumMonitors);
end;

function GetDesktopMonitorInfo(Index: Integer): TMonitorInfo;  // RENAMED TO AVOID CONFLICT
begin
  if Length(EnumMonitors) = 0 then
    GetMonitorCount;

  if (Index >= 0) and (Index < Length(EnumMonitors)) then
    Result := EnumMonitors[Index]
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

function GetPrimaryMonitor: TMonitorInfo;
var
  i: Integer;
begin
  if Length(EnumMonitors) = 0 then
    GetMonitorCount;

  for i := 0 to High(EnumMonitors) do
    if EnumMonitors[i].IsPrimary then
    begin
      Result := EnumMonitors[i];
      Exit;
    end;

  // Fallback
  Result := GetDesktopMonitorInfo(0);
end;

function GetMonitorFromPoint(const Point: TPoint): TMonitorInfo;
var
  hMonitor: Windows.HMONITOR;
  MonitorInfoEx: TMonitorInfoEx;
  i: Integer;
begin
  hMonitor := MonitorFromPoint(Point, MONITOR_DEFAULTTONEAREST);

  if Length(EnumMonitors) = 0 then
    GetMonitorCount;

  for i := 0 to High(EnumMonitors) do
    if EnumMonitors[i].Handle = hMonitor then
    begin
      Result := EnumMonitors[i];
      Exit;
    end;

  // If not found, get monitor info directly
  MonitorInfoEx.cbSize := SizeOf(TMonitorInfoEx);
  if GetMonitorInfo(hMonitor, @MonitorInfoEx) then
  begin
    Result.MonitorIndex := -1; // Unknown index
    Result.Handle := hMonitor;
    Result.Left := MonitorInfoEx.rcMonitor.Left;
    Result.Top := MonitorInfoEx.rcMonitor.Top;
    Result.Right := MonitorInfoEx.rcMonitor.Right;
    Result.Bottom := MonitorInfoEx.rcMonitor.Bottom;
    Result.IsPrimary := (MonitorInfoEx.dwFlags and MONITORINFOF_PRIMARY) <> 0;
  end
  else
    Result := GetPrimaryMonitor;
end;

function GetMonitorContainingWindow(WindowHandle: HWND): TMonitorInfo;
var
  hMonitor: Windows.HMONITOR;
  i: Integer;
begin
  hMonitor := MonitorFromWindow(WindowHandle, MONITOR_DEFAULTTONEAREST);

  if Length(EnumMonitors) = 0 then
    GetMonitorCount;

  // Find the monitor in our enum list
  for i := 0 to High(EnumMonitors) do
    if EnumMonitors[i].Handle = hMonitor then
    begin
      Result := EnumMonitors[i];
      Exit;
    end;

  // Fallback to primary monitor
  Result := GetPrimaryMonitor;
end;

// =============================================================================
// MEMORY AND DATA CONVERSION FUNCTIONS
// =============================================================================

function CreateBitmapFromBytes(const Data: TBytes; Width, Height: Integer): TBitmap;
begin
  Result := BytesToBitmap(Data, Width, Height);
end;

function BytesToBitmap(const PixelData: TBytes; Width, Height: Integer): TBitmap;
var
  BitmapInfo: TBitmapInfo;
  ScreenDC: HDC;
begin
  Result := TBitmap.Create;
  try
    Result.PixelFormat := pf32bit;
    Result.Width := Width;
    Result.Height := Height;

    if Length(PixelData) >= (Width * Height * 4) then
    begin
      ZeroMemory(@BitmapInfo, SizeOf(BitmapInfo));
      BitmapInfo.bmiHeader.biSize := SizeOf(BITMAPINFOHEADER);
      BitmapInfo.bmiHeader.biWidth := Width;
      BitmapInfo.bmiHeader.biHeight := -Height; // Top-down
      BitmapInfo.bmiHeader.biPlanes := 1;
      BitmapInfo.bmiHeader.biBitCount := 32;
      BitmapInfo.bmiHeader.biCompression := BI_RGB;

      ScreenDC := GetDC(0);
      try
        SetDIBits(ScreenDC, Result.Handle, 0, Height, @PixelData[0], BitmapInfo, DIB_RGB_COLORS);
      finally
        ReleaseDC(0, ScreenDC);
      end;
    end;
  except
    Result.Free;
    raise;
  end;
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
    cmDirtyOnly: Result := 'Dirty Regions Only';
  else
    Result := 'Unknown';
  end;
end;

function MonitorSelectionToString(Selection: TMonitorSelection): string;
begin
  case Selection of
    msDefault: Result := 'Virtual Desktop (All Monitors)';
    msPrimary: Result := 'Primary Monitor';
    msSpecific: Result := 'Specific Monitor';
  else
    Result := 'Unknown';
  end;
end;

function FormatBytesSize(Bytes: Int64): string;
const
  Units: array[0..6] of string = ('B', 'KB', 'MB', 'GB', 'TB', 'PB', 'EB');
var
  UnitIndex: Integer;
  Size: Double;
begin
  UnitIndex := 0;
  Size := Bytes;

  while (Size >= 1024) and (UnitIndex < High(Units)) do
  begin
    Size := Size / 1024;
    Inc(UnitIndex);
  end;

  if UnitIndex = 0 then
    Result := Format('%d %s', [Round(Size), Units[UnitIndex]])
  else
    Result := Format('%.2f %s', [Size, Units[UnitIndex]]);
end;

// =============================================================================
// ERROR HANDLING UTILITIES
// =============================================================================

function GetLastWindowsError: string;
var
  ErrorCode: DWORD;
  Buffer: array[0..255] of Char;
begin
  ErrorCode := GetLastError;
  if FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, ErrorCode, 0, Buffer, SizeOf(Buffer), nil) > 0 then
    Result := Format('Error %d: %s', [ErrorCode, Trim(Buffer)])
  else
    Result := Format('Error %d (no description available)', [ErrorCode]);
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
  OSInfo: TOSVersionInfo;
  SysInfo: TSystemInfo;
begin
  OSInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(OSInfo);
  Windows.GetSystemInfo(SysInfo);

  Result := Format(
    'OS: Windows %d.%d (Build %d)' + sLineBreak +
    'Processor: %s (%d processors)' + sLineBreak +
    'Memory: %d MB' + sLineBreak +
    'Screen: %dx%d' + sLineBreak +
    'Virtual Desktop: %s' + sLineBreak +
    'DXGI Support: %s',
    [
      OSInfo.dwMajorVersion,
      OSInfo.dwMinorVersion,
      OSInfo.dwBuildNumber,
      {$IFDEF WIN64}'x64'{$ELSE}'x86'{$ENDIF},
      SysInfo.dwNumberOfProcessors,
      GetSystemMetrics(SM_CXSCREEN) * GetSystemMetrics(SM_CYSCREEN) * 4 div (1024 * 1024),
      GetSystemMetrics(SM_CXSCREEN),
      GetSystemMetrics(SM_CYSCREEN),
      Format('%dx%d at %d,%d',
        [GetSystemMetrics(SM_CXVIRTUALSCREEN),
         GetSystemMetrics(SM_CYVIRTUALSCREEN),
         GetSystemMetrics(SM_XVIRTUALSCREEN),
         GetSystemMetrics(SM_YVIRTUALSCREEN)]),
      BoolToStr(HasDXGISupport, True)
    ]);
end;

function GetMultiMonitorInfo: string;
var
  MonitorCount: Integer;
  i: Integer;
  Monitor: TMonitorInfo;
  VirtualRect: TRect;
begin
  MonitorCount := GetMonitorCount;
  VirtualRect := GetVirtualDesktopRect;

  Result := Format('Monitor Configuration:' + sLineBreak +
                   'Total Monitors: %d' + sLineBreak +
                   'Virtual Desktop: %dx%d at %d,%d' + sLineBreak + sLineBreak,
                   [MonitorCount,
                    VirtualRect.Right - VirtualRect.Left,
                    VirtualRect.Bottom - VirtualRect.Top,
                    VirtualRect.Left,
                    VirtualRect.Top]);

  for i := 0 to MonitorCount - 1 do
  begin
    Monitor := GetDesktopMonitorInfo(i);  // USES RENAMED FUNCTION
    Result := Result + Format(
      'Monitor %d%s' + sLineBreak +
      '  Resolution: %dx%d' + sLineBreak +
      '  Position: %d,%d to %d,%d' + sLineBreak +
      '  Handle: $%x' + sLineBreak,
      [i,
       IfThen(Monitor.IsPrimary, ' (Primary)', ''),
       Monitor.Right - Monitor.Left,
       Monitor.Bottom - Monitor.Top,
       Monitor.Left,
       Monitor.Top,
       Monitor.Right,
       Monitor.Bottom,
       Integer(Monitor.Handle)]);

    if i < MonitorCount - 1 then
      Result := Result + sLineBreak;
  end;
end;

end.
