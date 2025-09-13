unit DesktopCaptureTypes;

{
  ========================================================================
  Desktop Capture Component Types - MULTI-MONITOR FIXED
  ========================================================================
  Simple types for easy desktop streaming over network
}

interface

uses
  Windows, Classes, SysUtils, Types;

type
  // Simple capture method
  TCaptureMethod = (cmGDI, cmDXGI);

  // Simple capture mode
  TCaptureMode = (cmFullFrame, cmDirtyOnly);

  // Monitor selection
  TMonitorSelection = (msDefault, msPrimary, msSpecific);

  // Simple events
  TOnFrameCaptured = procedure(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean) of object;
  TOnReceiverUpdate = procedure(Sender: TObject; Width, Height: Integer) of object;

  // Frame header for TBytes data
  TFrameHeader = packed record
    IsFullFrame: Boolean;
    Width: Integer;
    Height: Integer;
    DataSize: Integer;
    DirtyRegionCount: Integer;
  end;

  // Dirty region info
  TDirtyRegion = packed record
    Left, Top, Right, Bottom: Integer;
  end;

  // Monitor info for multi-monitor support
  TMonitorInfo = record
    MonitorIndex: Integer;
    Handle: HMONITOR;
    Left, Top, Right, Bottom: Integer;
    IsPrimary: Boolean;
  end;

implementation

end.
