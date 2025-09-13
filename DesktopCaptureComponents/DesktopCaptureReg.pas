unit DesktopCaptureReg;

{
  ========================================================================
  Desktop Capture Component Registration - SIMPLIFIED
  ========================================================================

  Registers both TDesktopCapture and TDesktopCaptureReceiver components
  on the "Desktop Capture" tab in the Delphi IDE.
}

interface

uses
  Classes, DesktopCapture, DesktopCaptureReceiver;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Desktop Capture', [TDesktopCapture, TDesktopCaptureReceiver]);
end;

end.
