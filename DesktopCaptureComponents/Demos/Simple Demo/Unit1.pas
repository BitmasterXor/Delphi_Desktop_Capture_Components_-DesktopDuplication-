unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, DesktopCaptureReceiver,
  DesktopCapture, DesktopCaptureTypes, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    DesktopCapture1: TDesktopCapture;
    DesktopCaptureReceiver1: TDesktopCaptureReceiver;
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    StartBtn: TButton;
    StopBtn: TButton;
    GroupBox1: TGroupBox;
    RadioGDI: TRadioButton;
    RadioDXGI: TRadioButton;
    GroupBox2: TGroupBox;
    RadioFullFrame: TRadioButton;
    RadioDirtyOnly: TRadioButton;
    StatusBar1: TStatusBar;
    IncludeCursorChk: TCheckBox;
    Label1: TLabel;
    FPSTrack: TTrackBar;
    FPSLabel: TLabel;

    procedure DesktopCaptureReceiver1FrameReceived(Sender: TObject; Width, Height: Integer);
    procedure DesktopCapture1FrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure StartBtnClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure FPSTrackChange(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FFrameCount: Integer;
    FLastUpdate: Cardinal;
    FFullFrameCount: Integer;
    FDirtyFrameCount: Integer;
    function FormatBytesSize(Bytes: Int64): string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.FormatBytesSize(Bytes: Int64): string;
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

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Setup UI defaults
  RadioDXGI.Checked := True;
  RadioDirtyOnly.Checked := True;
  IncludeCursorChk.Checked := True;
  FPSTrack.Position := 30;
  FPSLabel.Caption := 'FPS: 30';

  // Setup receiver to display captured frames
  DesktopCaptureReceiver1.TargetImage := Image1;
  DesktopCaptureReceiver1.Active := True;

  // Setup image properties
  Image1.Stretch := True;
  Image1.Proportional := True;
  Image1.Center := True;

  // Initialize counters
  FFrameCount := 0;
  FFullFrameCount := 0;
  FDirtyFrameCount := 0;
  FLastUpdate := GetTickCount;

  // Initial button states
  StopBtn.Enabled := False;
  StatusBar1.SimpleText := 'Ready - Click Start to begin desktop capture demo';
end;

procedure TForm1.StartBtnClick(Sender: TObject);
begin
  // Configure capture method
  if RadioGDI.Checked then
    DesktopCapture1.Method := cmGDI
  else
    DesktopCapture1.Method := cmDXGI;

  // Configure capture mode
  if RadioFullFrame.Checked then
    DesktopCapture1.Mode := cmFullFrame
  else
    DesktopCapture1.Mode := cmDirtyOnly;

  // Configure other settings
  DesktopCapture1.TargetFPS := FPSTrack.Position;
  DesktopCapture1.IncludeCursor := IncludeCursorChk.Checked;

  // Reset counters
  FFrameCount := 0;
  FFullFrameCount := 0;
  FDirtyFrameCount := 0;
  FLastUpdate := GetTickCount;

  // START CAPTURE - This is all you need!
  DesktopCapture1.Active := True;

  // Update UI
  StartBtn.Enabled := False;
  StopBtn.Enabled := True;
  RadioGDI.Enabled := False;
  RadioDXGI.Enabled := False;
  StatusBar1.SimpleText := 'Capturing desktop... Watch the magic happen!';
end;

procedure TForm1.StopBtnClick(Sender: TObject);
begin
  // STOP CAPTURE
  DesktopCapture1.Active := False;

  // Update UI
  StartBtn.Enabled := True;
  StopBtn.Enabled := False;
  RadioGDI.Enabled := True;
  RadioDXGI.Enabled := True;
  StatusBar1.SimpleText := 'Capture stopped';
end;

procedure TForm1.DesktopCapture1FrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
var
  FrameTypeStr: string;
begin
  // THIS IS THE MAGIC - Direct pass-through from capture to receiver!
  // In a real network app, this is where you would:
  // Socket.Send(FrameData);

  // For this demo, we directly feed it to the receiver:
  DesktopCaptureReceiver1.ReceiveFrameData(FrameData);

  // Update statistics
  Inc(FFrameCount);
  if IsFullFrame then
    Inc(FFullFrameCount)
  else
    Inc(FDirtyFrameCount);

  // Update status every second
  if GetTickCount - FLastUpdate > 1000 then
  begin
    if IsFullFrame then
      FrameTypeStr := 'FULL'
    else
      FrameTypeStr := 'DIRTY';

    StatusBar1.SimpleText := Format('Frames: %d | Full: %d | Dirty: %d | Size: %s | Type: %s',
      [FFrameCount, FFullFrameCount, FDirtyFrameCount,
       FormatBytesSize(Length(FrameData)),
       FrameTypeStr]);
    FLastUpdate := GetTickCount;
  end;
end;

procedure TForm1.DesktopCaptureReceiver1FrameReceived(Sender: TObject; Width, Height: Integer);
begin
  // This fires when the receiver successfully updates the image
  // You can add any post-processing here if needed

  // For example, update window title with current resolution
  if DesktopCapture1.Active then
    Caption := Format('Desktop Capture Demo - %dx%d', [Width, Height]);
end;

procedure TForm1.FPSTrackChange(Sender: TObject);
begin
  FPSLabel.Caption := Format('FPS: %d', [FPSTrack.Position]);

  // Update FPS if currently capturing
  if DesktopCapture1.Active then
    DesktopCapture1.TargetFPS := FPSTrack.Position;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  // Keep the image properly sized
  if Assigned(Image1) then
    Image1.Repaint;
end;

end.
