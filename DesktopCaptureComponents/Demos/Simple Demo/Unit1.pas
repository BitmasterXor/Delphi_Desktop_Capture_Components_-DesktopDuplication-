unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  DesktopCaptureReceiver, DesktopCapture, DesktopCaptureTypes, DesktopCaptureUtils;

type
  TForm1 = class(TForm)
    DesktopCapture1: TDesktopCapture;
    DesktopCaptureReceiver1: TDesktopCaptureReceiver;
    MainPanel: TPanel;
    ControlPanel: TPanel;
    StatusPanel: TPanel;
    Image1: TImage;
    ScrollBox1: TScrollBox;

    // Control buttons
    StartButton: TButton;
    StopButton: TButton;

    // Settings group
    SettingsGroupBox: TGroupBox;
    MethodLabel: TLabel;
    MethodComboBox: TComboBox;
    ModeLabel: TLabel;
    ModeComboBox: TComboBox;
    MonitorLabel: TLabel;
    MonitorComboBox: TComboBox;
    MonitorInfoLabel: TLabel;
    FPSLabel: TLabel;
    FPSTrackBar: TTrackBar;
    FPSValueLabel: TLabel;
    IncludeCursorCheckBox: TCheckBox;

    // Status group (simplified)
    StatusGroupBox: TGroupBox;
    StatusLabel: TLabel;
    FPSDisplayLabel: TLabel;
    ResolutionLabel: TLabel;
    PixelChangesLabel: TLabel;  // NEW: Current pixel changes size

    // Timer for status updates
    StatusTimer: TTimer;
    Splitter1: TSplitter;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure DesktopCapture1FrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
    procedure DesktopCaptureReceiver1FrameReceived(Sender: TObject; Width, Height: Integer);
    procedure MethodComboBoxChange(Sender: TObject);
    procedure ModeComboBoxChange(Sender: TObject);
    procedure MonitorComboBoxChange(Sender: TObject);
    procedure FPSTrackBarChange(Sender: TObject);
    procedure IncludeCursorCheckBoxClick(Sender: TObject);
    procedure StatusTimerTimer(Sender: TObject);
    procedure DesktopCaptureReceiver1BitmapReady(Sender: TObject;
      Bitmap: TBitmap; Width, Height: Integer);

  private
    // Simplified statistics
    FFPSFrameCount: Integer;
    FStartTime: Cardinal;
    FLastStatsUpdate: Cardinal;
    FCurrentFPS: Double;
    FCurrentPixelChangesSize: Integer;  // Current frame data size

    procedure UpdateControls;
    procedure UpdateMonitorList;
    procedure UpdateMonitorAvailability;
    procedure UpdateStatus;
    procedure ResetStatistics;
    function GetSystemInfoText: string;
    function IsDXGISelected: Boolean;

  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize form
  Caption := 'Desktop Capture Demo - Simplified Logging';

  // Setup method combo
  MethodComboBox.Items.Clear;
  MethodComboBox.Items.Add('DXGI (Hardware Accelerated)');
  MethodComboBox.Items.Add('GDI (Software)');
  MethodComboBox.ItemIndex := 0; // Default to DXGI

  // Setup mode combo
  ModeComboBox.Items.Clear;
  ModeComboBox.Items.Add('Dirty Regions Only (Efficient)');
  ModeComboBox.Items.Add('Full Frame (Simple)');
  ModeComboBox.ItemIndex := 0; // Default to dirty regions

  // Setup FPS trackbar
  FPSTrackBar.Min := 5;
  FPSTrackBar.Max := 60;
  FPSTrackBar.Position := 30;
  FPSValueLabel.Caption := '30 FPS';

  // Setup components
  DesktopCapture1.Method := cmDXGI;
  DesktopCapture1.Mode := cmDirtyOnly;
  DesktopCapture1.TargetFPS := 30;
  DesktopCapture1.IncludeCursor := True;
  DesktopCapture1.MonitorSelection := msPrimary;

  DesktopCaptureReceiver1.TargetImage := Image1;

  // Update monitor list
  UpdateMonitorList;
  UpdateMonitorAvailability;

  // Initialize statistics
  ResetStatistics;

  // Update initial state
  UpdateControls;
  UpdateStatus;

  // Start status timer
  StatusTimer.Interval := 500; // Update every 500ms
  StatusTimer.Enabled := True;

  // Show system info in status
  StatusLabel.Caption := 'Ready - ' + GetSystemInfoText;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if DesktopCapture1.Active then
    DesktopCapture1.Active := False;
end;

procedure TForm1.StartButtonClick(Sender: TObject);
begin
  try
    ResetStatistics;
    DesktopCapture1.Active := True;
    FStartTime := GetTickCount;
    UpdateControls;
    StatusLabel.Caption := 'Capturing...';
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start capture: ' + E.Message);
      UpdateControls;
    end;
  end;
end;

procedure TForm1.StopButtonClick(Sender: TObject);
begin
  DesktopCapture1.Active := False;
  UpdateControls;
  StatusLabel.Caption := 'Stopped - ' + GetSystemInfoText;
end;

procedure TForm1.DesktopCapture1FrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
begin
  // Track only what we need: FPS calculation and current frame size
  Inc(FFPSFrameCount);
  FCurrentPixelChangesSize := Length(FrameData);

  // Pass to receiver - this should update the image
  DesktopCaptureReceiver1.ReceiveFrameData(FrameData);

  // Force image refresh if needed
  if Assigned(Image1) then
    Image1.Invalidate;
end;

procedure TForm1.DesktopCaptureReceiver1BitmapReady(Sender: TObject;
  Bitmap: TBitmap; Width, Height: Integer);
begin
self.Image1.Picture.Bitmap.Assign(Bitmap);
end;

procedure TForm1.DesktopCaptureReceiver1FrameReceived(Sender: TObject; Width, Height: Integer);
begin
  // Update resolution display
  ResolutionLabel.Caption := Format('Image Size: %dx%d', [Width, Height]);
end;

function TForm1.IsDXGISelected: Boolean;
begin
  Result := MethodComboBox.ItemIndex = 0;
end;

procedure TForm1.MethodComboBoxChange(Sender: TObject);
begin
  if not DesktopCapture1.Active then
  begin
    case MethodComboBox.ItemIndex of
      0: DesktopCapture1.Method := cmDXGI;
      1: DesktopCapture1.Method := cmGDI;
    end;

    UpdateMonitorAvailability;
  end;
end;

procedure TForm1.ModeComboBoxChange(Sender: TObject);
begin
  case ModeComboBox.ItemIndex of
    0: DesktopCapture1.Mode := cmDirtyOnly;
    1: DesktopCapture1.Mode := cmFullFrame;
  end;

  // Reset statistics when mode changes
  if DesktopCapture1.Active then
    ResetStatistics;
end;

procedure TForm1.MonitorComboBoxChange(Sender: TObject);
begin
  if not DesktopCapture1.Active then
  begin
    // Prevent virtual desktop selection with DXGI
    if (MonitorComboBox.ItemIndex = 0) and IsDXGISelected then
    begin
      ShowMessage('Virtual Desktop is not supported with DXGI. Switching to Primary Monitor.');
      MonitorComboBox.ItemIndex := 1; // Switch to Primary Monitor
    end;

    case MonitorComboBox.ItemIndex of
      0: DesktopCapture1.MonitorSelection := msDefault;
      1: DesktopCapture1.MonitorSelection := msPrimary;
    else
      DesktopCapture1.MonitorSelection := msSpecific;
      DesktopCapture1.MonitorIndex := MonitorComboBox.ItemIndex - 2;
    end;
  end;
end;

procedure TForm1.FPSTrackBarChange(Sender: TObject);
begin
  DesktopCapture1.TargetFPS := FPSTrackBar.Position;
  FPSValueLabel.Caption := Format('%d FPS', [FPSTrackBar.Position]);
end;

procedure TForm1.IncludeCursorCheckBoxClick(Sender: TObject);
begin
  DesktopCapture1.IncludeCursor := IncludeCursorCheckBox.Checked;
end;

procedure TForm1.StatusTimerTimer(Sender: TObject);
begin
  if DesktopCapture1.Active then
    UpdateStatus;
end;

procedure TForm1.UpdateControls;
begin
  StartButton.Enabled := not DesktopCapture1.Active;
  StopButton.Enabled := DesktopCapture1.Active;

  // Disable some controls during capture
  MethodComboBox.Enabled := not DesktopCapture1.Active;
  MonitorComboBox.Enabled := not DesktopCapture1.Active;

  // These can be changed during capture
  ModeComboBox.Enabled := True;
  FPSTrackBar.Enabled := True;
  IncludeCursorCheckBox.Enabled := True;

  // Update checkbox state
  IncludeCursorCheckBox.Checked := DesktopCapture1.IncludeCursor;
end;

procedure TForm1.UpdateMonitorList;
var
  i, MonitorCount: Integer;
  MonitorInfo: TMonitorInfo;
begin
  MonitorComboBox.Items.Clear;
  MonitorComboBox.Items.Add('Virtual Desktop (All Monitors)');
  MonitorComboBox.Items.Add('Primary Monitor');

  MonitorCount := DesktopCapture1.GetMonitorCount;
  for i := 0 to MonitorCount - 1 do
  begin
    MonitorInfo := DesktopCapture1.GetMonitorInfo(i);
    if MonitorInfo.IsPrimary then
      MonitorComboBox.Items.Add(Format('Monitor %d (%dx%d - Primary)', [
        i,
        MonitorInfo.Right - MonitorInfo.Left,
        MonitorInfo.Bottom - MonitorInfo.Top
      ]))
    else
      MonitorComboBox.Items.Add(Format('Monitor %d (%dx%d)', [
        i,
        MonitorInfo.Right - MonitorInfo.Left,
        MonitorInfo.Bottom - MonitorInfo.Top
      ]));
  end;

  MonitorComboBox.ItemIndex := 1; // Default to Primary Monitor
end;

procedure TForm1.UpdateMonitorAvailability;
begin
  if IsDXGISelected then
  begin
    // DXGI selected - disable virtual desktop option
    MonitorInfoLabel.Caption := 'Note: Virtual Desktop not supported with DXGI';
    MonitorInfoLabel.Font.Color := clMaroon;
    MonitorInfoLabel.Visible := True;

    // If virtual desktop is currently selected, switch to primary monitor
    if MonitorComboBox.ItemIndex = 0 then
    begin
      MonitorComboBox.ItemIndex := 1; // Switch to Primary Monitor
      DesktopCapture1.MonitorSelection := msPrimary;
    end;
  end
  else
  begin
    // GDI selected - all options available
    MonitorInfoLabel.Caption := 'All monitor options available with GDI';
    MonitorInfoLabel.Font.Color := clGreen;
    MonitorInfoLabel.Visible := True;
  end;
end;

procedure TForm1.UpdateStatus;
var
  CurrentTime: Cardinal;
  ElapsedSeconds: Double;
  MethodText: string;
begin
  CurrentTime := GetTickCount;

  // Calculate FPS (only essential logging)
  if CurrentTime - FLastStatsUpdate >= 1000 then // Update every second
  begin
    ElapsedSeconds := (CurrentTime - FStartTime) / 1000.0;
    if ElapsedSeconds > 0 then
    begin
      FCurrentFPS := FFPSFrameCount / ElapsedSeconds;
      FPSDisplayLabel.Caption := Format('Actual FPS: %.1f', [FCurrentFPS]);
    end;
    FLastStatsUpdate := CurrentTime;
  end;

  // Update current pixel changes size
  if FCurrentPixelChangesSize > 0 then
  begin
    if FCurrentPixelChangesSize >= 1024 * 1024 then
      PixelChangesLabel.Caption := Format('Pixel Changes: %.2f MB', [FCurrentPixelChangesSize / (1024 * 1024)])
    else if FCurrentPixelChangesSize >= 1024 then
      PixelChangesLabel.Caption := Format('Pixel Changes: %.1f KB', [FCurrentPixelChangesSize / 1024])
    else
      PixelChangesLabel.Caption := Format('Pixel Changes: %d bytes', [FCurrentPixelChangesSize]);
  end
  else
    PixelChangesLabel.Caption := 'Pixel Changes: 0 bytes';

  // Update method status
  case DesktopCapture1.Method of
    cmDXGI: MethodText := 'DXGI (Hardware)';
    cmGDI: MethodText := 'GDI (Software)';
  end;

  case DesktopCapture1.Mode of
    cmDirtyOnly: MethodText := MethodText + ' - Dirty Regions';
    cmFullFrame: MethodText := MethodText + ' - Full Frame';
  end;

  case DesktopCapture1.MonitorSelection of
    msDefault: MethodText := MethodText + ' - Virtual Desktop';
    msPrimary: MethodText := MethodText + ' - Primary Monitor';
    msSpecific: MethodText := MethodText + Format(' - Monitor %d', [DesktopCapture1.MonitorIndex]);
  end;

  StatusLabel.Caption := 'Capturing with ' + MethodText;
end;

procedure TForm1.ResetStatistics;
begin
  FFPSFrameCount := 0;
  FCurrentFPS := 0;
  FCurrentPixelChangesSize := 0;
  FLastStatsUpdate := GetTickCount;

  FPSDisplayLabel.Caption := 'Actual FPS: 0.0';
  ResolutionLabel.Caption := 'Image Size: Not Available';
  PixelChangesLabel.Caption := 'Pixel Changes: 0 bytes';
end;

function TForm1.GetSystemInfoText: string;
begin
  if HasDXGISupport then
    Result := 'DXGI Supported'
  else
    Result := 'DXGI Not Available';

  Result := Result + Format(' - %d Monitors', [DesktopCapture1.GetMonitorCount]);
end;

end.
