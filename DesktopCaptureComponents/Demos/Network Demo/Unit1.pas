unit Unit1;

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Controls,
  Vcl.Forms, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, ncSources,
  DesktopCapture, DesktopCaptureReceiver, DesktopCaptureTypes, DesktopCaptureUtils;

const
  CMD_FRAME = 1000;

type
  TForm1 = class(TForm)
    ServerGroup: TGroupBox;
    ClientGroup: TGroupBox;
    SettingsGroup: TGroupBox;
    Image1: TImage;

    StartServerBtn: TButton;
    StopServerBtn: TButton;
    PortEdit: TEdit;

    ConnectBtn: TButton;
    DisconnectBtn: TButton;
    IPEdit: TEdit;
    StartCaptureBtn: TButton;
    StopCaptureBtn: TButton;

    MonitorCombo: TComboBox;
    ModeCombo: TComboBox;
    FPSTrack: TTrackBar;

    FPSLabel: TLabel;
    SizeLabel: TLabel;
    DataSizeLabel: TLabel;
    Timer1: TTimer;

    ncServer: TncServerSource;
    ncClient: TncClientSource;
    Capture: TDesktopCapture;
    Receiver: TDesktopCaptureReceiver;
    TheMethodCombo: TComboBox;

    procedure FormCreate(Sender: TObject);
    procedure StartServerBtnClick(Sender: TObject);
    procedure StopServerBtnClick(Sender: TObject);
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure StartCaptureBtnClick(Sender: TObject);
    procedure StopCaptureBtnClick(Sender: TObject);
    procedure CaptureFrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
    procedure ReceiverFrameReceived(Sender: TObject; Width, Height: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure MonitorComboChange(Sender: TObject);
    procedure ModeComboChange(Sender: TObject);
    procedure FPSTrackChange(Sender: TObject);
    procedure ncServerConnected(Sender: TObject; aLine: TncLine);
    procedure ncServerDisconnected(Sender: TObject; aLine: TncLine);
    function ncServerHandleCommand(Sender: TObject; aLine: TncLine; aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean; const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure ncClientConnected(Sender: TObject; aLine: TncLine);
    procedure ncClientDisconnected(Sender: TObject; aLine: TncLine);

  private
    FConnected: Boolean;
    FFrameCount: Integer;
    FStartTime: Cardinal;
    FCurrentDataSize: Integer;
    procedure UpdateMonitors;
    procedure UpdateUI;
    function FormatBytes(Bytes: Integer): string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.FormatBytes(Bytes: Integer): string;
begin
  if Bytes >= 1024 * 1024 * 1024 then
    Result := Format('%.1f GB', [Bytes / (1024 * 1024 * 1024)])
  else if Bytes >= 1024 * 1024 then
    Result := Format('%.1f MB', [Bytes / (1024 * 1024)])
  else if Bytes >= 1024 then
    Result := Format('%.1f KB', [Bytes / 1024])
  else
    Result := Format('%d B', [Bytes]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FConnected := False;
  FFrameCount := 0;
  FCurrentDataSize := 0;

  PortEdit.Text := '8080';
  IPEdit.Text := '127.0.0.1';

  ModeCombo.Items.Add('Full Frame');
  ModeCombo.Items.Add('Dirty Only');
  ModeCombo.ItemIndex := 1;

  TheMethodCombo.Items.Add('GDI');
  TheMethodCombo.Items.Add('DXGI');
  TheMethodCombo.ItemIndex := 0;

  FPSTrack.Min := 5;
  FPSTrack.Max := 60;
  FPSTrack.Position := 30;

  Receiver.TargetImage := Image1;

  UpdateMonitors;
  UpdateUI;

  Timer1.Interval := 1000;
  Timer1.Enabled := True;
end;

procedure TForm1.UpdateMonitors;
var
  i: Integer;
  Info: TMonitorInfo;
begin
  MonitorCombo.Items.Clear;
  MonitorCombo.Items.Add('Primary Monitor');

  for i := 0 to Capture.GetMonitorCount - 1 do
  begin
    Info := Capture.GetMonitorInfo(i);
    MonitorCombo.Items.Add(Format('Monitor %d (%dx%d)', [i, Info.Right - Info.Left, Info.Bottom - Info.Top]));
  end;

  MonitorCombo.ItemIndex := 0;
end;

procedure TForm1.StartServerBtnClick(Sender: TObject);
begin
  ncServer.Port := StrToInt(PortEdit.Text);
  ncServer.Active := True;
  UpdateUI;
end;

procedure TForm1.StopServerBtnClick(Sender: TObject);
begin
  ncServer.Active := False;
  FConnected := False;
  UpdateUI;
end;

procedure TForm1.ConnectBtnClick(Sender: TObject);
begin
  ncClient.Host := IPEdit.Text;
  ncClient.Port := StrToInt(PortEdit.Text);
  ncClient.Active := True;
end;

procedure TForm1.DisconnectBtnClick(Sender: TObject);
begin
  ncClient.Active := False;
  FConnected := False;
  UpdateUI;
end;

procedure TForm1.StartCaptureBtnClick(Sender: TObject);
begin
  if MonitorCombo.ItemIndex = 0 then
    Capture.MonitorSelection := msPrimary
  else
  begin
    Capture.MonitorSelection := msSpecific;
    Capture.MonitorIndex := MonitorCombo.ItemIndex - 1;
  end;

  if TheMethodCombo.ItemIndex = 0 then
    Capture.method := cmGDI
  else
    Capture.method := cmDXGI;

  if ModeCombo.ItemIndex = 0 then
    Capture.Mode := cmFullFrame
  else
    Capture.Mode := cmDirtyOnly;

  Capture.TargetFPS := FPSTrack.Position;
  Capture.Active := True;
  FStartTime := GetTickCount;
  FFrameCount := 0;
  UpdateUI;
end;

procedure TForm1.StopCaptureBtnClick(Sender: TObject);
begin
  Capture.Active := False;
  UpdateUI;
end;

procedure TForm1.CaptureFrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
begin
  if FConnected then
  begin
    ncClient.ExecCommand(CMD_FRAME, FrameData);
    Inc(FFrameCount);
  end;
end;

procedure TForm1.ReceiverFrameReceived(Sender: TObject; Width, Height: Integer);
begin
  SizeLabel.Caption := Format('Size: %dx%d', [Width, Height]);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var
  Elapsed: Double;
begin
  if (FStartTime > 0) and (FFrameCount > 0) then
  begin
    Elapsed := (GetTickCount - FStartTime) / 1000;
    FPSLabel.Caption := Format('FPS: %.1f', [FFrameCount / Elapsed]);
  end;
end;

procedure TForm1.MonitorComboChange(Sender: TObject);
begin
  // Monitor selection will be applied when capture starts
end;

procedure TForm1.ModeComboChange(Sender: TObject);
begin
  // Mode selection will be applied when capture starts
end;

procedure TForm1.FPSTrackChange(Sender: TObject);
begin
  if Capture.Active then
    Capture.TargetFPS := FPSTrack.Position;
end;

procedure TForm1.ncServerConnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  UpdateUI;
end;

procedure TForm1.ncServerDisconnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := False;
  UpdateUI;
end;

function TForm1.ncServerHandleCommand(Sender: TObject; aLine: TncLine; aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean; const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  if aCmd = CMD_FRAME then
  begin
    // EXACTLY like your working demo
    Receiver.ReceiveFrameData(aData);

    // Force image refresh - this is what your demo does
    if Assigned(Image1) then
      Image1.Invalidate;

    FCurrentDataSize := Length(aData);
    DataSizeLabel.Caption := 'Data: ' + FormatBytes(FCurrentDataSize);
  end;
  SetLength(Result, 0);
end;

procedure TForm1.ncClientConnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  UpdateUI;
end;

procedure TForm1.ncClientDisconnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := False;
  UpdateUI;
end;

procedure TForm1.UpdateUI;
begin
  StartServerBtn.Enabled := not ncServer.Active;
  StopServerBtn.Enabled := ncServer.Active;
  ConnectBtn.Enabled := not FConnected;
  DisconnectBtn.Enabled := FConnected;
  StartCaptureBtn.Enabled := FConnected and not Capture.Active;
  StopCaptureBtn.Enabled := FConnected and Capture.Active;
end;

end.
