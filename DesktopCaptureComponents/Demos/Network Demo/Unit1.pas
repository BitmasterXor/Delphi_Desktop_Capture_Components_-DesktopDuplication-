unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ncSources, Vcl.ExtCtrls,
  Vcl.ComCtrls, Vcl.StdCtrls, DesktopCapture, DesktopCaptureReceiver, DesktopCaptureTypes;

const
  CMD_DESKTOP_FRAME = 1000; // Command ID for desktop frame data

type
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    StatusBar1: TStatusBar;
    ServerRecievingImage: TImage;
    ncServerSource1: TncServerSource;
    ncClientSource1: TncClientSource;

    // Server (Receiver) Controls
    Panel1: TPanel;
    StartServerBtn: TButton;
    StopServerBtn: TButton;
    ServerPortEdit: TEdit;
    Label1: TLabel;
    ServerStatusLabel: TLabel;
    ClearServerBtn: TButton;

    // Client (Sender) Controls
    Panel2: TPanel;
    ConnectBtn: TButton;
    DisconnectBtn: TButton;
    ServerIPEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ClientPortEdit: TEdit;
    ClientStatusLabel: TLabel;
    StartCaptureBtn: TButton;
    StopCaptureBtn: TButton;

    // Desktop Capture Components
    DesktopCapture1: TDesktopCapture;
    DesktopCaptureReceiver1: TDesktopCaptureReceiver;

    // IMPROVED Client Settings
    GroupBox3: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    CaptureMethodCombo: TComboBox;
    CaptureModeCombo: TComboBox;
    FPSTrack: TTrackBar;
    FPSLabel: TLabel;
    IncludeCursorChk: TCheckBox;
    OptimizeForNetworkChk: TCheckBox;

    // Statistics
    GroupBox4: TGroupBox;
    StatsLabel: TLabel;
    Timer1: TTimer;

    procedure ncClientSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;
    procedure ncServerSource1Connected(Sender: TObject; aLine: TncLine);
    procedure ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
    function ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
      aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
      const aSenderComponent, aReceiverComponent: string): TBytes;

    // Form events
    procedure FormCreate(Sender: TObject);

    // Server events
    procedure StartServerBtnClick(Sender: TObject);
    procedure StopServerBtnClick(Sender: TObject);
    procedure ClearServerBtnClick(Sender: TObject);

    // Client events
    procedure ConnectBtnClick(Sender: TObject);
    procedure DisconnectBtnClick(Sender: TObject);
    procedure StartCaptureBtnClick(Sender: TObject);
    procedure StopCaptureBtnClick(Sender: TObject);

    // Desktop Capture events
    procedure DesktopCapture1FrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
    procedure DesktopCaptureReceiver1FrameReceived(Sender: TObject; Width, Height: Integer);

    // UI events
    procedure FPSTrackChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CaptureMethodComboChange(Sender: TObject);
    procedure CaptureModeComboChange(Sender: TObject);
    procedure OptimizeForNetworkChkClick(Sender: TObject);

  private
    FConnected: Boolean;
    FServerRunning: Boolean;
    FFramesSent: Integer;
    FFramesReceived: Integer;
    FBytesSent: Int64;
    FBytesReceived: Int64;
    FFullFramesSent: Integer;
    FDirtyFramesSent: Integer;
    FStartTime: TDateTime;

    function FormatBytesSize(Bytes: Int64): string;
    procedure UpdateStats;
    procedure UpdateUI;
    procedure ApplyOptimizations;
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
  // Initialize state
  FConnected := False;
  FServerRunning := False;
  FFramesSent := 0;
  FFramesReceived := 0;
  FBytesSent := 0;
  FBytesReceived := 0;
  FFullFramesSent := 0;
  FDirtyFramesSent := 0;

  // Setup default values
  ServerPortEdit.Text := '8080';
  ServerIPEdit.Text := '127.0.0.1';
  ClientPortEdit.Text := '8080';

  // Setup capture method combo
  CaptureMethodCombo.Items.Clear;
  CaptureMethodCombo.Items.Add('GDI (Compatible)');
  CaptureMethodCombo.Items.Add('DXGI (Fast)');
  CaptureMethodCombo.ItemIndex := 1; // Default to DXGI

  // Setup capture mode combo
  CaptureModeCombo.Items.Clear;
  CaptureModeCombo.Items.Add('Full Frame (Always complete image)');
  CaptureModeCombo.Items.Add('Dirty Only (Changed pixels only)');
  CaptureModeCombo.ItemIndex := 1; // Default to Dirty Only

  // Setup other defaults
  FPSTrack.Position := 30;
  FPSLabel.Caption := 'FPS: 30';
  IncludeCursorChk.Checked := True;
  OptimizeForNetworkChk.Checked := True; // Enable network optimizations

  // Setup receiver
  DesktopCaptureReceiver1.TargetImage := ServerRecievingImage;
  DesktopCaptureReceiver1.Active := True;

  // Setup image
  ServerRecievingImage.Stretch := True;
  ServerRecievingImage.Proportional := True;
  ServerRecievingImage.Center := True;

  // Apply initial optimizations
  ApplyOptimizations;

  // Initialize UI
  UpdateUI;

  StatusBar1.SimpleText := 'Ready - Start server first, then connect client and begin capture';

  // Start stats timer
  Timer1.Interval := 1000;
  Timer1.Enabled := True;
end;

// =============================================================================
// UI OPTIMIZATION HELPERS
// =============================================================================

procedure TForm1.ApplyOptimizations;
begin
  if OptimizeForNetworkChk.Checked then
  begin
    // Network-optimized settings
    if CaptureModeCombo.ItemIndex <> 1 then
    begin
      CaptureModeCombo.ItemIndex := 1; // Force Dirty Only for network
      StatusBar1.SimpleText := 'Network optimization: Using Dirty Only mode for efficiency';
    end;

    // Lower FPS for network streaming
    if FPSTrack.Position > 25 then
    begin
      FPSTrack.Position := 25;
      FPSLabel.Caption := 'FPS: 25';
    end;
  end;
end;

procedure TForm1.CaptureMethodComboChange(Sender: TObject);
begin
  // Show info about selected method
  case CaptureMethodCombo.ItemIndex of
    0: StatusBar1.SimpleText := 'GDI method: Compatible with all systems, moderate performance';
    1: StatusBar1.SimpleText := 'DXGI method: High performance, requires Windows 8+ for best results';
  end;
end;

procedure TForm1.CaptureModeComboChange(Sender: TObject);
begin
  // Show info about selected mode
  case CaptureModeCombo.ItemIndex of
    0: StatusBar1.SimpleText := 'Full Frame mode: Always sends complete image (higher bandwidth)';
    1: StatusBar1.SimpleText := 'Dirty Only mode: Sends only changed pixels (lower bandwidth)';
  end;
end;

procedure TForm1.OptimizeForNetworkChkClick(Sender: TObject);
begin
  ApplyOptimizations;
end;

// =============================================================================
// SERVER EVENTS (RECEIVER SIDE)
// =============================================================================

procedure TForm1.StartServerBtnClick(Sender: TObject);
begin
  try
    ncServerSource1.Port := StrToInt(ServerPortEdit.Text);
    ncServerSource1.Active := True;
    FServerRunning := True;
    UpdateUI;
    ServerStatusLabel.Caption := 'Server running on port ' + ServerPortEdit.Text;
    StatusBar1.SimpleText := 'Server started - Waiting for client connection';
  except
    on E: Exception do
    begin
      ShowMessage('Failed to start server: ' + E.Message);
      ServerStatusLabel.Caption := 'Server failed to start';
    end;
  end;
end;

procedure TForm1.StopServerBtnClick(Sender: TObject);
begin
  ncServerSource1.Active := False;
  FServerRunning := False;
  FConnected := False;
  UpdateUI;
  ServerStatusLabel.Caption := 'Server stopped';
  StatusBar1.SimpleText := 'Server stopped';
end;

procedure TForm1.ClearServerBtnClick(Sender: TObject);
begin
  DesktopCaptureReceiver1.ClearDisplay;
  StatusBar1.SimpleText := 'Display cleared';
end;

procedure TForm1.ncServerSource1Connected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  UpdateUI;
  ServerStatusLabel.Caption := 'Client connected from ' + aLine.PeerIP;
  StatusBar1.SimpleText := 'Client connected - Ready to receive frames';
  FStartTime := Now;
end;

procedure TForm1.ncServerSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := False;
  UpdateUI;
  ServerStatusLabel.Caption := 'Client disconnected';
  StatusBar1.SimpleText := 'Client disconnected';
end;

function TForm1.ncServerSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  // Handle incoming desktop frame data
  if aCmd = CMD_DESKTOP_FRAME then
  begin
    // Pass the received frame data directly to the receiver component
    DesktopCaptureReceiver1.ReceiveFrameData(aData);

    // Update statistics
    Inc(FFramesReceived);
    FBytesReceived := FBytesReceived + Length(aData);
  end;

  SetLength(Result, 0); // No response needed
end;

// =============================================================================
// CLIENT EVENTS (SENDER SIDE)
// =============================================================================

procedure TForm1.ConnectBtnClick(Sender: TObject);
begin
  try
    ClientStatusLabel.Caption := 'Connecting...';
    ncClientSource1.Host := ServerIPEdit.Text;
    ncClientSource1.Port := StrToInt(ClientPortEdit.Text);
    ncClientSource1.Active := True;
  except
    on E: Exception do
    begin
      ShowMessage('Failed to connect: ' + E.Message);
      ClientStatusLabel.Caption := 'Connection failed';
    end;
  end;
end;

procedure TForm1.DisconnectBtnClick(Sender: TObject);
begin
  // Stop capture first
  DesktopCapture1.Active := False;

  // Then disconnect
  ncClientSource1.Active := False;
  FConnected := False;
  UpdateUI;
  ClientStatusLabel.Caption := 'Disconnected';
end;

procedure TForm1.StartCaptureBtnClick(Sender: TObject);
begin
  if not FConnected then
  begin
    ShowMessage('Not connected to server!');
    Exit;
  end;

  // Apply network optimizations if enabled
  ApplyOptimizations;

  // Configure capture settings from UI
  case CaptureMethodCombo.ItemIndex of
    0: DesktopCapture1.Method := cmGDI;
    1: DesktopCapture1.Method := cmDXGI;
  end;

  case CaptureModeCombo.ItemIndex of
    0: DesktopCapture1.Mode := cmFullFrame;
    1: DesktopCapture1.Mode := cmDirtyOnly;
  end;

  DesktopCapture1.TargetFPS := FPSTrack.Position;
  DesktopCapture1.IncludeCursor := IncludeCursorChk.Checked;

  // Reset stats
  FFramesSent := 0;
  FBytesSent := 0;
  FFullFramesSent := 0;
  FDirtyFramesSent := 0;
  FStartTime := Now;

  // Start capture
  DesktopCapture1.Active := True;

  UpdateUI;
  ClientStatusLabel.Caption := 'Capturing and streaming...';
  StatusBar1.SimpleText := Format('Desktop capture started - %s mode, %s method',
    [CaptureModeCombo.Text, CaptureMethodCombo.Text]);
end;

procedure TForm1.StopCaptureBtnClick(Sender: TObject);
begin
  DesktopCapture1.Active := False;
  UpdateUI;
  ClientStatusLabel.Caption := 'Capture stopped';
  StatusBar1.SimpleText := 'Desktop capture stopped';
end;

procedure TForm1.ncClientSource1Connected(Sender: TObject; aLine: TncLine);
begin
  FConnected := True;
  UpdateUI;
  ClientStatusLabel.Caption := 'Connected to server';
  StatusBar1.SimpleText := 'Connected - Ready to start desktop capture';
end;

procedure TForm1.ncClientSource1Disconnected(Sender: TObject; aLine: TncLine);
begin
  FConnected := False;
  DesktopCapture1.Active := False; // Stop capture on disconnect
  UpdateUI;
  ClientStatusLabel.Caption := 'Disconnected from server';
  StatusBar1.SimpleText := 'Disconnected from server';
end;

function TForm1.ncClientSource1HandleCommand(Sender: TObject; aLine: TncLine;
  aCmd: Integer; const aData: TBytes; aRequiresResult: Boolean;
  const aSenderComponent, aReceiverComponent: string): TBytes;
begin
  // Client doesn't handle commands in this demo
  SetLength(Result, 0);
end;

// =============================================================================
// DESKTOP CAPTURE EVENTS
// =============================================================================

procedure TForm1.DesktopCapture1FrameCaptured(Sender: TObject; const FrameData: TBytes; IsFullFrame: Boolean);
begin
  // Send captured frame to server via NetCom7
  if FConnected and ncClientSource1.Active then
  begin
    try
      ncClientSource1.ExecCommand(CMD_DESKTOP_FRAME, FrameData);

      // Update statistics
      Inc(FFramesSent);
      FBytesSent := FBytesSent + Length(FrameData);

      if IsFullFrame then
        Inc(FFullFramesSent)
      else
        Inc(FDirtyFramesSent);

    except
      on E: Exception do
      begin
        // Handle send errors (connection lost, etc.)
        StatusBar1.SimpleText := 'Send error: ' + E.Message;
      end;
    end;
  end;
end;

procedure TForm1.DesktopCaptureReceiver1FrameReceived(Sender: TObject; Width, Height: Integer);
begin
  // Frame successfully received and displayed
  // This fires on the server side when a frame is processed (or client side / Server side it all depends on which network endpoint your using the component in the first place)
  // however you need to understand (This event is NOT used to recieve the image data at all its just to fire when or if you need to check
  // if the component has indeed successfully just recieved an image (or image data per say)...
  // The proper way to RECEIVE the image and force the component to deal with it is to Pass the received frame data directly to the receiver component
  //  DesktopCaptureReceiver1.ReceiveFrameData(aData);  << YOU Should do this on whatever socket library your using to stream or send the raw Tbytes with!
  // Super simple to use and understand :)
end;

// =============================================================================
// UI HELPERS
// =============================================================================

procedure TForm1.FPSTrackChange(Sender: TObject);
begin
  FPSLabel.Caption := Format('FPS: %d', [FPSTrack.Position]);

  // Update FPS if currently capturing
  if DesktopCapture1.Active then
    DesktopCapture1.TargetFPS := FPSTrack.Position;
end;

procedure TForm1.UpdateUI;
begin
  // Server controls
  StartServerBtn.Enabled := not FServerRunning;
  StopServerBtn.Enabled := FServerRunning;
  ServerPortEdit.Enabled := not FServerRunning;
  ClearServerBtn.Enabled := FServerRunning;

  // Client controls
  ConnectBtn.Enabled := not FConnected;
  DisconnectBtn.Enabled := FConnected;
  ServerIPEdit.Enabled := not FConnected;
  ClientPortEdit.Enabled := not FConnected;

  StartCaptureBtn.Enabled := FConnected and not DesktopCapture1.Active;
  StopCaptureBtn.Enabled := FConnected and DesktopCapture1.Active;

  // Capture settings - disabled when capturing
  CaptureMethodCombo.Enabled := not DesktopCapture1.Active;
  CaptureModeCombo.Enabled := not DesktopCapture1.Active;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  UpdateStats;
end;

procedure TForm1.UpdateStats;
var
  ElapsedSec: Double;
  SendFPS, ReceiveFPS: Double;
  CompressionRatio: Double;
  AvgFullSize, AvgDirtySize: Double;
  DataSaved: Int64;
begin
  if FStartTime > 0 then
    ElapsedSec := (Now - FStartTime) * 24 * 60 * 60
  else
    ElapsedSec := 0;

  if ElapsedSec > 0 then
  begin
    SendFPS := FFramesSent / ElapsedSec;
    ReceiveFPS := FFramesReceived / ElapsedSec;
  end
  else
  begin
    SendFPS := 0;
    ReceiveFPS := 0;
  end;

  // Calculate compression stats
  if FFullFramesSent > 0 then
    AvgFullSize := FBytesSent / FFullFramesSent
  else
    AvgFullSize := 0;

  if FDirtyFramesSent > 0 then
    AvgDirtySize := FBytesSent / FDirtyFramesSent
  else
    AvgDirtySize := 0;

  if (AvgFullSize > 0) and (FDirtyFramesSent > 0) then
  begin
    CompressionRatio := (AvgDirtySize / AvgFullSize) * 100;
    DataSaved := Round((FFullFramesSent + FDirtyFramesSent) * AvgFullSize) - FBytesSent;
  end
  else
  begin
    CompressionRatio := 0;
    DataSaved := 0;
  end;

  StatsLabel.Caption := Format(
    'NETWORK PERFORMANCE:' + sLineBreak +
    '━━━━━━━━━━━━━━━━━━━━━━━' + sLineBreak +
    '📡 TRANSMISSION' + sLineBreak +
    '   Sent: %d frames (%.1f FPS)' + sLineBreak +
    '   Received: %d frames (%.1f FPS)' + sLineBreak +
    '   Data: %s transferred' + sLineBreak +
    sLineBreak +
    '⚡ OPTIMIZATION' + sLineBreak +
    '   Full Frames: %d' + sLineBreak +
    '   Dirty Frames: %d' + sLineBreak +
    '   Compression: %.1f%%' + sLineBreak +
    '   Data Saved: %s' + sLineBreak +
    sLineBreak +
    '🎯 METHOD: %s' + sLineBreak +
    '🎯 MODE: %s',
    [FFramesSent, SendFPS,
     FFramesReceived, ReceiveFPS,
     FormatBytesSize(FBytesSent),
     FFullFramesSent, FDirtyFramesSent,
     CompressionRatio,
     FormatBytesSize(DataSaved),
     CaptureMethodCombo.Text,
     CaptureModeCombo.Text]);
end;

end.
