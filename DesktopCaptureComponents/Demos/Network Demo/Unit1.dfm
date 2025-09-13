object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Desktop Capture - Simple'
  ClientHeight = 600
  ClientWidth = 800
  Color = clBtnFace
  DoubleBuffered = True
  DoubleBufferedMode = dbmRequested
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 13
  object FPSLabel: TLabel
    Left = 590
    Top = 20
    Width = 43
    Height = 13
    Caption = 'FPS: 0.0'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SizeLabel: TLabel
    Left = 590
    Top = 40
    Width = 44
    Height = 13
    Caption = 'Size: 0x0'
  end
  object DataSizeLabel: TLabel
    Left = 590
    Top = 60
    Width = 45
    Height = 13
    Caption = 'Data: 0 B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Image1: TImage
    Left = 8
    Top = 143
    Width = 784
    Height = 449
    Stretch = True
  end
  object ServerGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 150
    Height = 129
    Caption = 'SERVER'
    TabOrder = 0
    object StartServerBtn: TButton
      Left = 8
      Top = 16
      Width = 60
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = StartServerBtnClick
    end
    object StopServerBtn: TButton
      Left = 80
      Top = 16
      Width = 60
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = StopServerBtnClick
    end
    object PortEdit: TEdit
      Left = 8
      Top = 48
      Width = 60
      Height = 21
      TabOrder = 2
      Text = '8080'
    end
  end
  object ClientGroup: TGroupBox
    Left = 170
    Top = 8
    Width = 200
    Height = 129
    Caption = 'CLIENT'
    TabOrder = 1
    object ConnectBtn: TButton
      Left = 8
      Top = 16
      Width = 60
      Height = 25
      Caption = 'Connect'
      TabOrder = 0
      OnClick = ConnectBtnClick
    end
    object DisconnectBtn: TButton
      Left = 80
      Top = 16
      Width = 60
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 1
      OnClick = DisconnectBtnClick
    end
    object IPEdit: TEdit
      Left = 8
      Top = 48
      Width = 80
      Height = 21
      TabOrder = 2
      Text = '127.0.0.1'
    end
    object StartCaptureBtn: TButton
      Left = 8
      Top = 72
      Width = 60
      Height = 25
      Caption = 'Capture'
      TabOrder = 3
      OnClick = StartCaptureBtnClick
    end
    object StopCaptureBtn: TButton
      Left = 80
      Top = 72
      Width = 60
      Height = 25
      Caption = 'Stop'
      TabOrder = 4
      OnClick = StopCaptureBtnClick
    end
  end
  object SettingsGroup: TGroupBox
    Left = 380
    Top = 8
    Width = 200
    Height = 129
    Caption = 'SETTINGS'
    TabOrder = 2
    object MonitorCombo: TComboBox
      Left = 8
      Top = 16
      Width = 180
      Height = 21
      Style = csDropDownList
      TabOrder = 0
      OnChange = MonitorComboChange
    end
    object ModeCombo: TComboBox
      Left = 8
      Top = 64
      Width = 180
      Height = 21
      Style = csDropDownList
      TabOrder = 1
      OnChange = ModeComboChange
    end
    object FPSTrack: TTrackBar
      Left = 8
      Top = 88
      Width = 180
      Height = 25
      Max = 60
      Min = 5
      Position = 30
      TabOrder = 2
      OnChange = FPSTrackChange
    end
    object TheMethodCombo: TComboBox
      Left = 8
      Top = 41
      Width = 180
      Height = 21
      Style = csDropDownList
      TabOrder = 3
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 368
    Top = 352
  end
  object ncServer: TncServerSource
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncServerConnected
    OnDisconnected = ncServerDisconnected
    OnHandleCommand = ncServerHandleCommand
    Left = 104
    Top = 64
  end
  object ncClient: TncClientSource
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncClientConnected
    OnDisconnected = ncClientDisconnected
    Left = 328
    Top = 56
  end
  object Capture: TDesktopCapture
    OnFrameCaptured = CaptureFrameCaptured
    Left = 328
    Top = 288
  end
  object Receiver: TDesktopCaptureReceiver
    TargetImage = Image1
    OnFrameReceived = ReceiverFrameReceived
    Left = 400
    Top = 288
  end
end
