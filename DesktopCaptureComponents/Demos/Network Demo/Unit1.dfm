object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Desktop Capture Network Demo - Professional UI'
  ClientHeight = 650
  ClientWidth = 1200
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 730
    Height = 600
    Caption = ' SERVER (Receiver) '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object ServerRecievingImage: TImage
      Left = 16
      Top = 80
      Width = 700
      Height = 500
      Center = True
      Proportional = True
      Stretch = True
    end
    object Panel1: TPanel
      Left = 16
      Top = 24
      Width = 700
      Height = 50
      ParentBackground = False
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 15
        Width = 25
        Height = 15
        Caption = 'Port:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object ServerStatusLabel: TLabel
        Left = 240
        Top = 15
        Width = 78
        Height = 15
        Caption = 'Server stopped'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object StartServerBtn: TButton
        Left = 120
        Top = 10
        Width = 80
        Height = 25
        Caption = 'Start Server'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = StartServerBtnClick
      end
      object StopServerBtn: TButton
        Left = 450
        Top = 10
        Width = 80
        Height = 25
        Caption = 'Stop Server'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = StopServerBtnClick
      end
      object ServerPortEdit: TEdit
        Left = 50
        Top = 12
        Width = 60
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        Text = '8080'
      end
      object ClearServerBtn: TButton
        Left = 540
        Top = 10
        Width = 80
        Height = 25
        Caption = 'Clear Display'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        OnClick = ClearServerBtnClick
      end
    end
  end
  object GroupBox2: TGroupBox
    Left = 750
    Top = 8
    Width = 435
    Height = 361
    Caption = ' CLIENT (Sender) '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    object Panel2: TPanel
      Left = 16
      Top = 24
      Width = 400
      Height = 80
      ParentBackground = False
      TabOrder = 0
      object Label2: TLabel
        Left = 16
        Top = 15
        Width = 48
        Height = 15
        Caption = 'Server IP:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label3: TLabel
        Left = 16
        Top = 50
        Width = 25
        Height = 15
        Caption = 'Port:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object ClientStatusLabel: TLabel
        Left = 240
        Top = 30
        Width = 72
        Height = 15
        Caption = 'Disconnected'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clMaroon
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object ConnectBtn: TButton
        Left = 150
        Top = 12
        Width = 80
        Height = 25
        Caption = 'Connect'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnClick = ConnectBtnClick
      end
      object DisconnectBtn: TButton
        Left = 150
        Top = 47
        Width = 80
        Height = 25
        Caption = 'Disconnect'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnClick = DisconnectBtnClick
      end
      object ServerIPEdit: TEdit
        Left = 75
        Top = 12
        Width = 65
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 2
        Text = '127.0.0.1'
      end
      object ClientPortEdit: TEdit
        Left = 75
        Top = 47
        Width = 65
        Height = 23
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 3
        Text = '8080'
      end
    end
    object StartCaptureBtn: TButton
      Left = 16
      Top = 110
      Width = 100
      Height = 30
      Caption = 'Start Capture'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = StartCaptureBtnClick
    end
    object StopCaptureBtn: TButton
      Left = 130
      Top = 110
      Width = 100
      Height = 30
      Caption = 'Stop Capture'
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = StopCaptureBtnClick
    end
    object GroupBox3: TGroupBox
      Left = 16
      Top = 150
      Width = 400
      Height = 203
      Caption = ' Capture Settings '
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      object Label4: TLabel
        Left = 16
        Top = 30
        Width = 90
        Height = 15
        Caption = 'Capture Method:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label5: TLabel
        Left = 16
        Top = 70
        Width = 79
        Height = 15
        Caption = 'Capture Mode:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object Label6: TLabel
        Left = 16
        Top = 150
        Width = 62
        Height = 15
        Caption = 'Frame Rate:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object FPSLabel: TLabel
        Left = 300
        Top = 150
        Width = 40
        Height = 15
        Caption = 'FPS: 30'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object CaptureMethodCombo: TComboBox
        Left = 16
        Top = 48
        Width = 360
        Height = 23
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 0
        OnChange = CaptureMethodComboChange
      end
      object CaptureModeCombo: TComboBox
        Left = 16
        Top = 88
        Width = 360
        Height = 23
        Style = csDropDownList
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        TabOrder = 1
        OnChange = CaptureModeComboChange
      end
      object FPSTrack: TTrackBar
        Left = 90
        Top = 145
        Width = 200
        Height = 25
        Max = 60
        Min = 5
        Position = 30
        TabOrder = 2
        OnChange = FPSTrackChange
      end
      object IncludeCursorChk: TCheckBox
        Left = 16
        Top = 120
        Width = 150
        Height = 17
        Caption = 'Include mouse cursor'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
        State = cbChecked
        TabOrder = 3
      end
      object OptimizeForNetworkChk: TCheckBox
        Left = 16
        Top = 180
        Width = 200
        Height = 17
        Caption = 'Optimize for network streaming'
        Checked = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        State = cbChecked
        TabOrder = 4
        OnClick = OptimizeForNetworkChkClick
      end
    end
  end
  object GroupBox4: TGroupBox
    Left = 750
    Top = 375
    Width = 435
    Height = 233
    Caption = ' Network Statistics '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clNavy
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    object StatsLabel: TLabel
      Left = 16
      Top = 24
      Width = 400
      Height = 130
      AutoSize = False
      Caption = 'No data yet...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      WordWrap = True
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 623
    Width = 1200
    Height = 27
    Panels = <>
    SimplePanel = True
    SimpleText = 
      'Ready - Start server first, then connect client and begin captur' +
      'e'
  end
  object ncServerSource1: TncServerSource
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncServerSource1Connected
    OnDisconnected = ncServerSource1Disconnected
    OnHandleCommand = ncServerSource1HandleCommand
    Left = 224
    Top = 272
  end
  object ncClientSource1: TncClientSource
    EncryptionKey = 'SetEncryptionKey'
    OnConnected = ncClientSource1Connected
    OnDisconnected = ncClientSource1Disconnected
    OnHandleCommand = ncClientSource1HandleCommand
    Left = 946
    Top = 473
  end
  object DesktopCapture1: TDesktopCapture
    OnFrameCaptured = DesktopCapture1FrameCaptured
    Left = 1062
    Top = 473
  end
  object DesktopCaptureReceiver1: TDesktopCaptureReceiver
    OnFrameReceived = DesktopCaptureReceiver1FrameReceived
    Left = 352
    Top = 273
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 850
    Top = 473
  end
end
