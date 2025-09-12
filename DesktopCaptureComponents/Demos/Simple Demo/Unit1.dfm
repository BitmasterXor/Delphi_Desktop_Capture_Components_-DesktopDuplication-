object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Desktop Capture Demo - SIMPLIFIED'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  OnResize = FormResize
  TextHeight = 15
  object Image1: TImage
    Left = 0
    Top = 120
    Width = 900
    Height = 459
    Align = alClient
    Center = True
    Proportional = True
    Stretch = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 120
    Align = alTop
    TabOrder = 0
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 898
      Height = 118
      Align = alClient
      TabOrder = 0
      object Label1: TLabel
        Left = 16
        Top = 88
        Width = 58
        Height = 15
        Caption = 'Target FPS:'
      end
      object FPSLabel: TLabel
        Left = 200
        Top = 88
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
      object StartBtn: TButton
        Left = 16
        Top = 16
        Width = 100
        Height = 30
        Caption = '&Start Capture'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = StartBtnClick
      end
      object StopBtn: TButton
        Left = 130
        Top = 16
        Width = 100
        Height = 30
        Caption = 'S&top Capture'
        Enabled = False
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -12
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = StopBtnClick
      end
      object GroupBox1: TGroupBox
        Left = 260
        Top = 8
        Width = 120
        Height = 70
        Caption = ' Capture Method '
        TabOrder = 2
        object RadioGDI: TRadioButton
          Left = 16
          Top = 20
          Width = 60
          Height = 17
          Caption = 'GDI'
          TabOrder = 0
        end
        object RadioDXGI: TRadioButton
          Left = 16
          Top = 43
          Width = 60
          Height = 17
          Caption = 'DXGI'
          Checked = True
          TabOrder = 1
          TabStop = True
        end
      end
      object GroupBox2: TGroupBox
        Left = 400
        Top = 8
        Width = 130
        Height = 70
        Caption = ' Capture Mode '
        TabOrder = 3
        object RadioFullFrame: TRadioButton
          Left = 16
          Top = 20
          Width = 80
          Height = 17
          Caption = 'Full Frame'
          TabOrder = 0
        end
        object RadioDirtyOnly: TRadioButton
          Left = 16
          Top = 43
          Width = 80
          Height = 17
          Caption = 'Dirty Only'
          Checked = True
          TabOrder = 1
          TabStop = True
        end
      end
      object IncludeCursorChk: TCheckBox
        Left = 560
        Top = 30
        Width = 100
        Height = 17
        Caption = 'Include Cursor'
        Checked = True
        State = cbChecked
        TabOrder = 4
      end
      object FPSTrack: TTrackBar
        Left = 80
        Top = 84
        Width = 113
        Height = 25
        Max = 60
        Min = 5
        Position = 30
        TabOrder = 5
        OnChange = FPSTrackChange
      end
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 579
    Width = 900
    Height = 21
    Panels = <>
    SimplePanel = True
    SimpleText = 'Ready - Click Start to begin desktop capture demo'
  end
  object DesktopCapture1: TDesktopCapture
    OnFrameCaptured = DesktopCapture1FrameCaptured
    Left = 668
    Top = 50
  end
  object DesktopCaptureReceiver1: TDesktopCaptureReceiver
    OnFrameReceived = DesktopCaptureReceiver1FrameReceived
    Left = 806
    Top = 50
  end
end
