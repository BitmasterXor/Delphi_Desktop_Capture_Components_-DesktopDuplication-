object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Desktop Capture Demo - (Basic Components Demo)'
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object MainPanel: TPanel
    Left = 0
    Top = 0
    Width = 900
    Height = 600
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Splitter1: TSplitter
      Left = 280
      Top = 0
      Height = 600
    end
    object ControlPanel: TPanel
      Left = 0
      Top = 0
      Width = 280
      Height = 600
      Align = alLeft
      TabOrder = 0
      object SettingsGroupBox: TGroupBox
        Left = 8
        Top = 8
        Width = 264
        Height = 320
        Caption = 'Capture Settings'
        TabOrder = 0
        object MethodLabel: TLabel
          Left = 8
          Top = 24
          Width = 40
          Height = 13
          Caption = 'Method:'
        end
        object ModeLabel: TLabel
          Left = 8
          Top = 72
          Width = 30
          Height = 13
          Caption = 'Mode:'
        end
        object MonitorLabel: TLabel
          Left = 8
          Top = 120
          Width = 40
          Height = 13
          Caption = 'Monitor:'
        end
        object MonitorInfoLabel: TLabel
          Left = 8
          Top = 168
          Width = 133
          Height = 13
          Caption = 'All monitor options available'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clGreen
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Visible = False
        end
        object FPSLabel: TLabel
          Left = 8
          Top = 192
          Width = 22
          Height = 13
          Caption = 'FPS:'
        end
        object FPSValueLabel: TLabel
          Left = 200
          Top = 192
          Width = 37
          Height = 13
          Caption = '30 FPS'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object MethodComboBox: TComboBox
          Left = 8
          Top = 40
          Width = 248
          Height = 21
          Style = csDropDownList
          TabOrder = 0
          OnChange = MethodComboBoxChange
        end
        object ModeComboBox: TComboBox
          Left = 8
          Top = 88
          Width = 248
          Height = 21
          Style = csDropDownList
          TabOrder = 1
          OnChange = ModeComboBoxChange
        end
        object MonitorComboBox: TComboBox
          Left = 8
          Top = 136
          Width = 248
          Height = 21
          Style = csDropDownList
          TabOrder = 2
          OnChange = MonitorComboBoxChange
        end
        object FPSTrackBar: TTrackBar
          Left = 8
          Top = 208
          Width = 248
          Height = 33
          Max = 60
          Min = 5
          Position = 30
          TabOrder = 3
          OnChange = FPSTrackBarChange
        end
        object IncludeCursorCheckBox: TCheckBox
          Left = 8
          Top = 256
          Width = 97
          Height = 17
          Caption = 'Include Cursor'
          Checked = True
          State = cbChecked
          TabOrder = 4
          OnClick = IncludeCursorCheckBoxClick
        end
        object StartButton: TButton
          Left = 8
          Top = 280
          Width = 75
          Height = 25
          Caption = 'Start'
          TabOrder = 5
          OnClick = StartButtonClick
        end
        object StopButton: TButton
          Left = 96
          Top = 280
          Width = 75
          Height = 25
          Caption = 'Stop'
          Enabled = False
          TabOrder = 6
          OnClick = StopButtonClick
        end
      end
      object StatusGroupBox: TGroupBox
        Left = 8
        Top = 336
        Width = 264
        Height = 120
        Caption = 'Status Information'
        TabOrder = 1
        object StatusLabel: TLabel
          Left = 8
          Top = 24
          Width = 31
          Height = 13
          Caption = 'Ready'
        end
        object FPSDisplayLabel: TLabel
          Left = 8
          Top = 48
          Width = 82
          Height = 13
          Caption = 'Actual FPS: 0.0'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clBlue
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object ResolutionLabel: TLabel
          Left = 8
          Top = 72
          Width = 122
          Height = 13
          Caption = 'Image Size: Not Available'
        end
        object PixelChangesLabel: TLabel
          Left = 8
          Top = 96
          Width = 110
          Height = 13
          Caption = 'Pixel Changes: 0 bytes'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clMaroon
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
        end
      end
    end
    object StatusPanel: TPanel
      Left = 283
      Top = 0
      Width = 617
      Height = 600
      Align = alClient
      BevelOuter = bvLowered
      TabOrder = 1
      object ScrollBox1: TScrollBox
        Left = 1
        Top = 1
        Width = 615
        Height = 598
        Align = alClient
        TabOrder = 0
        object Image1: TImage
          Left = 0
          Top = 0
          Width = 608
          Height = 591
          Stretch = True
        end
      end
    end
  end
  object StatusTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = StatusTimerTimer
    Left = 200
    Top = 384
  end
  object DesktopCapture1: TDesktopCapture
    OnFrameCaptured = DesktopCapture1FrameCaptured
    Left = 112
    Top = 488
  end
  object DesktopCaptureReceiver1: TDesktopCaptureReceiver
    OnFrameReceived = DesktopCaptureReceiver1FrameReceived
    Left = 112
    Top = 544
  end
end
