object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'File Server v1.2'
  ClientHeight = 373
  ClientWidth = 592
  Color = clBtnFace
  Constraints.MinWidth = 600
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 278
    Width = 592
    Height = 95
    Align = alBottom
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Constraints.MinHeight = 95
    TabOrder = 0
    object Label1: TLabel
      Left = 242
      Top = 27
      Width = 30
      Height = 13
      Caption = 'Folder'
    end
    object btnStart: TButton
      Left = 128
      Top = 9
      Width = 75
      Height = 25
      Caption = 'Start'
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnStop: TButton
      Left = 128
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = btnStopClick
    end
    object edtPort: TLabeledEdit
      Left = 3
      Top = 42
      Width = 121
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Port'
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 2
      Text = '8001'
    end
    object StatusBar: TStatusBar
      Left = 0
      Top = 76
      Width = 592
      Height = 19
      Panels = <
        item
          Width = 100
        end
        item
          Width = 100
        end
        item
          Width = 50
        end>
    end
    object edtFolder: TJvDirectoryEdit
      Left = 240
      Top = 42
      Width = 337
      Height = 21
      DialogKind = dkWin32
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 4
      Text = ''
    end
  end
  object mmLog: TMemo
    Left = 0
    Top = 0
    Width = 592
    Height = 278
    Align = alClient
    Constraints.MinHeight = 200
    ImeName = 'Microsoft Office IME 2007'
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object IdTCPServer: TIdTCPServer
    Bindings = <>
    DefaultPort = 0
    OnConnect = IdTCPServerConnect
    OnDisconnect = IdTCPServerDisconnect
    OnExecute = IdTCPServerExecute
    Left = 520
    Top = 224
  end
end
