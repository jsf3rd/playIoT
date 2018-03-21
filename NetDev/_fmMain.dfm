object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'NetDEVSDK Sample'
  ClientHeight = 390
  ClientWidth = 590
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 97
    Top = 81
    Width = 493
    Height = 309
    Align = alClient
    TabOrder = 0
    StyleElements = [seFont, seBorder]
    ExplicitLeft = 119
    ExplicitTop = 71
    ExplicitWidth = 538
    ExplicitHeight = 220
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 590
    Height = 81
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 712
    object edtPwd: TLabeledEdit
      Left = 350
      Top = 32
      Width = 105
      Height = 21
      EditLabel.Width = 46
      EditLabel.Height = 13
      EditLabel.Caption = 'Password'
      TabOrder = 0
      Text = 'judico'
    end
    object edtID: TLabeledEdit
      Left = 239
      Top = 32
      Width = 105
      Height = 21
      EditLabel.Width = 11
      EditLabel.Height = 13
      EditLabel.Caption = 'ID'
      TabOrder = 1
      Text = 'root'
    end
    object edtPort: TLabeledEdit
      Left = 119
      Top = 32
      Width = 105
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Port'
      NumbersOnly = True
      TabOrder = 2
      Text = '80'
    end
    object edtHost: TLabeledEdit
      Left = 8
      Top = 32
      Width = 105
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Host'
      TabOrder = 3
      Text = '192.168.0.15'
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 81
    Width = 97
    Height = 309
    Align = alLeft
    TabOrder = 2
    object Button1: TButton
      Left = 8
      Top = 139
      Width = 75
      Height = 25
      Caption = 'Logout'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button5: TButton
      Left = 8
      Top = 108
      Width = 75
      Height = 25
      Caption = 'Stop'
      TabOrder = 1
      OnClick = Button5Click
    end
    object Button4: TButton
      Left = 8
      Top = 77
      Width = 75
      Height = 25
      Caption = 'Capture'
      TabOrder = 2
      OnClick = Button4Click
    end
    object Button3: TButton
      Left = 8
      Top = 46
      Width = 75
      Height = 25
      Caption = 'Real Play'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button2: TButton
      Left = 8
      Top = 15
      Width = 75
      Height = 25
      Caption = 'Login'
      TabOrder = 4
      OnClick = Button2Click
    end
  end
end
