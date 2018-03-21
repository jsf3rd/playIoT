object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'NetDEVSDK Sample'
  ClientHeight = 299
  ClientWidth = 635
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
  object Button2: TButton
    Left = 8
    Top = 71
    Width = 75
    Height = 25
    Caption = 'Login'
    TabOrder = 0
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 149
    Width = 75
    Height = 25
    Caption = 'Real Play'
    TabOrder = 1
    OnClick = Button3Click
  end
  object Panel1: TPanel
    Left = 89
    Top = 71
    Width = 538
    Height = 220
    TabOrder = 2
    StyleElements = [seFont, seBorder]
  end
  object Button4: TButton
    Left = 8
    Top = 266
    Width = 75
    Height = 25
    Caption = 'Capture'
    TabOrder = 3
    OnClick = Button4Click
  end
  object Button1: TButton
    Left = 8
    Top = 102
    Width = 75
    Height = 25
    Caption = 'Logout'
    TabOrder = 4
    OnClick = Button1Click
  end
  object Button5: TButton
    Left = 8
    Top = 180
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 5
    OnClick = Button5Click
  end
  object edtHost: TLabeledEdit
    Left = 8
    Top = 32
    Width = 105
    Height = 21
    EditLabel.Width = 22
    EditLabel.Height = 13
    EditLabel.Caption = 'Host'
    TabOrder = 6
    Text = '192.168.0.15'
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
    TabOrder = 7
    Text = '80'
  end
  object edtID: TLabeledEdit
    Left = 239
    Top = 32
    Width = 105
    Height = 21
    EditLabel.Width = 11
    EditLabel.Height = 13
    EditLabel.Caption = 'ID'
    TabOrder = 8
    Text = 'root'
  end
  object edtPwd: TLabeledEdit
    Left = 350
    Top = 32
    Width = 105
    Height = 21
    EditLabel.Width = 46
    EditLabel.Height = 13
    EditLabel.Caption = 'Password'
    TabOrder = 9
    Text = 'judico'
  end
end
