object fmHeaderInfo: TfmHeaderInfo
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Header Info'
  ClientHeight = 207
  ClientWidth = 337
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object edtStation: TLabeledEdit
    Left = 192
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 34
    EditLabel.Height = 13
    EditLabel.Caption = 'Station'
    ImeName = 'Microsoft Office IME 2007'
    MaxLength = 5
    TabOrder = 0
    Text = 'KMG'
  end
  object edtChannel: TLabeledEdit
    Left = 192
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 39
    EditLabel.Height = 13
    EditLabel.Caption = 'Channel'
    ImeName = 'Microsoft Office IME 2007'
    MaxLength = 3
    TabOrder = 1
    Text = 'HGE'
  end
  object edtLocation: TLabeledEdit
    Left = 24
    Top = 72
    Width = 121
    Height = 21
    EditLabel.Width = 40
    EditLabel.Height = 13
    EditLabel.Caption = 'Location'
    ImeName = 'Microsoft Office IME 2007'
    MaxLength = 2
    TabOrder = 2
    Text = '00'
  end
  object edtNetwork: TLabeledEdit
    Left = 24
    Top = 24
    Width = 121
    Height = 21
    EditLabel.Width = 40
    EditLabel.Height = 13
    EditLabel.Caption = 'Network'
    ImeName = 'Microsoft Office IME 2007'
    MaxLength = 2
    TabOrder = 3
    Text = 'SL'
  end
  object edtSampleRate: TLabeledEdit
    Left = 24
    Top = 120
    Width = 121
    Height = 21
    EditLabel.Width = 60
    EditLabel.Height = 13
    EditLabel.Caption = 'Sample Rate'
    ImeName = 'Microsoft Office IME 2007'
    MaxLength = 3
    TabOrder = 4
    Text = '100'
  end
  object btnOK: TButton
    Left = 131
    Top = 168
    Width = 75
    Height = 25
    Caption = 'OK'
    TabOrder = 5
    OnClick = btnOKClick
  end
end
