object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 457
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 121
    Align = alTop
    TabOrder = 0
    object edtPort: TLabeledEdit
      Left = 16
      Top = 35
      Width = 121
      Height = 21
      EditLabel.Width = 52
      EditLabel.Height = 13
      EditLabel.Caption = 'Comm Port'
      ImeName = 'Microsoft Office IME 2007'
      NumbersOnly = True
      TabOrder = 0
      Text = '11'
    end
    object edtBaudRate: TLabeledEdit
      Left = 16
      Top = 83
      Width = 121
      Height = 21
      EditLabel.Width = 47
      EditLabel.Height = 13
      EditLabel.Caption = 'BaudRate'
      ImeName = 'Microsoft Office IME 2007'
      NumbersOnly = True
      TabOrder = 1
      Text = '9600'
    end
    object btnOpen: TButton
      Left = 176
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Open'
      TabOrder = 2
      OnClick = btnOpenClick
    end
    object btnGetData: TButton
      Left = 464
      Top = 33
      Width = 100
      Height = 25
      Caption = 'GetData'
      TabOrder = 3
      OnClick = btnGetDataClick
    end
    object btnClose: TButton
      Left = 176
      Top = 81
      Width = 75
      Height = 25
      Caption = 'Close'
      TabOrder = 4
      OnClick = btnCloseClick
    end
    object edtDataIndex: TLabeledEdit
      Left = 327
      Top = 35
      Width = 121
      Height = 21
      EditLabel.Width = 54
      EditLabel.Height = 13
      EditLabel.Caption = 'Data Index'
      ImeName = 'Microsoft Office IME 2007'
      NumbersOnly = True
      TabOrder = 5
      Text = '-1'
    end
    object btnCommaData: TButton
      Left = 464
      Top = 81
      Width = 100
      Height = 25
      Caption = 'GetCommaData'
      TabOrder = 6
      OnClick = btnCommaDataClick
    end
  end
  object Memo1: TMemo
    Left = 0
    Top = 121
    Width = 635
    Height = 336
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      'CR1K Tester...')
    ScrollBars = ssVertical
    TabOrder = 1
  end
end
