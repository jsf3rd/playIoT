object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Test Reader'
  ClientHeight = 299
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox2: TGroupBox
    Left = 26
    Top = 64
    Width = 255
    Height = 161
    Caption = 'Reader'
    TabOrder = 0
    object btnGetInit: TButton
      Left = 14
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Init'
      TabOrder = 0
      OnClick = btnGetInitClick
    end
    object btnGetFirst: TButton
      Left = 14
      Top = 59
      Width = 75
      Height = 25
      Caption = 'get First'
      TabOrder = 1
      OnClick = btnGetFirstClick
    end
    object btnAutoGet: TButton
      Left = 14
      Top = 90
      Width = 75
      Height = 25
      Caption = 'auto'
      TabOrder = 2
    end
    object btnGetNext: TButton
      Left = 92
      Top = 59
      Width = 75
      Height = 25
      Caption = 'get Next'
      TabOrder = 3
      OnClick = btnGetNextClick
    end
    object btnGetLast: TButton
      Left = 173
      Top = 59
      Width = 75
      Height = 25
      Caption = 'get Last'
      TabOrder = 4
      OnClick = btnGetLastClick
    end
    object edtGetInterval: TEdit
      Left = 92
      Top = 92
      Width = 43
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      TabOrder = 5
      Text = '1000'
    end
    object btnGetFinal: TButton
      Left = 14
      Top = 121
      Width = 75
      Height = 25
      Caption = 'Finalize'
      TabOrder = 6
    end
  end
  object edtCodeName: TLabeledEdit
    Left = 86
    Top = 16
    Width = 99
    Height = 21
    Alignment = taRightJustify
    EditLabel.Width = 52
    EditLabel.Height = 13
    EditLabel.Caption = 'CodeName'
    LabelPosition = lpLeft
    TabOrder = 1
    Text = 'Global\'
  end
  object Memo1: TMemo
    Left = 312
    Top = 48
    Width = 297
    Height = 217
    Lines.Strings = (
      'Memo1')
    TabOrder = 2
  end
end
