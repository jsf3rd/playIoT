object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'miniSEED Parser v1.3'
  ClientHeight = 362
  ClientWidth = 634
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object mmLog: TMemo
    Left = 0
    Top = 121
    Width = 634
    Height = 241
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      'Do not use commercial purpose.'
      ''
      'jsf3rd@playiot.biz'
      ''
      '(c)playIoT 2016.'
      ''
      '----------------------------------------------------')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 634
    Height = 121
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 19
      Width = 46
      Height = 13
      Caption = 'File Name'
    end
    object Label2: TLabel
      Left = 16
      Top = 52
      Width = 51
      Height = 13
      Caption = 'Begin Time'
    end
    object Label3: TLabel
      Left = 16
      Top = 85
      Width = 43
      Height = 13
      Caption = 'End Time'
    end
    object edtFileName: TJvFilenameEdit
      Left = 74
      Top = 16
      Width = 187
      Height = 21
      AddQuotes = False
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 0
      Text = ''
    end
    object btnMSeed2MSeed_ST1: TButton
      Left = 381
      Top = 12
      Width = 100
      Height = 25
      Caption = 'MSeed2MSeed ST1'
      TabOrder = 1
      OnClick = btnMSeed2MSeed_ST1Click
    end
    object btnMSeed2ASCII: TButton
      Left = 275
      Top = 12
      Width = 100
      Height = 25
      Caption = 'MSeed2ASCII'
      TabOrder = 2
      OnClick = btnMSeed2ASCIIClick
    end
    object btnASCII2MSeed_ST1: TButton
      Left = 487
      Top = 12
      Width = 100
      Height = 25
      Caption = 'ASCII2MSeed ST1'
      TabOrder = 3
      OnClick = btnASCII2MSeed_ST1Click
    end
    object btnASCII2MSeed_ST2: TButton
      Left = 487
      Top = 47
      Width = 100
      Height = 25
      Caption = 'ASCII2MSeed ST2'
      TabOrder = 4
      OnClick = btnASCII2MSeed_ST2Click
    end
    object btnMSeed2MSeed_ST2: TButton
      Left = 381
      Top = 47
      Width = 100
      Height = 25
      Caption = 'MSeed2MSeed ST2'
      TabOrder = 5
      OnClick = btnMSeed2MSeed_ST2Click
    end
    object edtBegin: TEdit
      Left = 74
      Top = 49
      Width = 187
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 6
      Text = '2014-11-05 03:30:30 '
    end
    object edtEnd: TEdit
      Left = 74
      Top = 82
      Width = 187
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 7
      Text = '2014-11-05 03:30:30 '
    end
  end
  object ActionList: TActionList
    Left = 312
    Top = 184
    object actTest: TAction
      Caption = 'Test'
      ShortCut = 16456
      OnExecute = actTestExecute
    end
  end
end
