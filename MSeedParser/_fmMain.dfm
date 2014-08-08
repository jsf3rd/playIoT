object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'MSeed Parser'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 91
    Width = 635
    Height = 209
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 91
    Align = alTop
    TabOrder = 1
    object edtFileName: TJvFilenameEdit
      Left = 16
      Top = 22
      Width = 218
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 0
      Text = ''
    end
    object btnMSeed2MSeed_ST1: TButton
      Left = 357
      Top = 20
      Width = 100
      Height = 25
      Caption = 'MSeed2MSeed ST1'
      TabOrder = 1
      OnClick = btnMSeed2MSeed_ST1Click
    end
    object btnMSeed2ASCII: TButton
      Left = 251
      Top = 51
      Width = 100
      Height = 25
      Caption = 'MSeed2ASCII'
      TabOrder = 2
      OnClick = btnMSeed2ASCIIClick
    end
    object btnASCII2MSeed_ST1: TButton
      Left = 463
      Top = 20
      Width = 100
      Height = 25
      Caption = 'ASCII2MSeed ST1'
      TabOrder = 3
      OnClick = btnASCII2MSeed_ST1Click
    end
    object btnASCII2MSeed_ST2: TButton
      Left = 463
      Top = 51
      Width = 100
      Height = 25
      Caption = 'ASCII2MSeed ST2'
      TabOrder = 4
      OnClick = btnASCII2MSeed_ST2Click
    end
    object btnMSeed2MSeed_ST2: TButton
      Left = 357
      Top = 51
      Width = 100
      Height = 25
      Caption = 'MSeed2MSeed ST2'
      TabOrder = 5
      OnClick = btnMSeed2MSeed_ST2Click
    end
    object btnTEST: TButton
      Left = 251
      Top = 20
      Width = 100
      Height = 25
      Caption = 'TEST'
      TabOrder = 6
      OnClick = btnTESTClick
    end
  end
  object TestTimer: TTimer
    Enabled = False
    Interval = 500
    OnTimer = TestTimerTimer
    Left = 32
    Top = 240
  end
end
