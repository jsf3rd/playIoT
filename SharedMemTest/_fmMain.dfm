object Form2: TForm2
  Left = 0
  Top = 0
  Caption = 'Shared Memory Test'
  ClientHeight = 493
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 257
    Width = 635
    Height = 236
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 0
    ExplicitWidth = 631
    ExplicitHeight = 235
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 635
    Height = 257
    Align = alTop
    TabOrder = 1
    ExplicitWidth = 631
    object edtCodeName: TLabeledEdit
      Left = 86
      Top = 16
      Width = 99
      Height = 21
      Alignment = taRightJustify
      EditLabel.Width = 52
      EditLabel.Height = 21
      EditLabel.Caption = 'CodeName'
      LabelPosition = lpLeft
      TabOrder = 0
      Text = 'TEST01'
    end
    object GroupBox2: TGroupBox
      Left = 202
      Top = 56
      Width = 255
      Height = 161
      Caption = 'Reader'
      TabOrder = 1
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
        OnClick = btnAutoGetClick
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
        OnClick = btnGetFinalClick
      end
    end
  end
  object GroupBox1: TGroupBox
    Left = 24
    Top = 56
    Width = 161
    Height = 161
    Caption = 'Writer'
    TabOrder = 2
    object btnPutInit: TButton
      Left = 13
      Top = 28
      Width = 75
      Height = 25
      Caption = 'Init'
      TabOrder = 0
      OnClick = btnPutInitClick
    end
    object btnPut: TButton
      Left = 13
      Top = 59
      Width = 75
      Height = 25
      Caption = 'put'
      TabOrder = 1
      OnClick = btnPutClick
    end
    object btnAutoPut: TButton
      Left = 13
      Top = 90
      Width = 75
      Height = 25
      Caption = 'auto'
      TabOrder = 2
      OnClick = btnAutoPutClick
    end
    object edtPutInterval: TEdit
      Left = 94
      Top = 92
      Width = 43
      Height = 21
      Alignment = taRightJustify
      NumbersOnly = True
      TabOrder = 3
      Text = '1000'
    end
    object btnPutFinal: TButton
      Left = 13
      Top = 121
      Width = 75
      Height = 25
      Caption = 'Finalize'
      TabOrder = 4
      OnClick = btnPutFinalClick
    end
  end
  object GetTimer: TTimer
    Enabled = False
    Interval = 10000
    OnTimer = GetTimerTimer
    Left = 416
    Top = 72
  end
  object PutTimer: TTimer
    Enabled = False
    Interval = 300
    OnTimer = PutTimerTimer
    Left = 144
    Top = 72
  end
end
