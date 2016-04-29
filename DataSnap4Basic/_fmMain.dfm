object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'DataSnap for Basic'
  ClientHeight = 392
  ClientWidth = 634
  Color = clBtnFace
  Constraints.MinHeight = 420
  Constraints.MinWidth = 620
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object mmLog: TMemo
    Left = 200
    Top = 0
    Width = 434
    Height = 373
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 373
    Width = 634
    Height = 19
    Panels = <
      item
        Width = 100
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 200
    Height = 373
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    object GroupBox1: TGroupBox
      Left = 9
      Top = 8
      Width = 185
      Height = 217
      Caption = 'User List'
      TabOrder = 0
      object lbUserList: TListBox
        Left = 2
        Top = 15
        Width = 181
        Height = 200
        Align = alClient
        ImeName = 'Microsoft Office IME 2007'
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object StaticText1: TStaticText
      Left = 117
      Top = 350
      Width = 77
      Height = 17
      Caption = #9426' ENBGROUP.'
      TabOrder = 1
    end
  end
  object ActionList: TActionList
    Left = 288
    Top = 8
    object actClearLog: TAction
      Caption = #47196#44536#49325#51228'(&X)'
      ShortCut = 16472
      OnExecute = actClearLogExecute
    end
    object actAbout: TAction
      Caption = '&About'
      ShortCut = 112
      OnExecute = actAboutExecute
    end
    object actExit: TAction
      Caption = '&Exit'
      ShortCut = 16465
      OnExecute = actExitExecute
    end
    object actOptions: TAction
      Caption = '&Options'
      OnExecute = actOptionsExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 8
    object File1: TMenuItem
      Caption = '&File'
      object X1: TMenuItem
        Action = actClearLog
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object ools1: TMenuItem
      Caption = '&Tools'
      object Option1: TMenuItem
        Action = actOptions
        ShortCut = 121
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Action = actAbout
      end
    end
  end
end
