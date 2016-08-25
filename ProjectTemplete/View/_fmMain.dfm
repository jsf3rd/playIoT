object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'Templete Form'
  ClientHeight = 462
  ClientWidth = 684
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object MainMenu: TMainMenu
    Left = 136
    Top = 24
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object ool1: TMenuItem
      Caption = '&Tool'
      object ShowLog1: TMenuItem
        Action = actShowLog
      end
      object ShowIniFile1: TMenuItem
        Action = actShowIni
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About'
        OnClick = actAboutExecute
      end
    end
    object MenuTest: TMenuItem
      Caption = 'T&est'
      Visible = False
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    Left = 216
    Top = 24
  end
  object ActionList: TActionList
    Left = 72
    Top = 24
    object actAbout: TAction
      Caption = '&About'
      OnExecute = actAboutExecute
    end
    object actClearLog: TAction
      Caption = '&Clear Log'
      ShortCut = 16472
      OnExecute = actClearLogExecute
    end
    object actExit: TAction
      Caption = '&Exit'
      ShortCut = 16465
      OnExecute = actExitExecute
    end
    object actShowIni: TAction
      Caption = 'Show &IniFile'
      OnExecute = actShowIniExecute
    end
    object actShowLog: TAction
      Caption = 'Show &Log'
      OnExecute = actShowLogExecute
    end
    object actTestMenu: TAction
      Caption = '&Test&Menu'
      ShortCut = 16456
      OnExecute = actTestMenuExecute
    end
  end
end
