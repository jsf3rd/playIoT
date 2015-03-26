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
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ActionList: TActionList
    Left = 56
    Top = 24
    object actAbout: TAction
      Caption = '&About'
      OnExecute = actAboutExecute
    end
  end
  object MainMenu: TMainMenu
    Left = 136
    Top = 24
    object File1: TMenuItem
      Caption = '&File'
    end
    object ool1: TMenuItem
      Caption = '&Tool'
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Action = actAbout
      end
    end
  end
  object ApplicationEvents: TApplicationEvents
    OnException = ApplicationEventsException
    Left = 240
    Top = 24
  end
end
