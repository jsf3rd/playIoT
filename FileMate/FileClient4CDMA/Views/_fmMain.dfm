object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'FileMan v1.2'
  ClientHeight = 493
  ClientWidth = 692
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StatusBar: TStatusBar
    Left = 0
    Top = 474
    Width = 692
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 100
      end
      item
        Width = 150
      end
      item
        Width = 150
      end
      item
        Width = 50
      end>
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 241
    Height = 474
    Align = alLeft
    BevelOuter = bvLowered
    TabOrder = 1
    DesignSize = (
      241
      474)
    object GroupBox2: TGroupBox
      Left = 5
      Top = 87
      Width = 230
      Height = 66
      Caption = 'File Server Info'
      TabOrder = 0
      object edtFileIP: TLabeledEdit
        Left = 15
        Top = 33
        Width = 100
        Height = 21
        EditLabel.Width = 10
        EditLabel.Height = 13
        EditLabel.Caption = 'IP'
        ImeName = 'Microsoft Office IME 2007'
        TabOrder = 0
        Text = '58.151.219.107'
      end
      object edtFilePort: TLabeledEdit
        Left = 121
        Top = 33
        Width = 100
        Height = 21
        EditLabel.Width = 20
        EditLabel.Height = 13
        EditLabel.Caption = 'Prot'
        ImeName = 'Microsoft Office IME 2007'
        TabOrder = 1
        Text = '8001'
      end
    end
    object btnStart: TButton
      Left = 5
      Top = 400
      Width = 115
      Height = 30
      Action = actStartFileMan
      TabOrder = 1
    end
    object btnStop: TButton
      Left = 120
      Top = 400
      Width = 115
      Height = 30
      Action = actStopFileMan
      TabOrder = 2
    end
    object GroupBox4: TGroupBox
      Left = 5
      Top = 15
      Width = 230
      Height = 66
      Caption = 'CDMA Info'
      TabOrder = 3
      object edtCommPort: TLabeledEdit
        Left = 15
        Top = 33
        Width = 100
        Height = 21
        EditLabel.Width = 52
        EditLabel.Height = 13
        EditLabel.Caption = 'Comm Prot'
        ImeName = 'Microsoft Office IME 2007'
        TabOrder = 0
        Text = 'COM2'
      end
      object edtBaudRate: TLabeledEdit
        Left = 121
        Top = 33
        Width = 100
        Height = 21
        EditLabel.Width = 50
        EditLabel.Height = 13
        EditLabel.Caption = 'Baud Rate'
        ImeName = 'Microsoft Office IME 2007'
        TabOrder = 1
        Text = '9600'
      end
    end
    object StaticText1: TStaticText
      Left = 161
      Top = 451
      Width = 74
      Height = 17
      Anchors = [akLeft, akBottom]
      Caption = #9426' ENB Group.'
      TabOrder = 4
    end
    object GroupBox1: TGroupBox
      Left = 5
      Top = 159
      Width = 230
      Height = 235
      Caption = 'Folder List'
      TabOrder = 5
      object lbxFolder: TListBox
        Left = 3
        Top = 24
        Width = 224
        Height = 162
        ImeName = 'Microsoft Office IME 2007'
        ItemHeight = 13
        TabOrder = 0
      end
      object Button2: TButton
        Left = 10
        Top = 192
        Width = 100
        Height = 28
        Action = actAddFolder
        TabOrder = 1
      end
      object Button3: TButton
        Left = 120
        Top = 192
        Width = 100
        Height = 28
        Action = actDeleteFolder
        TabOrder = 2
      end
    end
  end
  object Panel3: TPanel
    Left = 241
    Top = 0
    Width = 451
    Height = 474
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 150
    TabOrder = 2
    object mmLog: TMemo
      Left = 0
      Top = 0
      Width = 451
      Height = 474
      Align = alClient
      ImeName = 'Microsoft Office IME 2007'
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitTop = -6
    end
  end
  object ActionList: TActionList
    Left = 624
    Top = 200
    object actStartFileMan: TAction
      Caption = '&Start FileMan'
      OnExecute = actStartFileManExecute
    end
    object actStopFileMan: TAction
      Caption = 'S&top FileMan'
      OnExecute = actStopFileManExecute
    end
    object actBackupFile: TAction
      Caption = '&Backup File'
      OnExecute = actBackupFileExecute
    end
    object actInit: TAction
      Caption = 'Init'
      OnExecute = actInitExecute
    end
    object actExit: TAction
      Caption = 'E&xit'
      OnExecute = actExitExecute
    end
    object actClearLog: TAction
      Caption = '&Clear Log'
      OnExecute = actClearLogExecute
    end
    object actAbout: TAction
      Caption = '&About'
      OnExecute = actAboutExecute
    end
    object actSendFile: TAction
      Caption = 'Send &File'
      OnExecute = actSendFileExecute
    end
    object actAddFolder: TAction
      Caption = '&Add Folder'
      OnExecute = actAddFolderExecute
    end
    object actDeleteFolder: TAction
      Caption = '&Delete Folder'
      OnExecute = actDeleteFolderExecute
    end
  end
  object ClientDataSet: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 552
    Top = 424
  end
  object DataSource: TDataSource
    DataSet = ClientDataSet
    Left = 624
    Top = 424
  end
  object MainMenu: TMainMenu
    Left = 552
    Top = 200
    object File1: TMenuItem
      Caption = '&File'
      object StartFileMan1: TMenuItem
        Action = actStartFileMan
      end
      object StopFileMan1: TMenuItem
        Action = actStopFileMan
      end
      object ClearLog1: TMenuItem
        Action = actClearLog
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object ool1: TMenuItem
      Caption = '&Tool'
      object SendFile1: TMenuItem
        Action = actAddFolder
      end
      object DeleteFolder1: TMenuItem
        Action = actDeleteFolder
      end
      object SendFile2: TMenuItem
        Action = actSendFile
      end
    end
    object About1: TMenuItem
      Caption = '&Help'
      object About2: TMenuItem
        Action = actAbout
      end
      object ShowCDMALog1: TMenuItem
        Caption = '&Show CDMA Log'
        OnClick = ShowCDMALog1Click
      end
    end
  end
  object FileTimer: TTimer
    Enabled = False
    Interval = 30000
    OnTimer = FileTimerTimer
    Left = 472
    Top = 424
  end
  object JvSelectDirectory: TJvSelectDirectory
    Left = 392
    Top = 424
  end
end
