object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'QSCD Server'
  ClientHeight = 775
  ClientWidth = 958
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  Position = poScreenCenter
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 958
    Height = 65
    Align = alTop
    TabOrder = 0
    ExplicitWidth = 959
    object Label2: TLabel
      Left = 287
      Top = 16
      Width = 55
      Height = 13
      Caption = #45936#51060#53552#49440#53469
    end
    object edtUDPPort: TLabeledEdit
      Left = 17
      Top = 31
      Width = 90
      Height = 21
      EditLabel.Width = 81
      EditLabel.Height = 13
      EditLabel.Caption = 'MMA/S '#49688#49888' Port'
      NumbersOnly = True
      TabOrder = 0
      Text = '5002'
    end
    object cmbData: TComboBox
      Left = 287
      Top = 31
      Width = 219
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 1
      Text = 'Total PGA'
      Items.Strings = (
        'Total PGA'
        'Horizental PGA'
        'Windowed MMA'
        'True MMA'
        'MEC(Maximum Value of Each Channel)')
    end
    object btnExecute: TButton
      Left = 544
      Top = 27
      Width = 105
      Height = 25
      Action = actExecute
      TabOrder = 2
    end
    object edtKeyCode: TLabeledEdit
      Left = 142
      Top = 31
      Width = 90
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = #53076#46300
      TabOrder = 3
      Text = 'SLKMB'
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 65
    Width = 958
    Height = 710
    Align = alClient
    Caption = 'Panel2'
    TabOrder = 1
    ExplicitLeft = 656
    ExplicitTop = 16
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Chart3: TChart
      Left = 1
      Top = 473
      Width = 956
      Height = 236
      Legend.Visible = False
      Title.Color = clLime
      Title.Cursor = crHandPoint
      Title.Font.Color = clLime
      Title.Font.Height = -16
      Title.Font.Style = [fsBold]
      Title.Text.Strings = (
        'Up-Down')
      BottomAxis.Axis.Color = clGreen
      BottomAxis.DateTimeFormat = 'hh:mm:ss'
      BottomAxis.Grid.Color = clGreen
      BottomAxis.GridCentered = True
      BottomAxis.LabelsFormat.Font.Color = clGreen
      BottomAxis.LabelsFormat.Font.Height = -13
      BottomAxis.MinorTicks.Color = clGreen
      BottomAxis.Ticks.Color = clGreen
      BottomAxis.TicksInner.Color = clGreen
      LeftAxis.Axis.Color = clGreen
      LeftAxis.Grid.Color = clGreen
      LeftAxis.GridCentered = True
      LeftAxis.LabelsFormat.Font.Color = clGreen
      LeftAxis.LabelsFormat.Font.Height = -13
      LeftAxis.MinorTicks.Color = clGreen
      LeftAxis.Ticks.Color = clGreen
      LeftAxis.TicksInner.Color = clGreen
      LeftAxis.Title.Caption = 'gal'
      LeftAxis.Title.Font.Color = clLime
      LeftAxis.Title.Font.Height = -16
      LeftAxis.Title.Font.Style = [fsBold]
      View3D = False
      Align = alBottom
      Color = clBlack
      TabOrder = 0
      ExplicitTop = 479
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 16
      object LineSeries3: TLineSeries
        SeriesColor = clLime
        Brush.BackColor = clDefault
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        XValues.DateTime = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object Chart2: TChart
      Left = 1
      Top = 237
      Width = 956
      Height = 236
      Legend.Visible = False
      Title.Color = clLime
      Title.Cursor = crHandPoint
      Title.Font.Color = clLime
      Title.Font.Height = -16
      Title.Font.Style = [fsBold]
      Title.Text.Strings = (
        'East-West')
      BottomAxis.Axis.Color = clGreen
      BottomAxis.DateTimeFormat = 'hh:mm:ss'
      BottomAxis.Grid.Color = clGreen
      BottomAxis.GridCentered = True
      BottomAxis.LabelsFormat.Font.Color = clGreen
      BottomAxis.LabelsFormat.Font.Height = -13
      BottomAxis.MinorTicks.Color = clGreen
      BottomAxis.Ticks.Color = clGreen
      BottomAxis.TicksInner.Color = clGreen
      LeftAxis.Axis.Color = clGreen
      LeftAxis.Grid.Color = clGreen
      LeftAxis.GridCentered = True
      LeftAxis.LabelsFormat.Font.Color = clGreen
      LeftAxis.LabelsFormat.Font.Height = -13
      LeftAxis.MinorTicks.Color = clGreen
      LeftAxis.Ticks.Color = clGreen
      LeftAxis.TicksInner.Color = clGreen
      LeftAxis.Title.Caption = 'gal'
      LeftAxis.Title.Font.Color = clLime
      LeftAxis.Title.Font.Height = -16
      LeftAxis.Title.Font.Style = [fsBold]
      View3D = False
      Align = alClient
      Color = clBlack
      TabOrder = 1
      ExplicitLeft = 2
      ExplicitTop = 231
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 16
      object LineSeries1: TLineSeries
        SeriesColor = clLime
        Brush.BackColor = clDefault
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        XValues.DateTime = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object Chart1: TChart
      Left = 1
      Top = 1
      Width = 956
      Height = 236
      Legend.Visible = False
      Title.Color = clLime
      Title.Cursor = crHandPoint
      Title.Font.Color = clLime
      Title.Font.Height = -16
      Title.Font.Style = [fsBold]
      Title.Text.Strings = (
        'North-South')
      BottomAxis.Axis.Color = clGreen
      BottomAxis.DateTimeFormat = 'hh:mm:ss'
      BottomAxis.Grid.Color = clGreen
      BottomAxis.GridCentered = True
      BottomAxis.LabelsFormat.Font.Color = clGreen
      BottomAxis.LabelsFormat.Font.Height = -13
      BottomAxis.MinorTicks.Color = clGreen
      BottomAxis.Ticks.Color = clGreen
      BottomAxis.TicksInner.Color = clGreen
      LeftAxis.Axis.Color = clGreen
      LeftAxis.Grid.Color = clGreen
      LeftAxis.GridCentered = True
      LeftAxis.LabelsFormat.Font.Color = clGreen
      LeftAxis.LabelsFormat.Font.Height = -13
      LeftAxis.MinorTicks.Color = clGreen
      LeftAxis.Ticks.Color = clGreen
      LeftAxis.TicksInner.Color = clGreen
      LeftAxis.Title.Caption = 'gal'
      LeftAxis.Title.Font.Color = clLime
      LeftAxis.Title.Font.Height = -16
      LeftAxis.Title.Font.Style = [fsBold]
      View3D = False
      Align = alTop
      Color = clBlack
      TabOrder = 2
      ExplicitWidth = 957
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 16
      object LineSeries2: TLineSeries
        SeriesColor = clLime
        Brush.BackColor = clDefault
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        XValues.DateTime = True
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
  end
  object MainMenu: TMainMenu
    Left = 760
    Top = 88
    object File1: TMenuItem
      Caption = '&File'
      object Exit1: TMenuItem
        Action = actExit
      end
    end
    object Help1: TMenuItem
      Caption = '&Help'
      object About1: TMenuItem
        Caption = '&About'
        OnClick = About1Click
      end
    end
    object MenuTest: TMenuItem
      Caption = 'T&est'
      Visible = False
    end
  end
  object ApplicationEvents: TApplicationEvents
    Left = 840
    Top = 88
  end
  object ActionList: TActionList
    Left = 696
    Top = 88
    object actAbout: TAction
      Caption = '&About'
    end
    object actClearLog: TAction
      Caption = '&Clear Log'
      ShortCut = 16472
    end
    object actExit: TAction
      Caption = '&Exit'
      ShortCut = 16465
      OnExecute = actExitExecute
    end
    object actShowIni: TAction
      Caption = 'Show &IniFile'
    end
    object actShowLog: TAction
      Caption = 'Show &Log'
    end
    object actTestMenu: TAction
      Caption = '&Test&Menu'
      ShortCut = 16456
      OnExecute = actTestMenuExecute
    end
    object actExecute: TAction
      Caption = #49884#51089
      OnExecute = actExecuteExecute
    end
  end
  object IdUDPServer: TIdUDPServer
    Bindings = <>
    DefaultPort = 0
    OnUDPRead = IdUDPServerUDPRead
    Left = 728
    Top = 8
  end
end
