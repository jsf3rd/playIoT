object fmOption: TfmOption
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 239
  ClientWidth = 417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnConfirm: TButton
    Left = 119
    Top = 200
    Width = 75
    Height = 25
    Caption = #54869#51064
    TabOrder = 0
    OnClick = btnConfirmClick
  end
  object btnCancle: TButton
    Left = 223
    Top = 200
    Width = 75
    Height = 25
    Cancel = True
    Caption = #52712#49548
    TabOrder = 1
    OnClick = btnCancleClick
  end
  object RzGroupBox1: TGroupBox
    Left = 14
    Top = 8
    Width = 195
    Height = 177
    Caption = 'DB '#50672#44208
    Color = clBtnFace
    ParentColor = False
    TabOrder = 2
    object RzLabel1: TLabel
      Left = 10
      Top = 32
      Width = 22
      Height = 13
      Caption = 'Host'
      Transparent = False
    end
    object RzLabel2: TLabel
      Left = 10
      Top = 87
      Width = 46
      Height = 13
      Caption = 'DataBase'
      Transparent = False
    end
    object RzLabel3: TLabel
      Left = 10
      Top = 115
      Width = 22
      Height = 13
      Caption = 'User'
      Transparent = False
    end
    object RzLabel4: TLabel
      Left = 10
      Top = 143
      Width = 46
      Height = 13
      Caption = 'Password'
      Transparent = False
    end
    object RzLabel8: TLabel
      Left = 10
      Top = 59
      Width = 20
      Height = 13
      Caption = 'Port'
      Transparent = False
    end
    object edtHost: TEdit
      Left = 62
      Top = 29
      Width = 120
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 0
    end
    object edtDataBase: TEdit
      Left = 62
      Top = 84
      Width = 120
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 2
    end
    object edtUser: TEdit
      Left = 62
      Top = 112
      Width = 120
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 3
    end
    object edtPass: TEdit
      Left = 62
      Top = 140
      Width = 120
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      PasswordChar = '*'
      TabOrder = 4
    end
    object edtDBPort: TEdit
      Left = 62
      Top = 56
      Width = 120
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      NumbersOnly = True
      TabOrder = 1
    end
  end
  object RzGroupBox3: TGroupBox
    Left = 215
    Top = 8
    Width = 187
    Height = 177
    Caption = 'DataSnap'
    Color = clBtnFace
    ParentColor = False
    TabOrder = 3
    object RzLabel6: TLabel
      Left = 10
      Top = 29
      Width = 46
      Height = 13
      Caption = 'Port(211)'
      Transparent = False
    end
    object edtDSPort: TEdit
      Left = 62
      Top = 25
      Width = 120
      Height = 21
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 0
    end
  end
end
