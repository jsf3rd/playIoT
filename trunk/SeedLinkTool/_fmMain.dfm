object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'SeedLink Tool'
  ClientHeight = 412
  ClientWidth = 684
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 684
    Height = 137
    Align = alTop
    TabOrder = 0
    object edtHost: TLabeledEdit
      Left = 16
      Top = 27
      Width = 129
      Height = 21
      EditLabel.Width = 22
      EditLabel.Height = 13
      EditLabel.Caption = 'Host'
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 0
      Text = 'geofon.gfz-potsdam.de'
    end
    object edtPort: TLabeledEdit
      Left = 159
      Top = 27
      Width = 58
      Height = 21
      EditLabel.Width = 20
      EditLabel.Height = 13
      EditLabel.Caption = 'Port'
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 1
      Text = '18000'
    end
    object edtStation: TLabeledEdit
      Left = 327
      Top = 66
      Width = 50
      Height = 21
      EditLabel.Width = 34
      EditLabel.Height = 13
      EditLabel.Caption = 'Station'
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 3
      Text = 'STU'
    end
    object btnConnect: TButton
      Left = 16
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Connect'
      TabOrder = 5
      OnClick = btnConnectClick
    end
    object btnDisconnect: TButton
      Left = 97
      Top = 62
      Width = 75
      Height = 25
      Caption = 'Disconnect'
      TabOrder = 6
      OnClick = btnDisconnectClick
    end
    object btnHello: TButton
      Left = 386
      Top = 23
      Width = 75
      Height = 25
      Caption = '1. HELLO'
      TabOrder = 8
      OnClick = btnHelloClick
    end
    object btnData: TButton
      Left = 473
      Top = 100
      Width = 75
      Height = 25
      Caption = '4. DATA'
      TabOrder = 11
      OnClick = btnDataClick
    end
    object bntEND: TButton
      Left = 568
      Top = 100
      Width = 75
      Height = 25
      Caption = '5. END'
      TabOrder = 12
      OnClick = bntENDClick
    end
    object edtNetwork: TLabeledEdit
      Left = 247
      Top = 66
      Width = 50
      Height = 21
      EditLabel.Width = 68
      EditLabel.Height = 13
      EditLabel.Caption = 'Network Code'
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 2
      Text = 'GE'
    end
    object edtChannel: TLabeledEdit
      Left = 247
      Top = 105
      Width = 50
      Height = 21
      EditLabel.Width = 67
      EditLabel.Height = 13
      EditLabel.Caption = 'Channel Code'
      ImeName = 'Microsoft Office IME 2007'
      TabOrder = 4
      Text = 'HH?'
    end
    object btnStation: TButton
      Left = 386
      Top = 62
      Width = 75
      Height = 25
      Caption = '2. STATION'
      TabOrder = 9
      OnClick = btnStationClick
    end
    object btnChannel: TButton
      Left = 385
      Top = 101
      Width = 75
      Height = 25
      Caption = '3. CHANNEL'
      TabOrder = 10
      OnClick = btnChannelClick
    end
    object btnCat: TButton
      Left = 16
      Top = 101
      Width = 75
      Height = 25
      Caption = '0. CAT'
      TabOrder = 7
      OnClick = btnCatClick
    end
  end
  object mmLog: TMemo
    Left = 0
    Top = 137
    Width = 684
    Height = 275
    Align = alClient
    ImeName = 'Microsoft Office IME 2007'
    Lines.Strings = (
      'SEEDLink Test Tool.'
      ''
      '(c)ENBGROUP 2015.'
      ''
      'jsf3rd@enbgroup.co.kr'
      ''
      '-----------------------------------------------------'
      '')
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object IdTCPClient: TIdTCPClient
    OnDisconnected = IdTCPClientDisconnected
    OnConnected = IdTCPClientConnected
    ConnectTimeout = 3000
    IPVersion = Id_IPv4
    Port = 0
    ReadTimeout = 1000
    Left = 600
    Top = 152
  end
end
