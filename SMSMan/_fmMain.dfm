object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'fmMain'
  ClientHeight = 246
  ClientWidth = 474
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RESTClient: TRESTClient
    BaseURL = 'https://directsend.co.kr/'
    Params = <>
    Left = 56
    Top = 48
  end
  object RESTRequest: TRESTRequest
    Client = RESTClient
    Params = <>
    Resource = 'index.php/api_v2/sms_change_word2'
    Response = RESTResponse
    SynchronizedEvents = False
    Left = 136
    Top = 48
  end
  object RESTResponse: TRESTResponse
    Left = 224
    Top = 48
  end
end
