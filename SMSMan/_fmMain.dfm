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
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object RESTClient: TRESTClient
    BaseURL = 'https://directsend.co.kr'
    Params = <>
    SynchronizedEvents = False
    Left = 56
    Top = 48
  end
  object RESTRequest: TRESTRequest
    Client = RESTClient
    Params = <>
    Resource = 'index.php/api_v2/sms_change_word'
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
