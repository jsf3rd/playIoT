unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Option, Global, JsonData,
  JdcGlobal, System.MaskUtils, System.AnsiStrings, math, JdcLogging, REST.Types, Data.Bind.Components,
  Data.Bind.ObjectScope, REST.Client, System.JSON, REST.JSON, JdcGlobal.ClassHelper;

type
  TfmMain = class(TForm)
    RESTClient: TRESTClient;
    RESTRequest: TRESTRequest;
    RESTResponse: TRESTResponse;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure ManageMessage(AMessage: TStringList);
    procedure OperationLog(AMsg: string);
  public
    //
  published

  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  fmMain.Close;
end;

procedure TfmMain.FormCreate(Sender: TObject);
var
  I: Integer;
  RawMessage: TStringList;
begin
  try
    TGlobal.Obj.ExeName := Application.ExeName;
    RawMessage := TStringList.create;

    if TOption.Obj.ReadID['CENTER'] = '' then
    begin
      TOption.Obj.ReadID['CENTER'] := '';
      TOption.Obj.ReadPassword['CENTER'] := '';
    end;

    try
      for I := 1 to ParamCount do
      begin
        RawMessage.Add(ParamStr(I));
      end;
      ManageMessage(RawMessage);
    except
      on E: Exception do
        OperationLog('메시지(Message) 송신자(From) 수신자(To) 데이터가 필요합니다.' + NEXT_LINE + '메시지 기능 (@,_) , 수신자 구분(/),E='
          + E.Message);
    end;

    RawMessage.free;
  finally
    Application.Terminate;
  end;
end;

procedure TfmMain.OperationLog(AMsg: string);
var
  Path: string;
begin
  Path := ExtractFilePath(TGlobal.Obj.ExeName) + 'logs';
  PrintLog(Path + '\sms_' + FormatDateTime('YYYYMMDD', now) + '.log', now, AMsg);
end;

procedure TfmMain.ManageMessage(AMessage: TStringList);
var
  ID, Password: String;
  _SMSUser: string;
  SMSUser: TStringList;
  SMSMessage, AFrom, ATo, Msg: string;

  Response: TJSONObject;
  // Index: Integer;
  // Ascii: string;
  // Dec: Integer;

  function GetBody(mobile: string): TJSONObject;
  var
    container: TJSONArray;
    receiver: TJSONObject;
    list: TArray<String>;
  begin
    container := TJSONArray.create;

    list := ATo.Split([',']);

    receiver := TJSONObject.create;
    receiver.AddPair('name', 'name');
    receiver.AddPair('mobile', mobile);
    container.AddElement(receiver);

    result := TJSONObject.create;
    result.AddPair('title', 'title');
    result.AddPair('sender', AFrom);
    result.AddPair('receiver', container);
    result.AddPair('message', Msg);
    result.AddPair('username', ID);
    result.AddPair('key', Password);
  end;

begin
  SMSMessage := AMessage.strings[0];

  Msg := SMSMessage;
  Msg := Msg.Replace('_', ' ', [rfReplaceAll]);
  Msg := Msg.Replace('@', NEXT_LINE, [rfReplaceAll]);
  {
    while pos('$', Msg) > 0 do
    begin
    Index := pos('$', Msg);
    Ascii := Msg.Substring(Index, 2);

    Dec := HexStrToByte(Ascii);
    SMSMessage := SMSMessage.Replace('$' + Ascii, AnsiChar(Dec), [rfReplaceAll]);
    Msg := Msg.Replace('$' + Ascii, '', [rfReplaceAll]);
    end;
  }
  AFrom := AMessage.strings[1];

  ATo := AMessage.strings[2];
  ATo := ATo.Replace('/', ',', [rfReplaceAll]);

  ID := TOption.Obj.ReadID['CENTER'];
  Password := TOption.Obj.ReadPassword['CENTER'];

  SMSUser := TStringList.create;
  SMSUser.CommaText := ATo;
  try
    for _SMSUser in SMSUser do
    begin
      if _SMSUser = '' then
        Exit;

      RESTRequest.ClearBody;
      RESTRequest.Body.Add(GetBody(_SMSUser));
      RESTRequest.Execute;

      if RESTResponse.StatusCode = 200 then
      begin
        Response := TJSONObject.ParseJSONValue(RESTResponse.Content) as TJSONObject;

        if Response.GetString('status') = '0' then
          OperationLog(Format('전송완료 - From=%s,To=%s,Msg=%s', [AFrom, _SMSUser, SMSMessage]))
        else
          OperationLog(Format('전송실패- ErrorCode=%s,ErrorMsg=%s', [Response.GetString('status'),
            Response.GetString('msg')]));
      end
      else
      begin
        OperationLog(Format('전송실패- StatusCode=%d', [RESTResponse.StatusCode]));
      end;
    end;
  finally
    SMSUser.free;
  end;
end;

end.
