unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Option, Global, JsonData,
  JdcGlobal, System.MaskUtils, System.AnsiStrings, math;

function SendSMS(ID, Pass, AFrom, ATo, AMsg: PChar; var ErrMsg: PChar): Boolean; stdcall;
  external 'sms.dll';

type
  TfmMain = class(TForm)
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
        OperationLog('메시지(Message) 송신자(From) 수신자(To) 데이터가 필요합니다.' + NEXT_LINE +
          '아스키코드 ($00) , 수신자 구분(/)');
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
  PrintLog(Path + '\sms_' + FormatDateTime('YYYYMMDD', now) + '.log', AMsg);
end;

procedure TfmMain.ManageMessage(AMessage: TStringList);
var
  ID, Password: String;
  ErrMsg: PChar;
  _SMSUser: string;
  SMSUser: TStringList;
  SMSMessage, AFrom, ATo: string;

  Index: Integer;
  Ascii, Msg: string;
  Dec: Integer;
begin
  SMSMessage := AMessage.strings[0];

  Msg := SMSMessage;
  while pos('$', Msg) > 0 do
  begin
    Index := pos('$', Msg);
    Ascii := Msg.Substring(Index, 2);

    Dec := HexStrToByte(Ascii);
    SMSMessage := SMSMessage.Replace('$' + Ascii, AnsiChar(Dec), [rfReplaceAll]);
    Msg := Msg.Replace('$' + Ascii, '', [rfReplaceAll]);
  end;

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

      if SendSMS(PChar(ID), PChar(Password), PChar(AFrom), PChar(_SMSUser), PChar(SMSMessage),
        ErrMsg) then
        OperationLog('전송완료 - ' + AFrom + ' : ' + _SMSUser + ' - [' + SMSMessage + ']')
      else
        OperationLog('전송실패 - ' + AFrom + ' : ' + _SMSUser + ' - [' + SMSMessage + ']');
    end;
  finally
    SMSUser.free;
  end;
end;

end.
