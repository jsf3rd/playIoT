unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Option, Global, JdcView2, ValueList,
  JdcGlobal, System.MaskUtils, System.AnsiStrings, math;

function SendSMS(ID, Pass, AFrom, ATo, AMsg: PChar; var ErrMsg: PChar): Boolean;
  stdcall; external 'sms.dll';

type
  TfmMain = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    procedure ManageMessage(AMessage: TStringList);
  public
    { Public declarations }
  published
    procedure rp_OperationLog(APacket: TValueList);
    procedure rp_ErrorLog(APacket: TValueList);
    procedure rp_ShowMessage(APacket: TValueList);
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
  tmp: string;
  RawMessage: TStringList;
begin
  try
    tmp := '';
    TView.Obj.Add(Self);
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
        tmp := tmp + ParamStr(I);
      end;
      RawMessage.CommaText := tmp;
      ManageMessage(RawMessage);
    except
      on E: Exception do
        TView.Obj.sp_ShowMessage('메시지(Message), 송신자(From), 수신자(To) 데이터가 필요합니다.'
          + NEXT_LINE + '공백 (_),  줄바꿈 (@), 수신자 구분(/)');
    end;

  finally
    RawMessage.free;
    Application.Terminate;

  end;
end;

procedure TfmMain.rp_OperationLog(APacket: TValueList);
var
  Path: string;
begin
  Path := ExtractFilePath(TGlobal.Obj.ExeName) + 'Logs';
  PrintLog(Path + '\sms_' + FormatDateTime('YYYYMMDD', now) + '.log',
    APacket.Values['Msg']);
end;

procedure TfmMain.ManageMessage(AMessage: TStringList);
var
  ID, Password: String;
  ErrMsg: PChar;
  _SMSUser: string;
  SMSUser: TStringList;
  SMSMessage, AFrom, ATo: string;
begin
  SMSMessage := AMessage.strings[0];
  SMSMessage := SMSMessage.Replace('_', ' ', [rfReplaceAll]);
  SMSMessage := SMSMessage.Replace('@', NEXT_LINE, [rfReplaceAll]);

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

      if SendSMS(PChar(ID), PChar(Password), PChar(AFrom), PChar(_SMSUser),
        PChar(SMSMessage), ErrMsg) then
        TView.Obj.sp_ShowMessage('전송완료 - ' + AFrom + ' : ' + _SMSUser + '-' +
          SMSMessage)
      else
        TView.Obj.sp_ShowMessage('전송실패 - ' + AFrom + ' : ' + _SMSUser + '-' +
          SMSMessage);
    end;

  finally
    SMSUser.free;
  end;
end;

procedure TfmMain.rp_ErrorLog(APacket: TValueList);
var
  Path: string;
begin
  Path := ExtractFilePath(TGlobal.Obj.ExeName) + 'Logs';
  PrintLog(Path + '\erp_' + FormatDateTime('YYYYMMDD', now) + '.err',
    APacket.Values['Msg']);
end;

procedure TfmMain.rp_ShowMessage(APacket: TValueList);
begin
  rp_OperationLog(APacket);
end;

end.
