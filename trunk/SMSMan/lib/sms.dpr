library sms;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Classes,
  Windows,
  IDHTTP, Web.HTTPApp, IDGlobal;

{$R *.res}

procedure Debug(msg: string);
begin
  OutputDebugString(PChar('SMS::' + msg));
end;

function SendSMS(ID, Pass, AFrom, ATo, AMsg: PChar; var ErrMsg: PChar)
  : Boolean; stdcall;
var
  IDHTTP: TIdHTTP;
  Param: TStringList;
begin
  result := False;

  IDHTTP := TIdHTTP.Create(nil);
  try
    with IDHTTP do
    begin
      Param := TStringList.Create;
      Param.Values['stran_phone'] := ATo;
      Param.Values['stran_callback'] := AFrom;
      Param.Values['guest_no'] := ID;
      Param.Values['guest_key'] := Pass;
      Param.Values['stran_msg'] := HTTPEncode(AnsiString(AMsg));

      ErrMsg := PChar
        (IDHTTP.Post('http://sms.direct.co.kr/link/send.php?', Param));

      Debug(Param.CommaText + ', ' + ErrMsg);

      Param.Free;

      result := Pos('OK', ErrMsg) > 0;
    end;
  finally
    IDHTTP.Free;
  end;
end;

exports SendSMS;

begin

end.
