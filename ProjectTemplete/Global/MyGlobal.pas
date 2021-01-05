unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, MyCommon, JdcLogging;

const
  PROJECT_CODE = 'My-Project';
  APPLICATION_CODE = 'My-Application';
  APPLICATION_TITLE = 'My Form Application Templete';
  COPY_RIGHT_SIGN = 'ⓒ 2020';
  HOME_PAGE_URL = 'http://.com';

type
  TGlobal = class(TGlobalAbstract)
  protected
    procedure OnAfterLoggingEvent(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); override;
  public
    constructor Create; override;
    class function Obj: TGlobal;

    procedure Initialize; override;
    procedure Finalize; override;

    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); override;

  end;

implementation

uses MyOption, JdcView, JdcGlobal.ClassHelper;

var
  MyObj: TGlobal = nil;

  { TGlobal }

procedure TGlobal.ApplicationMessage(const AType: TMessageType; const ATitle, AMessage: String);
begin
  // 사용자 프로그램의 경우 종료 후 오류 메시지는 출력하지 않는다.
{$IFDEF RELEASE}
  if FIsFinalized then
    Exit;
{$ENDIF}
  TLogging.Obj.ApplicationMessage(AType, ATitle, AMessage);
end;

constructor TGlobal.Create;
begin
  inherited;

  FExeName := ParamStr(0);
  FAppCode := TOption.Obj.AppCode;
  FProjectCode := PROJECT_CODE;

  TLogging.Obj.Init(Self, TOption.Obj);
  // TOTO : after create
end;

procedure TGlobal.Finalize;
begin
  if FIsFinalized then
    Exit;

  inherited;

  // Todo :
  FIsFinalized := true;
end;

procedure TGlobal.Initialize;
begin
  if FIsFinalized then
    Exit;
  if FIsInitialized then
    Exit;

  inherited;

  // Todo :
  FIsInitialized := true;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then
    MyObj := TGlobal.Create;
  result := MyObj;
end;

procedure TGlobal.OnAfterLoggingEvent(const AType: TMessageType; const ATitle, AMessage: String);
begin
  //
end;

initialization

MyObj := TGlobal.Create;

end.
