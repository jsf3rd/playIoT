unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, MyCommon, JdcLogging;

const
  PROJECT_CODE = 'DACO-Project';
  APPLICATION_CODE = 'DACO-Application';
  APPLICATION_TITLE = 'DACO Form Application Templete';
  COPY_RIGHT_SIGN = '¨Ï 2020 DACO';
  HOME_PAGE_URL = 'http://www.i-daco.com';

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
  end;

implementation

uses MyOption, JdcView, JdcGlobal.ClassHelper;

var
  MyObj: TGlobal = nil;

  { TGlobal }

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
  if FIsfinalized then
    Exit;

  inherited;

  // Todo :
  FIsfinalized := true;
end;

procedure TGlobal.Initialize;
begin
  if FIsfinalized then
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
