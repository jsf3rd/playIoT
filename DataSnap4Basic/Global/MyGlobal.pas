unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, JdcLogging;

const
  PROJECT_CODE = 'DACO Project';
  APPLICATION_CODE = 'DACO Application';
  APPLICATION_TITLE = 'DataSnap for Basic';
  APPLICATION_VERSION = 'v1.0';
  COPY_RIGHT_SIGN = 'ⓒ 2020 DACO';
  HOME_PAGE_URL = 'http://www.i-daco.com';

  DB_DRIVER_ID = 'DriverID';
  DB_HOST = 'Server';
  DB_NAME = 'DataBase';
  DB_USER_NAME = 'User_Name';
  DB_PASSWORD = 'Password';
  DB_PORT = 'Port';

  CHANNEL_DEFAULT = 'CH_DataSnap';

type
  TGlobal = class(TGlobalAbstract)
  strict protected
    procedure OnAfterLoggingEvent(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); override;
  public
    constructor Create; override;
    class function Obj: TGlobal;

    procedure Initialize; override;
    procedure Finalize; override;
  end;

implementation

uses MyOption;

var
  MyObj: TGlobal = nil;

  { TGlobal }

constructor TGlobal.Create;
begin
  inherited;

  FExeName := ParamStr(0);
  FProjectCode := PROJECT_CODE;
  FAppCode := TOption.Obj.AppCode;

  TLogging.Obj.Init(Self, TOption.Obj);
  // TODO : after crete
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

  // TODO
  FIsInitialized := true;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then
    MyObj := TGlobal.Create;
  Result := MyObj;
end;

procedure TGlobal.OnAfterLoggingEvent(const AType: TMessageType; const ATitle, AMessage: String);
begin
  // TODO..
end;

initialization

MyObj := TGlobal.Create;

end.
