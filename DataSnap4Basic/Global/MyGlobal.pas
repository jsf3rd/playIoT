unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, JdcLogging;

const
  PROJECT_CODE = 'MyProject';
  APPLICATION_CODE = 'playIoT Application';
  APPLICATION_TITLE = 'DataSnap for Basic';
  APPLICATION_VERSION = 'v1.0';
  COPY_RIGHT_SIGN = 'ⓒ 2016 playIoT';
  HOME_PAGE_URL = 'http://www.playIoT.biz';

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
    constructor Create; override;
    procedure SetExeName(const Value: String); override;
    procedure OnAfterLoggingEvent(const AType: TMessageType; const ATitle: String;
      const AMessage: String = '');
  public
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

procedure TGlobal.SetExeName(const Value: String);
begin
  FExeName := Value;
  FAppCode := TOption.Obj.AppCode;
  FProjectCode := TOption.Obj.ProjectCode;

  TLogging.Obj.ProjectCode := FProjectCode;
  TLogging.Obj.AppCode := FAppCode;
  TLogging.Obj.UseDebug := TOption.Obj.UseDebug;
  TLogging.Obj.UseCloudLog := TOption.Obj.UseCloudLog;
  TLogging.Obj.LogServer := TOption.Obj.LogServer;
  TLogging.Obj.SetLogName(FExeName);
  TLogging.Obj.OnAfterLogging := OnAfterLoggingEvent;
end;

end.
