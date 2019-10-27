unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal;

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
    procedure SetExeName(const Value: String); override;

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

  // TODO : after crete
end;

procedure TGlobal.Finalize;
begin
  if FIsfinalized then
    Exit;

  // Todo :

  inherited;
  FIsfinalized := true;
end;

procedure TGlobal.Initialize;
begin
  if FIsfinalized then
    Exit;
  if FIsInitialized then
    Exit;
  FIsInitialized := true;

  inherited;

  // Todo :
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then
    MyObj := TGlobal.Create;
  Result := MyObj;
end;

procedure TGlobal.SetExeName(const Value: String);
begin
  FExeName := Value;
  FLogName := ChangeFileExt(FExeName, '.log');
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\playIoT\' + ExtractFileName(FLogName);

  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));

  FAppCode := TOption.Obj.AppCode;
  FProjectCode := TOption.Obj.ProjectCode;
  FUseCloudLog := TOption.Obj.UseCloudLog;
  FLogServer := TOption.Obj.LogServer;
end;

end.
