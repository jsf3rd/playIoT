unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal;

const
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
    destructor Destroy; override;

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

destructor TGlobal.Destroy;
begin
  // TODO : before Finalize

  inherited;
end;

procedure TGlobal.Finalize;
begin
  if not FInitialized then
    Exit;
  FInitialized := false;

end;

procedure TGlobal.Initialize;
begin
  if FInitialized then
    Exit;

  // Todo :

  FInitialized := true;
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
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\playIoT\' +
    ExtractFileName(FLogName);

  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));
end;

end.
