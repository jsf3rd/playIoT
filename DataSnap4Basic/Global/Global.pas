unit Global;

interface

uses
  Classes, SysUtils, JdcView2, JdcGlobal, DBXJSON;

const
  APPLICATION_TITLE = 'DataSnap for Basic';
  APPLICATION_VERSION = 'v1.0';
  COPY_RIGHT_SIGN = 'ⓒ 2015 ENBGROUP';
  HOME_PAGE_URL = 'http://www.enbgourp.co.kr';

  DB_DRIVER_ID = 'DriverID';
  DB_HOST = 'Server';
  DB_NAME = 'DataBase';
  DB_USER_NAME = 'User_Name';
  DB_PASSWORD = 'Password';
  DB_PORT = 'Port';

  CHANNEL_DEFAULT = 'CH_DataSnap';

type
  TGlobal = class(TComponent)
  strict private
    FInitialized: boolean;
  private
    FExeName: String;
    FLogName: string;
    procedure SetExeName(const Value: String);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure Initialize;
    procedure Finalize;

  published
    property Initialized: boolean read FInitialized;
    property ExeName: String read FExeName write SetExeName;
    property LogName: string read FLogName;
  end;

implementation

uses Option;

var
  MyObj: TGlobal = nil;

  { TGlobal }

constructor TGlobal.Create(AOwner: TComponent);
begin
  inherited;

  FExeName := '';
  FInitialized := false;
end;

destructor TGlobal.Destroy;
begin
  Finalize;

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
    MyObj := TGlobal.Create(nil);
  Result := MyObj;
end;

procedure TGlobal.SetExeName(const Value: String);
begin
  FExeName := Value;
  FLogName := ChangeFileExt(FExeName, '.log');
end;

end.
