unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal;

const
  PROJECT_CODE = 'playIoT';
  SERVICE_CODE = 'playIoTSvc';
  SERVICE_NAME = 'playIoT Service Application Templete';
  SERVICE_DESCRIPTION = '여기에 Service Application의 설명을 넣으세요.';

type
  TGlobal = class(TGlobalAbstract)
  protected
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

  FProjectCode := PROJECT_CODE;
  FAppCode := SERVICE_CODE;

  // TODO : after create
end;

destructor TGlobal.Destroy;
begin
  // TODO : before Finalize

  inherited;
end;

procedure TGlobal.Finalize;
begin
  if FIsfinalized then
    Exit;
  FIsfinalized := true;

  // Todo :

  inherited;
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
  result := MyObj;
end;

procedure TGlobal.SetExeName(const Value: String);
begin
  FExeName := Value;
  FLogName := ChangeFileExt(FExeName, '.log');

  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));

  FUseCloudLog := TOption.Obj.UseCloudLog;
  // FLogServer.StringValue := LOG_SERVER;
end;

initialization

MyObj := TGlobal.Create;

end.
