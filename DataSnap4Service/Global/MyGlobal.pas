unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, JdcLogging;

const
  PROJECT_CODE = 'playIoT';
  SERVICE_CODE = 'playIoTSvc';
  SERVICE_NAME = 'playIoT Service Application Templete';
  SERVICE_DESCRIPTION = '여기에 Service Application의 설명을 넣으세요.';

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

  // TODO : after Create
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
  TLogging.Obj.OnAfterLogging := OnAfterLoggingEvent;
  TLogging.Obj.SetLogName(FExeName);
end;

initialization

MyObj := TGlobal.Create;

end.
