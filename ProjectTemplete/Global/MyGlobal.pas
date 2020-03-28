unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, MyCommon, JdcLogging;

const
  PROJECT_CODE = 'MyProject';
  APPLICATION_CODE = 'playIoT Application';
  APPLICATION_TITLE = 'playIoT Form Application Templete';
  COPY_RIGHT_SIGN = '¨Ï 2018 playIoT';
  HOME_PAGE_URL = 'http://www.playIoT.biz';

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

uses MyOption, JdcView, JdcGlobal.ClassHelper;

var
  MyObj: TGlobal = nil;

  { TGlobal }

constructor TGlobal.Create;
begin
  inherited;

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

initialization

MyObj := TGlobal.Create;

end.
