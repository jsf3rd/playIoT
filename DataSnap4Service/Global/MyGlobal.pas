unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, JdcLogging;

const
  PROJECT_CODE = 'MyProject';
  SERVICE_CODE = 'DataSnapSvc';
  SERVICE_NAME = 'DataSnap Service Templete';
  SERVICE_DESCRIPTION = '여기에 DataSnap Service Application의 설명을 넣으세요.';

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

initialization

MyObj := TGlobal.Create;

end.
