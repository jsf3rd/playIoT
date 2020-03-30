unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, JdcLogging;

const
  PROJECT_CODE = 'DACO';
  SERVICE_CODE = 'ServiceTempleteSvc';
  SERVICE_NAME = 'DACO Service Templete';
  SERVICE_DESCRIPTION = '여기에 Service Templete의 설명을 넣으세요.';

type
  TGlobal = class(TGlobalAbstract)
  protected
    procedure OnAfterLoggingEvent(const AType: TMessageType; const ATitle, AMessage: String); override;
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
  FAppCode := TOption.Obj.AppCode;
  FProjectCode := PROJECT_CODE;

  TLogging.Obj.Init(Self, TOption.Obj);
  // TODO : after create
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
