unit MyGlobal;

interface

uses
  Classes, SysUtils, IOUtils, JdcGlobal, MyCommon;

const
  APPLICATION_CODE = 'playIoT Application';
  APPLICATION_TITLE = 'palyIoT Form Application Templete';
  APPLICATION_VERSION = 'v1.0';
  COPY_RIGHT_SIGN = '¨Ï 2016 playIoT';
  HOME_PAGE_URL = 'http://www.playIoT.biz';

type
  TGlobal = class(TGlobalAbstract)
  strict protected
    procedure SetExeName(const Value: String); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure ApplicationMessage(const AType: TMessageType;
      const ATitle: String; const AMessage: String = ''); override;

    procedure Initialize; override;
    procedure Finalize; override;
  end;

implementation

uses MyOption, JdcView, JdcGlobal.ClassHelper;

var
  MyObj: TGlobal = nil;

  { TGlobal }

procedure TGlobal.ApplicationMessage(const AType: TMessageType;
  const ATitle: String; const AMessage: String);
begin
  inherited;

  case AType of
    mtDebug:
      _ApplicationMessage(MESSAGE_TYPE_DEBUG, ATitle, AMessage,
        [moCloudMessage]);
    mtError:
      TView.Obj.sp_ErrorMessage(ATitle, AMessage);
    mtInfo:
      TView.Obj.sp_LogMessage(ATitle, AMessage);
  end;
end;

constructor TGlobal.Create;
begin
  inherited;

  FProjectCode := PROJECT_CODE;
  FAppCode := APPLICATION_CODE;

  // TOTO : after create
end;

destructor TGlobal.Destroy;
begin

  // TOTO : before Finalize

  inherited;
end;

procedure TGlobal.Finalize;
begin
  if FIsfinalized then
    Exit;
  FIsfinalized := true;

  ApplicationMessage(mtDebug, 'Stop', 'StartTime=' + FStartTime.ToString);
end;

procedure TGlobal.Initialize;
begin
  if FIsfinalized then
    Exit;
  if FIsInitialized then
    Exit;
  FIsInitialized := true;

  FStartTime := now;

{$IFDEF WIN32}
  ApplicationMessage(mtDebug, 'Start', '(x86)' + FExeName);
{$ENDIF}
{$IFDEF WIN64}
  ApplicationMessage(mtDebug, 'Start', '(x64)' + FxeName);
{$ENDIF}
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
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\palyIoT\' +
    ExtractFileName(FLogName);

  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));
end;

initialization

MyObj := TGlobal.Create;

end.
