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
  TGlobal = class(TComponent)
  strict private
    FInitialized: boolean;
  private
    FExeName: String;
    FLogName: string;
    procedure SetExeName(const Value: String);
    procedure _ApplicationMessge(AType, ATitle, AMessage: String;
      AOutputs: TMsgOutputs = [moDebugView, moLogFile, moCloudMessage]);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure ApplicationMessge(AType: TMessageType; ATitle: String;
      AMessage: String = ''); overload;
    procedure ApplicationMessge(AType: TMessageType; ATitle: String;
      AFormat: String; const Args: array of const); overload;

    procedure Initialize;
    procedure Finalize;
  published
    property Initialized: boolean read FInitialized;
    property ExeName: String read FExeName write SetExeName;
    property LogName: string read FLogName;
  end;

implementation

uses MyOption;

var
  MyObj: TGlobal = nil;

  { TGlobal }

procedure TGlobal.ApplicationMessge(AType: TMessageType;
  ATitle, AMessage: String);
begin
  case AType of
    mtDebug:
      _ApplicationMessge(MESSAGE_TYPE_DEBUG, ATitle, AMessage, [moDebugView]);
    mtLog:
      _ApplicationMessge(MESSAGE_TYPE_LOG, ATitle, AMessage);
    mtError:
      _ApplicationMessge(MESSAGE_TYPE_ERROR, ATitle, AMessage);
    mtWarning:
      _ApplicationMessge(MESSAGE_TYPE_WRANING, ATitle, AMessage);
  else
    _ApplicationMessge(MESSAGE_TYPE_UNKNOWN, ATitle, AMessage);
  end;
end;

procedure TGlobal.ApplicationMessge(AType: TMessageType;
  ATitle, AFormat: String; const Args: array of const);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessge(AType, ATitle, str);

end;

constructor TGlobal.Create(AOwner: TComponent);
begin
  inherited;

  FExeName := '';
  FInitialized := False;
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
  FInitialized := False;

end;

procedure TGlobal.Initialize;
begin
  if FInitialized then
    Exit;

  // Todo :

  FInitialized := True;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then
    MyObj := TGlobal.Create(nil);
  result := MyObj;
end;

procedure TGlobal.SetExeName(const Value: String);
begin
  FExeName := Value;
  FLogName := ChangeFileExt(FExeName, '.log');

  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));
end;

procedure TGlobal._ApplicationMessge(AType, ATitle, AMessage: String;
  AOutputs: TMsgOutputs);
begin
  if moDebugView in AOutputs then
    PrintDebug('<' + AType + '> [' + SERVICE_CODE + '] ' + ATitle + ' - ' +
      AMessage);

  if moLogFile in AOutputs then
    PrintLog(FLogName, '<' + AType + '> ' + ATitle + ' - ' + AMessage);

  if moCloudMessage in AOutputs then
    CloudMessage(PROJECT_CODE, SERVICE_CODE, AType, ATitle, AMessage);
end;

end.
