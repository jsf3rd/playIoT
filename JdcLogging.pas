// *******************************************************
//
// DACO Logging Util
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@e-daco.net
//
// Define : LOCALAPPDATA, LOGGING_DAY_TAG
//
// *******************************************************

unit JdcLogging;

interface

uses System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, JdcGlobal, JclFileUtils,
  Vcl.StdCtrls, Winapi.Windows;

type
  TLogProc = procedure(const AType: TMessageType; const ATitle: String; const AMessage: String = '')
    of object;

  TMsgOutput = (moDebugView, moLogFile, moCloudMessage);
  TMsgOutputs = set of TMsgOutput;

  TJdcLog = record
    LogName: string;
    Time: TDateTime;
    Msg: string;
    constructor Create(AName: string; ATime: TDateTime; AMsg: string);
    function ToString: string;
  end;

  TLogging = class sealed
  private
    FLogName: string;
    FExeVersion: string;

    FLogTask: TThread;
    FMsgQueue: TQueue<TJdcLog>;
    FLogServer: TConnInfo;
    FProjectCode: string;
    FAppCode: String;

    FUseDebug: Boolean;
    FUseCloudLog: Boolean;
    FOnAfterLogging: TLogProc;

    constructor Create;
    procedure FlushLog;

    procedure AppendLog(const AName: string; const AMsg: string);
    procedure _ApplicationMessage(const AType: string; const ATitle: string; const AMessage: String;
      const AOutputs: TMsgOutputs = [moDebugView, moLogFile, moCloudMessage]);

    function GetLogName: String;
    procedure SetUseCloudLog(const Value: Boolean);
    procedure SetUseDebug(const Value: Boolean);
    procedure SetLogServer(const Value: TConnInfo);
  public
    destructor Destroy; override;

    class function Obj: TLogging;

    procedure StartLogging;
    procedure StopLogging;

    procedure SetLogName(const ExeName: string);

    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AMessage: String = '';
      const DebugLog: Boolean = False); overload;
    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AFormat: String;
      const Args: array of const; const DebugLog: Boolean = False); overload;

    property ProjectCode: String read FProjectCode write FProjectCode;
    property AppCode: String read FAppCode write FAppCode;

    property UseDebug: Boolean read FUseDebug write SetUseDebug;
    property UseCloudLog: Boolean read FUseCloudLog write SetUseCloudLog;

    property LogName: String read GetLogName;
    property LogServer: TConnInfo read FLogServer write SetLogServer;

    property OnAfterLogging: TLogProc read FOnAfterLogging write FOnAfterLogging;
  end;

  // ·Î±× Âï±â..
procedure PrintLog(const AFile: string; const ATime: TDateTime; const AMessage: String); overload;
procedure PrintLog(AMemo: TMemo; const AMsg: String); overload;
procedure PrintDebug(const Format: string; const Args: array of const); overload;
procedure PrintDebug(const str: string); overload;

implementation

var
  MyObj: TLogging = nil;

procedure BackupLogFile(AName: string; AMaxSize: Integer = 1024 * 1024 * 5);
begin
  if FileExists(AName) then
  begin
    if JclFileUtils.FileGetSize(AName) > AMaxSize then
    begin
      try
        FileMove(AName, ChangeFileExt(AName, FormatDateTime('_YYYYMMDD_HHNNSS', Now) + '.bak'), True);
      except
        on E: Exception do
          AName := ChangeFileExt(AName, FormatDateTime('_YYYYMMDD', Now) + '.tmp');
      end;
    end;
  end;
end;

procedure PrintLog(AMemo: TMemo; const AMsg: String);
begin
  if AMemo.Lines.Count > 5000 then
    AMemo.Lines.Clear;

  if AMsg.IsEmpty then
    AMemo.Lines.Add('')
  else
  begin
    AMemo.Lines.Add(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', Now) + AMsg);
  end;
end;

procedure PrintLog(const AFile: string; const ATime: TDateTime; const AMessage: String);
var
  Stream: TStreamWriter;
  Time: TDateTime;
begin
  BackupLogFile(AFile);
  try
    if ATime = 0 then
      Time := Now
    else
      Time := ATime;

    Stream := TFile.AppendText(AFile);
    try
      if AMessage.IsEmpty then
        Stream.WriteLine
      else
        Stream.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', Time) + AMessage);
    finally
      FreeAndNil(Stream);
    end;
  except
    on E: Exception do
      PrintDebug(E.Message + ', ' + AMessage);
  end;
end;

procedure PrintDebug(const Format: string; const Args: array of const); overload;
var
  str: string;
begin
  FmtStr(str, Format, Args);
  PrintDebug(str);
end;

procedure PrintDebug(const str: string); overload;
begin
  OutputDebugString(PChar('[JDC] ' + str));
end;

{ TLogging }

procedure TLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AMessage: String;
  const DebugLog: Boolean);
const
  MESSAGE_TYPE_INFO = 'INFO';
  MESSAGE_TYPE_ERROR = 'ERROR';
  MESSAGE_TYPE_DEBUG = 'DEBUG';
  MESSAGE_TYPE_WARNING = 'WARNING';
  MESSAGE_TYPE_SYSTEM = 'SYSTEM';

begin
  case AType of
    msDebug:
      _ApplicationMessage(MESSAGE_TYPE_DEBUG, ATitle, AMessage, [moDebugView, moLogFile]);
    msInfo:
      _ApplicationMessage(MESSAGE_TYPE_INFO, ATitle, AMessage);
    msError:
      _ApplicationMessage(MESSAGE_TYPE_ERROR, ATitle, AMessage);
    msWarning:
      _ApplicationMessage(MESSAGE_TYPE_WARNING, ATitle, AMessage);
    msSystem:
      begin
        if FUseDebug then
          _ApplicationMessage(MESSAGE_TYPE_SYSTEM, ATitle, AMessage, [moDebugView, moLogFile])
        else
          _ApplicationMessage(MESSAGE_TYPE_SYSTEM, ATitle, AMessage, [moDebugView])
      end;
  else
    _ApplicationMessage('UNKNOWN', ATitle, AMessage);
  end;

  if Assigned(FOnAfterLogging) then
    FOnAfterLogging(AType, ATitle, AMessage);
end;

procedure TLogging.AppendLog(const AName, AMsg: string);
begin
  FMsgQueue.Enqueue(TJdcLog.Create(AName, Now, AMsg));
end;

procedure TLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AFormat: String;
  const Args: array of const; const DebugLog: Boolean);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str, DebugLog);
end;

constructor TLogging.Create;
begin
  FMsgQueue := TQueue<TJdcLog>.Create;

  FProjectCode := 'MyProject';
  FAppCode := 'MyApp';
  FLogName := '';
  FExeVersion := 'v1.0.0';

  FUseDebug := False;
  FLogServer.StringValue := '';
  FLogServer.IntegerValue := 8094;
  FUseCloudLog := False;
  FOnAfterLogging := nil;
end;

destructor TLogging.Destroy;
begin
  FMsgQueue.Free;
  inherited;
end;

procedure TLogging.FlushLog;

  procedure _PrintLog(ALog: TJdcLog);
  var
    Stream: TStreamWriter;
    LogName: string;
  begin
    LogName := ALog.LogName;

    if LogName.IsEmpty then
      Exit;

    BackupLogFile(LogName);
    try
      Stream := TFile.AppendText(LogName);
      try
        if ALog.Msg.IsEmpty then
          Stream.WriteLine
        else
          Stream.WriteLine(ALog.ToString);

        while FMsgQueue.Count > 0 do
        begin
          Sleep(1);
          ALog := FMsgQueue.Dequeue;

          if LogName <> ALog.LogName then
          begin
            _PrintLog(ALog);
            break;
          end;

          if ALog.Msg.IsEmpty then
            Stream.WriteLine
          else
            Stream.WriteLine(ALog.ToString);
        end;
      finally
        FreeAndNil(Stream);
      end;
    except
      on E: Exception do
        PrintLog(LogName + '.tmp', ALog.Time, ALog.Msg);
    end;
  end;

var
  MyLog: TJdcLog;
begin
  if FMsgQueue.Count = 0 then
    Exit;

  MyLog := FMsgQueue.Dequeue;
  _PrintLog(MyLog);
end;

function TLogging.GetLogName: String;
begin
{$IFDEF LOGGING_DAY_TAG}
  Result := ChangeFileExt(FLogName, FormatDateTime('_YYYYMMDD', Now) + '.log');
{$ELSE}
  Result := FLogName;
{$ENDIF}
end;

class function TLogging.Obj: TLogging;
begin
  if MyObj = nil then
    MyObj := TLogging.Create;
  Result := MyObj;
end;

procedure TLogging.SetLogName(const ExeName: string);
begin
  FExeVersion := FileVersion(ExeName);
  FLogName := ExtractFilePath(ExeName) + 'logs\' + ChangeFileExt(ExtractFileName(ExeName), '.log');
{$IFDEF LOCALAPPDATA}
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\DACO\' + FAppCode + '\' + ExtractFileName(FLogName);
{$ENDIF}
  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));
end;

procedure TLogging.SetLogServer(const Value: TConnInfo);
begin
  FLogServer := Value;
end;

procedure TLogging.SetUseCloudLog(const Value: Boolean);
begin
  FUseCloudLog := Value;
end;

procedure TLogging.SetUseDebug(const Value: Boolean);
begin
  FUseDebug := Value;
end;

procedure TLogging.StartLogging;
begin
  if FLogTask = nil then
  begin
    FLogTask := TThread.CreateAnonymousThread(
      procedure
      begin
        while not TThread.CurrentThread.CheckTerminated do
        begin
          Sleep(507);
          FlushLog;
        end;
      end);
    FLogTask.FreeOnTerminate := False;
    FLogTask.Start;
    ApplicationMessage(msDebug, 'Logging',
      'Project=%s,AppCode=%s,UseDebug=%s,UseCloudLog=%s,LogServer=%s,File=%s',
      [FProjectCode, FAppCode, BoolToStr(FUseDebug, True), BoolToStr(FUseCloudLog, True), FLogServer.ToString,
      FLogName]);
  end;
end;

procedure TLogging.StopLogging;
begin
  if Assigned(FLogTask) then
  begin
    FLogTask.Terminate;
    FLogTask.WaitFor;
    FreeAndNil(FLogTask);
    PrintDebug('[%s] StopLogging', [FAppCode]);
  end;
end;

procedure TLogging._ApplicationMessage(const AType, ATitle, AMessage: String; const AOutputs: TMsgOutputs);
var
  splitter: string;
  MyType: string;
begin
  if AMessage.IsEmpty then
    splitter := ''
  else
    splitter := ' - ';

  MyType := '<' + AType + '>';
  if moDebugView in AOutputs then
    PrintDebug('%-9s [%s] %s%s%s', [MyType, FAppCode, ATitle, splitter, AMessage]);

  if moLogFile in AOutputs then
    AppendLog(GetLogName, Format('%-9s %s%s%s', [MyType, ATitle, splitter, AMessage]));

  if (moCloudMessage in AOutputs) and FUseCloudLog then
    CloudMessage(FProjectCode, FAppCode, AType, ATitle, AMessage, FExeVersion, FLogServer);
end;

{ TJdcLog }

constructor TJdcLog.Create(AName: string; ATime: TDateTime; AMsg: string);
begin
  Self.LogName := AName;
  Self.Time := ATime;
  Self.Msg := AMsg;
end;

function TJdcLog.ToString: string;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', Self.Time) + Self.Msg
end;

initialization

MyObj := TLogging.Create;

finalization

MyObj.StopLogging;
MyObj.Free;

end.
