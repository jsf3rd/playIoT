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

  TLogMessage = record
    LogName: string;
    Time: TDateTime;
    Msg: string;
    constructor Create(AName: string; ATime: TDateTime; AMsg: string);
    function ToString: string;
  end;

  TJdcLogging = class sealed
  private
    FLogName: string;
    FExeVersion: string;

    FLogTask: TThread;
    FMsgQueue: TQueue<TLogMessage>;
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

    class function Obj: TJdcLogging;

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
  MyObj: TJdcLogging = nil;

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

{ TJdcLogging }

procedure TJdcLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AMessage: String;
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

procedure TJdcLogging.AppendLog(const AName, AMsg: string);
begin
  FMsgQueue.Enqueue(TLogMessage.Create(AName, Now, AMsg));
end;

procedure TJdcLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AFormat: String;
  const Args: array of const; const DebugLog: Boolean);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str, DebugLog);
end;

constructor TJdcLogging.Create;
begin
  FMsgQueue := TQueue<TLogMessage>.Create;

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

destructor TJdcLogging.Destroy;
begin
  FMsgQueue.Free;
  inherited;
end;

procedure TJdcLogging.FlushLog;

  procedure _PrintLog(ALog: TLogMessage);
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
  MyLog: TLogMessage;
begin
  if FMsgQueue.Count = 0 then
    Exit;

  MyLog := FMsgQueue.Dequeue;
  _PrintLog(MyLog);
end;

function TJdcLogging.GetLogName: String;
begin
{$IFDEF LOGGING_DAY_TAG}
  Result := ChangeFileExt(FLogName, FormatDateTime('_YYYYMMDD', Now) + '.log');
{$ELSE}
  Result := FLogName;
{$ENDIF}
end;

class function TJdcLogging.Obj: TJdcLogging;
begin
  if MyObj = nil then
    MyObj := TJdcLogging.Create;
  Result := MyObj;
end;

procedure TJdcLogging.SetLogName(const ExeName: string);
begin
  FExeVersion := FileVersion(ExeName);
  FLogName := ExtractFilePath(ExeName) + 'logs\' + ChangeFileExt(ExtractFileName(ExeName), '.log');
{$IFDEF LOCALAPPDATA}
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\' + FProjectCode + '\' + FAppCode + '\' +
    ExtractFileName(FLogName);
{$ENDIF}
  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));
end;

procedure TJdcLogging.SetLogServer(const Value: TConnInfo);
begin
  FLogServer := Value;
end;

procedure TJdcLogging.SetUseCloudLog(const Value: Boolean);
begin
  FUseCloudLog := Value;
end;

procedure TJdcLogging.SetUseDebug(const Value: Boolean);
begin
  FUseDebug := Value;
end;

procedure TJdcLogging.StartLogging;
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

procedure TJdcLogging.StopLogging;
begin
  if Assigned(FLogTask) then
  begin
    FLogTask.Terminate;
    FLogTask.WaitFor;
    FreeAndNil(FLogTask);
    PrintDebug('[%s] StopLogging', [FAppCode]);
  end;
end;

procedure TJdcLogging._ApplicationMessage(const AType, ATitle, AMessage: String; const AOutputs: TMsgOutputs);
var
  splitter: string;
begin
  if AMessage.IsEmpty then
    splitter := ''
  else
    splitter := ' - ';

  if moDebugView in AOutputs then
    PrintDebug('<%s> [%s] %s%s%s', [AType, FAppCode, ATitle, splitter, AMessage]);

  if moLogFile in AOutputs then
    AppendLog(GetLogName, Format('<%s> %s%s%s', [AType, ATitle, splitter, AMessage]));

  if (moCloudMessage in AOutputs) and FUseCloudLog then
    CloudMessage(FProjectCode, FAppCode, AType, ATitle, AMessage, FExeVersion, FLogServer);
end;

{ TJdcLog }

constructor TLogMessage.Create(AName: string; ATime: TDateTime; AMsg: string);
begin
  Self.LogName := AName;
  Self.Time := ATime;
  Self.Msg := AMsg;
end;

function TLogMessage.ToString: string;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', Self.Time) + Self.Msg
end;

initialization

MyObj := TJdcLogging.Create;

finalization

MyObj.StopLogging;
MyObj.Free;

end.
