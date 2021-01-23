// *******************************************************
//
// Jdc Logging Util
//
// Copyright(c) 2021 UDNS.
//
// jsf3rd@nate.net
//
// Define : LOCALAPPDATA, LOGGING_DAY_TAG
//
// *******************************************************

unit JdcLogging;

interface

uses System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, JdcGlobal, JclFileUtils,
  Vcl.StdCtrls, Winapi.Windows, JdcOption, System.DateUtils;

type
  TLogProc = procedure(const AType: TMessageType; const ATitle: String; const AMessage: String = '')
    of object;

  TMsgOutput = (moDebugView, moLogFile, moCloudMessage);
  TMsgOutputs = set of TMsgOutput;

  TJdcLog = record
    LogName: string;
    Time: TDateTime;
    Msg: string;
    constructor Create(const AName: string; const ATime: TDateTime; const AMsg: string);
    function ToString: string;
  end;

  TLogging = class sealed
  const
    MESSAGE_TYPE_INFO = 'INFO';
    MESSAGE_TYPE_ERROR = 'ERROR';
    MESSAGE_TYPE_DEBUG = 'DEBUG';
    MESSAGE_TYPE_WARNING = 'WARNING';
    MESSAGE_TYPE_SYSTEM = 'SYSTEM';
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

    FOption: TOptionAbstract;
    FCheckDebug: TDateTime;

    constructor Create;
    procedure FlushLog;

    procedure _ApplicationMessage(const AType: string; const ATitle: string; const AMessage: String;
      const AOutputs: TMsgOutputs = [moDebugView, moLogFile, moCloudMessage]);

    function GetLogName: String;
    procedure _PrintLog(var ALog: TJdcLog);
  public
    destructor Destroy; override;

    class function Obj: TLogging;
    class function MessageTypeToStr(const AType: TMessageType): string;

    procedure StartLogging;
    procedure StopLogging;

    procedure Init(AGlobal: TGlobalAbstract; AOption: TOptionAbstract);

    function GetLogNameEx(const ATag: string): string;

    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); overload;
    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AFormat: String;
      const Args: array of const); overload;

    procedure AppendLog(const AName: string; const AMsg: string);
    procedure PrintUseDebug;
    Procedure PrintUseCloudLog;

    property ProjectCode: String read FProjectCode write FProjectCode;
    property AppCode: String read FAppCode write FAppCode;

    property UseDebug: Boolean read FUseDebug write FUseDebug;
    property UseCloudLog: Boolean read FUseCloudLog write FUseCloudLog;

    property LogName: String read GetLogName;
    property LogServer: TConnInfo read FLogServer write FLogServer;

    property OnAfterLogging: TLogProc read FOnAfterLogging write FOnAfterLogging;
  end;

  // 로그 찍기..
procedure PrintLog(const AFile: string; const ATime: TDateTime; const AMessage: String); overload;
procedure PrintLog(const AMemo: TMemo; const AMsg: String); overload;
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
{$IFDEF LOGGING_DAY_TAG}
        FileMove(AName, ChangeFileExt(AName, FormatDateTime('_HHNNSS', Now) + '.bak'), True);
{$ELSE}
        FileMove(AName, ChangeFileExt(AName, FormatDateTime('_YYYYMMDD_HHNNSS', Now) + '.bak'), True);
{$ENDIF}
      except
        on E: Exception do
          AName := ChangeFileExt(AName, FormatDateTime('_YYYYMMDD', Now) + '.tmp');
      end;
    end;
  end;
end;

procedure PrintLog(const AMemo: TMemo; const AMsg: String);
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

procedure TLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AMessage: String);
begin
  case AType of
    msDebug:
      _ApplicationMessage(MessageTypeToStr(AType), ATitle, AMessage, [moDebugView, moLogFile]);
    msInfo, msError, msWarning:
      _ApplicationMessage(MessageTypeToStr(AType), ATitle, AMessage);
    msSystem:
      begin
        if FUseDebug then
          _ApplicationMessage(MessageTypeToStr(AType), ATitle, AMessage, [moDebugView, moLogFile])
        else
          _ApplicationMessage(MessageTypeToStr(AType), ATitle, AMessage, [moDebugView])
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
  const Args: array of const);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str);
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

  FCheckDebug := Now;
end;

destructor TLogging.Destroy;
begin
  StopLogging;

  if Assigned(FMsgQueue) then
    FreeAndNil(FMsgQueue);
end;

procedure TLogging._PrintLog(var ALog: TJdcLog);
var
  Stream: TStreamWriter;
  LogName: string;
begin
  LogName := ALog.LogName;

  if LogName.IsEmpty then
    Exit;

  BackupLogFile(LogName);
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
        FreeAndNil(Stream); // 기존 파일은 닫아 준다.
        _PrintLog(ALog);
        break;
      end;

      if ALog.Msg.IsEmpty then
        Stream.WriteLine
      else
        Stream.WriteLine(ALog.ToString);
    end;
  finally
    if Assigned(Stream) then
      FreeAndNil(Stream);
  end;
end;

procedure TLogging.FlushLog;
var
  MyLog: TJdcLog;
begin
  if FMsgQueue.Count = 0 then
    Exit;

  if MinuteSpan(Now, FCheckDebug) > 1 then
  begin
    FCheckDebug := Now;
    FUseDebug := FOption.UseDebug;
  end;

  Sleep(227);
  MyLog := FMsgQueue.Dequeue;
  try
    _PrintLog(MyLog);
  except
    on E: Exception do
      PrintLog(LogName + '.tmp', MyLog.Time, MyLog.Msg + ',E=' + E.Message);
  end;
end;

function TLogging.GetLogName: String;
begin
{$IFDEF LOGGING_DAY_TAG}
  Result := ChangeFileExt(FLogName, FormatDateTime('_YYYYMMDD', Now) + '.log');
{$ELSE}
  Result := FLogName;
{$ENDIF}
end;

function TLogging.GetLogNameEx(const ATag: string): string;
begin
{$IFDEF LOGGING_DAY_TAG}
  Result := ChangeFileExt(FLogName, Format('_%s_%s.log', [ATag, FormatDateTime('YYYYMMDD', Now)]));
{$ELSE}
  Result := ChangeFileExt(FLogName, Format('_%s.log', [ATag]));
{$ENDIF}
end;

procedure TLogging.Init(AGlobal: TGlobalAbstract; AOption: TOptionAbstract);
var
  ExeName: string;
begin
  FOption := AOption;

  FProjectCode := AGlobal.ProjectCode;
  FAppCode := AGlobal.AppCode;
  FUseDebug := AOption.UseDebug;
  FUseCloudLog := AOption.UseCloudLog;
  FLogServer := AOption.LogServer;

  ExeName := AGlobal.ExeName;
  FExeVersion := FileVersion(ExeName);
  FLogName := ExtractFilePath(ExeName) + 'logs\' + ChangeFileExt(ExtractFileName(ExeName), '.log');
{$IFDEF LOCALAPPDATA}
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\Judico\' + FAppCode + '\logs\' +
    ExtractFileName(FLogName);
{$ENDIF}
  if not TDirectory.Exists(ExtractFilePath(FLogName)) then
    TDirectory.CreateDirectory(ExtractFilePath(FLogName));

  StartLogging;
end;

class function TLogging.MessageTypeToStr(const AType: TMessageType): string;
begin
  case AType of
    msDebug:
      Result := MESSAGE_TYPE_DEBUG;
    msInfo:
      Result := MESSAGE_TYPE_INFO;
    msError:
      Result := MESSAGE_TYPE_ERROR;
    msWarning:
      Result := MESSAGE_TYPE_WARNING;
    msSystem:
      Result := MESSAGE_TYPE_SYSTEM;
  else
    Result := 'UNKNOWN';
  end;
end;

class function TLogging.Obj: TLogging;
begin
  if MyObj = nil then
    MyObj := TLogging.Create;
  Result := MyObj;
end;

procedure TLogging.PrintUseCloudLog;
begin
  ApplicationMessage(msInfo, 'UseCloudLog', BoolToStr(FUseCloudLog, True));
end;

procedure TLogging.PrintUseDebug;
begin
  ApplicationMessage(msInfo, 'UseDebug', BoolToStr(FUseDebug, True));
end;

procedure TLogging.StartLogging;
begin
  if LogName = '' then
  begin
    PrintDebug('No LogName!!');
    Exit;
  end;

  if Assigned(FLogTask) then
    Exit;

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

procedure TLogging.StopLogging;
begin
  if not Assigned(FLogTask) then
    Exit;

  FLogTask.Terminate;
  FLogTask.WaitFor;
  FreeAndNil(FLogTask);
  FlushLog;
  PrintDebug('[%s] StopLogging', [FAppCode]);
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

constructor TJdcLog.Create(const AName: string; const ATime: TDateTime; const AMsg: string);
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

MyObj.Free;

end.
