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

uses System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections, JdcGlobal,
  JdcOption, System.DateUtils
{$IFDEF MSWINDOWS}
    , Vcl.StdCtrls, Winapi.Windows
{$ENDIF}
{$IF CompilerVersion  < 35} // Delphi 11
    , JclFileUtils
{$ENDIF}
    ;

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
    FMsgQueue: TDictionary<string, TQueue<TJdcLog>>;
    FLogServer: TConnInfo;
    FProjectCode: string;
    FAppCode: String;
    FPublisher: string;

    FUseDebug: Boolean;
    FUseCloudLog: Boolean;
    FOnAfterLogging: TLogProc;

    FOption: TOptionAbstract;
    FCheckDebug: TDateTime;

    constructor Create;
    procedure FlushLog;

    procedure _ApplicationMessage(const AType: string; const ATitle: string; const AMessage: String;
      const ATime: TDateTime = 0; const AOutputs: TMsgOutputs = [moDebugView, moLogFile, moCloudMessage]);

    function GetLogName: String;
    procedure _PrintLog;
  public
    destructor Destroy; override;

    class function Obj: TLogging;
    class function MessageTypeToStr(const AType: TMessageType): string;

    procedure StartLogging;
    procedure StopLogging;

    procedure Init(AGlobal: TGlobalAbstract; AOption: TOptionAbstract; APath: string = '');

    function GetLogNameEx(const ATag: string): string;

    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AMessage: String = '';
      const ATime: TDateTime = 0); overload;
    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AFormat: String;
      const Args: array of const; const ATime: TDateTime = 0); overload;

    procedure AppendLog(const AName: string; const AMsg: string; const ATime: TDateTime = 0);
    procedure PrintUseDebug;
    Procedure PrintUseCloudLog;

    property ProjectCode: String read FProjectCode write FProjectCode;
    property AppCode: String read FAppCode write FAppCode;
    property ExeVersion: string read FExeVersion write FExeVersion;

    property UseDebug: Boolean read FUseDebug write FUseDebug;
    property UseCloudLog: Boolean read FUseCloudLog write FUseCloudLog;

    property LogName: String read GetLogName;
    property LogServer: TConnInfo read FLogServer write FLogServer;

    property OnAfterLogging: TLogProc read FOnAfterLogging write FOnAfterLogging;
  end;

  // ·Î±× Âï±â..
procedure PrintLog(const AFile: string; const AMessage: String; const ATime: TDateTime = 0); overload;
procedure PrintDebug(const Format: string; const Args: array of const); overload;
procedure PrintDebug(const str: string); overload;

{$IFDEF MSWINDOWS}
procedure PrintLog(const AMemo: TMemo; const AMsg: String; const ATime: TDateTime = 0;
  const ShowTime: Boolean = True); overload;
{$ENDIF}

implementation

var
  MyObj: TLogging = nil;

procedure PrintDebug(const Format: string; const Args: array of const); overload;
var
  str: string;
begin
  FmtStr(str, Format, Args);
  PrintDebug(str);
end;

procedure PrintDebug(const str: string); overload;
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar('[JDC] ' + str));
{$ENDIF}
end;

procedure BackupLogFile(AName: string; AMaxSize: Integer = 1024 * 1024 * 5);
begin
  if FileExists(AName) then
  begin
{$IF CompilerVersion  < 35} // Delphi 11
    if JclFileUtils.FileGetSize(AName) > AMaxSize then
{$ELSE}
    if TFile.GetSize(AName) > AMaxSize then
{$ENDIF}
    begin
      try
{$IFDEF LOGGING_DAY_TAG}
        TFile.Move(AName, ChangeFileExt(AName, FormatDateTime('_HHNNSS', Now) + '.bak'));
{$ELSE}
        TFile.Move(AName, ChangeFileExt(AName, FormatDateTime('_YYYYMMDD_HHNNSS', Now) + '.bak'));
{$ENDIF}
      except
        on E: Exception do
          PrintDebug('BackupLogFile - E=' + E.Message);
      end;
    end;
  end;
end;

{$IFDEF MSWINDOWS}

procedure PrintLog(const AMemo: TMemo; const AMsg: String; const ATime: TDateTime; const ShowTime: Boolean);
var
  TimeStr: string;
begin
  if not Assigned(AMemo) then
    Exit;

  ThreadSafe(
    procedure
    begin
      if AMemo.Lines.Count > 5000 then
        AMemo.Lines.Clear;

      if AMsg.IsEmpty then
        AMemo.Lines.Add('')
      else
      begin

        TimeStr := '';
        if ShowTime then
        begin
          if ATime = 0 then
            TimeStr := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', Now)
          else
            TimeStr := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', ATime);
        end;

        AMemo.Lines.Add(TimeStr + AMsg);
      end;
    end);
end;
{$ENDIF}

procedure PrintLog(const AFile: string; const AMessage: String; const ATime: TDateTime);
var
  Stream: TStreamWriter;
begin
  BackupLogFile(AFile);
  try
    Stream := TFile.AppendText(AFile);
    try
      if AMessage.IsEmpty then
        Stream.WriteLine
      else
      begin
        if ATime = 0 then
          Stream.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', Now) + AMessage)
        else
          Stream.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz, ', ATime) + AMessage)
      end;
    finally
      FreeAndNil(Stream);
    end;
  except
    on E: Exception do
      PrintDebug(E.Message + ', ' + AMessage);
  end;
end;

{ TLogging }

procedure TLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AMessage: String;
const ATime: TDateTime);
var
  TypeName: string;
begin
  TypeName := MessageTypeToStr(AType);
  case AType of
    msDebug:
      _ApplicationMessage(TypeName, ATitle, AMessage, ATime, [moDebugView, moLogFile]);
    msInfo, msError, msWarning:
      _ApplicationMessage(TypeName, ATitle, AMessage, ATime);
    msSystem:
      begin
        if FUseDebug then
          _ApplicationMessage(TypeName, ATitle, AMessage, ATime, [moDebugView, moLogFile])
        else
          _ApplicationMessage(TypeName, ATitle, AMessage, ATime, [moDebugView])
      end;
  else
    _ApplicationMessage('UNKNOWN', ATitle, AMessage, ATime);
  end;

  if Assigned(FOnAfterLogging) then
    FOnAfterLogging(AType, ATitle, AMessage);
end;

procedure TLogging.AppendLog(const AName, AMsg: string; const ATime: TDateTime);
var
  Queue: TQueue<TJdcLog>;
begin
  if AName = '' then
    Exit;
  // raise Exception.Create('No log file name.');

  if not FMsgQueue.ContainsKey(AName) then
  begin
    Queue := TQueue<TJdcLog>.Create;
    Queue.Capacity := 1024;
    FMsgQueue.Add(AName, Queue);
  end;

  if ATime = 0 then
    FMsgQueue.Items[AName].Enqueue(TJdcLog.Create(AName, Now, AMsg))
  else
    FMsgQueue.Items[AName].Enqueue(TJdcLog.Create(AName, ATime, AMsg))
end;

procedure TLogging.ApplicationMessage(const AType: TMessageType; const ATitle, AFormat: String;
const Args: array of const; const ATime: TDateTime);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str, ATime);
end;

constructor TLogging.Create;
begin
  FMsgQueue := TDictionary < String, TQueue < TJdcLog >>.Create;
  FMsgQueue.Capacity := 16;

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
var
  MyElem: TQueue<TJdcLog>;
begin
  StopLogging;

  if Assigned(FMsgQueue) then
  begin
    for MyElem in FMsgQueue.Values do
      MyElem.Free;
    FreeAndNil(FMsgQueue);
  end;
end;

procedure TLogging._PrintLog;
var
  Stream: TStreamWriter;
  JdcLog: TJdcLog;
  MyKey: string;
  MsgQueue: TQueue<TJdcLog>;
begin
  for MyKey in FMsgQueue.Keys do
  begin
    MsgQueue := FMsgQueue.Items[MyKey];
    if MsgQueue.Count = 0 then
      Continue;

    BackupLogFile(MyKey);
    Stream := TFile.AppendText(MyKey);
    try
      while MsgQueue.Count > 0 do
      begin
        Sleep(1);
        JdcLog := MsgQueue.Dequeue;

        if JdcLog.Msg.IsEmpty then
          Stream.WriteLine
        else
        begin
          try
            Stream.WriteLine(JdcLog.ToString);
          except
            on E: Exception do
              _ApplicationMessage(MessageTypeToStr(msError), '_PrintLog', JdcLog.ToString + ',E=' + E.Message,
                Now, [moCloudMessage]);
          end;
        end;
      end;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

procedure TLogging.FlushLog;
begin
  if FMsgQueue.Count = 0 then
    Exit;

  if MinuteSpan(Now, FCheckDebug) > 1 then
  begin
    FCheckDebug := Now;
    FUseDebug := FOption.UseDebug;
  end;

  // Sleep(441);
  try
    _PrintLog;
  except
    on E: Exception do
    begin
      _ApplicationMessage(MessageTypeToStr(msError), 'PrintLog', 'E=' + E.Message, Now, [moCloudMessage]);
    end;
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

procedure TLogging.Init(AGlobal: TGlobalAbstract; AOption: TOptionAbstract; APath: string);
var
  ExeName: string;
begin
  FOption := AOption;

  FProjectCode := AGlobal.ProjectCode;
  FAppCode := AGlobal.AppCode;
  FPublisher := AGlobal.Publisher;
  FUseDebug := AOption.UseDebug;
  FUseCloudLog := AOption.UseCloudLog;
  FLogServer := AOption.LogServer;

  ExeName := AGlobal.ExeName;
  FExeVersion := FileVersion(ExeName);

  if APath = '' then
    FLogName := TPath.Combine(ExtractFilePath(ExeName) + 'logs',
      ChangeFileExt(ExtractFileName(ExeName), '.log'))
  else
    FLogName := TPath.Combine(APath, ChangeFileExt(ExtractFileName(ExeName), '.log'));
{$IFDEF LOCALAPPDATA}
  FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\' + FPublisher + '\' + FAppCode + '\logs\' +
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
  ApplicationMessage(msInfo, 'UseCloudLog', BoolToStr(FUseCloudLog));
end;

procedure TLogging.PrintUseDebug;
begin
  ApplicationMessage(msInfo, 'UseDebug', BoolToStr(FUseDebug));
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
        Sleep(1001);
        try
          FlushLog;
        except
          on E: Exception do
          begin
            _ApplicationMessage(MessageTypeToStr(msError), 'FlushLog', 'E=' + E.Message, Now,
              [moCloudMessage]);
          end;
        end;
      end;
    end);
  FLogTask.FreeOnTerminate := False;
  FLogTask.Start;
  ApplicationMessage(msDebug, 'Logging',
    'Project=%s,AppCode=%s,UseDebug=%s,UseCloudLog=%s,LogServer=%s,File=%s',
    [FProjectCode, FAppCode, BoolToStr(FUseDebug), BoolToStr(FUseCloudLog), FLogServer.ToString, FLogName]);
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

procedure TLogging._ApplicationMessage(const AType, ATitle, AMessage: String; const ATime: TDateTime;
const AOutputs: TMsgOutputs);
var
  splitter: string;
  MyType: string;
begin
  if AMessage.IsEmpty then
    splitter := ''
  else
    splitter := ' - ';

  MyType := '<' + AType + '>';
  if (moCloudMessage in AOutputs) and FUseCloudLog then
    CloudMessage(FProjectCode, FAppCode, AType, ATitle, AMessage, FExeVersion, FLogServer);

{$IFDEF MSWindows}
  if moLogFile in AOutputs then
    AppendLog(GetLogName, Format('%-9s %s%s%s', [MyType, ATitle, splitter, AMessage]), ATime);

  if (moDebugView in AOutputs) and FUseDebug then
    PrintDebug('%-9s [%s] %s%s%s', [MyType, FAppCode, ATitle, splitter, AMessage]);
{$ENDIF}
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

if MyObj = nil then
  MyObj := TLogging.Create;

finalization

if Assigned(MyObj) then
  FreeAndNil(MyObj);

end.
