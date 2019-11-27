// *******************************************************
//
// playIoT Global Library
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
//
// *******************************************************

unit JdcGlobal;

interface

uses
  Classes, SysUtils, Windows, ZLib, IdGlobal, IOUtils, JclFileUtils, Vcl.ExtCtrls,
  IdUDPClient, JclSysInfo, psAPI, IdContext, IdExceptionCore, Vcl.StdCtrls, JclSvcCtrl, Vcl.ActnList,
  Vcl.Dialogs, WinApi.Shellapi, UITypes, System.Generics.Collections;

type
  IExecuteFunc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    function Execute(AValue: String): T;
  End;

  IExecuteProc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    procedure Execute(AValue: T);
  End;

  TMessageType = (msDebug, msInfo, msError, msWarning, msUnknown);

  TLogProc = //
    procedure(const AType: TMessageType; const ATitle: String; const AMessage: String = '') of object;

  TOnMessageEvent = //
    procedure(const Sender: TObject; const AName: string; const AMessage: string = '') of object;

  TMsgOutput = (moDebugView, moLogFile, moCloudMessage);
  TMsgOutputs = set of TMsgOutput;

  TConnInfo = record
    StringValue: string;
    IntegerValue: Integer;
    constructor Create(AString: string; AInteger: Integer);
    function ToString: string;
    function Equals(const ConnInfo: TConnInfo): boolean;
  end;

  TClientInfo = record
    Version: string;
    Url: string;
  end;

  TMemoLog = record
    Memo: TMemo;
    Msg: string;
    Time: TDateTime;
    constructor Create(AMemo: TMemo; AMsg: string);
  end;

  TJdcLog = record
    LogName: string;
    Time: TDateTime;
    Msg: string;
    constructor Create(AName: string; ATime: TDateTime; AMsg: string);
    function ToString: string;
  end;

  TGlobalAbstract = class abstract
  strict private
    FLogTask: TThread;
    FMsgQueue: TQueue<TJdcLog>;
    procedure FlushLog;
  protected
    FProjectCode: string;
    FAppCode: string;

    FIsInitialized: boolean;
    FIsFinalized: boolean;
    FExeName: String;
    FLogName: string;
    FUseCloudLog: boolean;
    FUseDebug: boolean;

    FStartTime: TDateTime;

    FLogServer: TConnInfo;
    procedure SetExeName(const Value: String); virtual; abstract;

    procedure _ApplicationMessage(const AType: string; const ATitle: string; const AMessage: String;
      const AOutputs: TMsgOutputs = [moDebugView, moLogFile, moCloudMessage]); virtual;

    function GetLogName: string; virtual;
    procedure SetUseDebug(const Value: boolean); virtual;
    procedure AppendLog(const AName: string; const AMsg: string);

    procedure StartLogging;
    procedure StopLogging;
  public
    constructor Create; virtual;

    procedure Initialize; virtual;
    procedure Finalize; virtual;

    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); overload; virtual;
    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AFormat: String;
      const Args: array of const); overload;

    property ExeName: String read FExeName write SetExeName;
    property LogName: string read GetLogName;

    property LogServer: TConnInfo read FLogServer write FLogServer;
    property UseDebug: boolean read FUseDebug write SetUseDebug;

  const
    MESSAGE_TYPE_INFO = 'INFO';
    MESSAGE_TYPE_ERROR = 'ERROR';
    MESSAGE_TYPE_DEBUG = 'DEBUG';
    MESSAGE_TYPE_WARNING = 'WARNING';
    MESSAGE_TYPE_UNKNOWN = 'UNKNOWN';
  end;

  // 로그 찍기..
procedure PrintLog(const AFile: string; const ATime: TDateTime; const AMessage: String); overload;
procedure PrintLog(AMemo: TMemo; const AMsg: String); overload;

procedure PrintDebug(const Format: string; const Args: array of const); overload;
procedure PrintDebug(const str: string); overload;

// 특정 자리수 이하 0 만들기
// Value : 값, Digit : 자리수
function TruncInt(Value: Integer; Digit: Integer): Integer;

function CurrentProcessMemory: Cardinal;
function FileVersion(const FileName: String): String;
procedure CloudMessage(const ProjectCode, AppCode, TypeCode, ATitle, AMessage, AVersion: String;
  const AServer: TConnInfo);

// 데이터 압축..
function CompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): boolean;

// 데이터 압축 해제..
function DeCompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): boolean;

// 응답 검사..
function Contains(Contents: string; const str: array of const): boolean;
function IsGoodResponse(Text, Command: string; Response: array of const): boolean;

// Reverse 2Btyes..
function Rev2Bytes(w: WORD): WORD;

// Reverse 4Btyes..
function Rev4Bytes(Value: Int32): Int32;

// Reverse 4Btyes..
function Rev4BytesF(Value: Single): Single;

// Big endian
function WordToBytes(AValue: WORD): TIdBytes;

// Big endian
function DWordToBytes(AValue: DWORD): TIdBytes;

// little endian
function HexStrToWord(const ASource: string; const AIndex: Integer = 1): WORD;

function HexStrToByte(const ASource: String; const AIndex: Integer = 1): Byte;
function HexStrToBytes(const ASource: string; const AIndex: Integer = 1): TIdBytes;

function IdBytesToHex(const AValue: TIdBytes; const ASpliter: String = ' '): String;
function BytesToHex(const AValue: TBytes; const ASpliter: String = ' '): String;

function IdBytesPos(const SubIdBytes, IdBytes: TIdBytes; const AIndex: Integer = 0): Integer;

function DefaultFormatSettings: TFormatSettings;

function StrDefault(str: string; Default: string): string;

// Thread Safe
procedure ThreadSafe(AMethod: TThreadMethod); overload;
procedure ThreadSafe(AThreadProc: TThreadProcedure); overload;

procedure FreeAndNilEx(var Obj; const AGlobal: TGlobalAbstract = nil);

// 서비스 관리
procedure StartService(const ServiceName: String; var OldStatus: TJclServiceState; StartAction: TAction);
procedure StopService(const ServiceName: String; var OldStatus: TJclServiceState; StopAction: TAction;
  hnd: HWND);
procedure UpdateServiceStatus(const ServiceName: String; var OldStatus: TJclServiceState;
  StartAction, StopAction: TAction; StatusEdit: TLabeledEdit);

function IntToBin(Value: Cardinal; Digits: Integer): String;

const
  LOCAL_SERVER = '\\localhost';

implementation

uses JdcGlobal.ClassHelper;

function IntToBin(Value: Cardinal; Digits: Integer): String;
var
  S: String;
begin
  S := '';
  While Digits > 0 do
  begin
    if Odd(Value) then
      S := '1' + S
    Else
      S := '0' + S;
    Value := Value shr 1;
    Digits := Digits - 1;
  end;
  Result := S;
end;

procedure FreeAndNilEx(var Obj; const AGlobal: TGlobalAbstract);
begin
  try
    if Assigned(TObject(Obj)) then
      FreeAndNil(Obj);
  except
    on E: Exception do
    begin
      if Assigned(AGlobal) then
        AGlobal.ApplicationMessage(msError, 'FreeAndNilEx - ' + TObject(Obj).ClassName, E.Message);
    end;
  end;
end;

procedure StartService(const ServiceName: String; var OldStatus: TJclServiceState; StartAction: TAction);
begin
  OldStatus := ssUnknown;

  StartAction.Enabled := False;
  if StartServiceByName(LOCAL_SERVER, ServiceName) then
    Exit;

  MessageDlg('서비스를 시작하지 못했습니다.', TMsgDlgType.mtWarning, [mbOK], 0);
  StartAction.Enabled := true;
end;

procedure StopService(const ServiceName: String; var OldStatus: TJclServiceState; StopAction: TAction;
  hnd: HWND);
begin
  OldStatus := ssUnknown;
  StopAction.Enabled := False;
  if StopServiceByName(LOCAL_SERVER, ServiceName) then
    Exit;

  if MessageDlg('알림 : 서비스를 중지하지 못했습니다.' + #13#10 + '강제로 중지하시겠습니까?', TMsgDlgType.mtConfirmation, [mbYes, mbNo],
    0) = mrYes then
    ShellExecute(hnd, 'open', 'taskkill', PWideChar(' -f -im ' + ServiceName + '.exe'), nil, SW_HIDE);
end;

procedure UpdateServiceStatus(const ServiceName: String; var OldStatus: TJclServiceState;
  StartAction, StopAction: TAction; StatusEdit: TLabeledEdit);
var
  Status: TJclServiceState;
begin
  Status := GetServiceStatusByName(LOCAL_SERVER, ServiceName);

  if OldStatus = Status then
    Exit;

  OldStatus := Status;
  StartAction.Enabled := False;
  StopAction.Enabled := False;
  case Status of
    ssUnknown:
      StatusEdit.Text := '알수없음(등록된 서비스가 없습니다).';
    ssStopped:
      begin
        StatusEdit.Text := '중지됨.';
        StartAction.Enabled := true;
      end;
    ssStartPending:
      StatusEdit.Text := '시작 중...';
    ssStopPending:
      StatusEdit.Text := '멈추는 중...';
    ssRunning:
      begin
        StatusEdit.Text := '시작됨.';
        StopAction.Enabled := true;
      end;
    ssContinuePending:
      StatusEdit.Text := '계속 중...';
    ssPausePending:
      StatusEdit.Text := '일시정지 중...';
    ssPaused:
      StatusEdit.Text := '일시정지됨.';
  end;

end;

procedure ThreadSafe(AMethod: TThreadMethod); overload;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    AMethod
  else
    TThread.Queue(nil, AMethod);
end;

procedure ThreadSafe(AThreadProc: TThreadProcedure); overload;
begin
{$IFDEF CONSOLE}
  AThreadProc;
{$ELSE}
  if TThread.CurrentThread.ThreadID = MainThreadID then
    AThreadProc
  else
    TThread.Queue(nil, AThreadProc);
{$ENDIF}
end;

function DefaultFormatSettings: TFormatSettings;
begin
{$WARN SYMBOL_PLATFORM OFF}
  Result := TFormatSettings.Create(GetThreadLocale);
{$WARN SYMBOL_PLATFORM ON}
  Result.ShortDateFormat := 'YYYY-MM-DD';
  Result.LongDateFormat := 'YYYY-MM-DD';
  Result.ShortTimeFormat := 'hh:mm:ss';
  Result.LongTimeFormat := 'hh:mm:ss';
  Result.DateSeparator := '-';
  Result.TimeSeparator := ':';
end;

function IdBytesPos(const SubIdBytes, IdBytes: TIdBytes; const AIndex: Integer = 0): Integer;
var
  Index: Integer;
  I: Integer;
begin
  Index := ByteIndex(SubIdBytes[0], IdBytes, AIndex);
  if Index = -1 then
    Exit(-1);

  for I := 0 to Length(SubIdBytes) - 1 do
  begin
    if IdBytes[Index + I] <> SubIdBytes[I] then
      Exit(IdBytesPos(SubIdBytes, IdBytes, Index + I));
  end;
  Result := Index;
end;

function IdBytesToHex(const AValue: TIdBytes; const ASpliter: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    Result := Result + ByteToHex(AValue[I]) + ASpliter;
  end;
end;

function BytesToHex(const AValue: TBytes; const ASpliter: String): String;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    Result := Result + ByteToHex(AValue[I]) + ASpliter;
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

procedure BackupLogFile(AName: string; AMaxSize: Integer = 1024 * 1024 * 5);
begin
  if FileExists(AName) then
  begin
    if JclFileUtils.FileGetSize(AName) > AMaxSize then
    begin
      try
        FileMove(AName, ChangeFileExt(AName, FormatDateTime('_YYYYMMDD_HHNNSS', Now) + '.bak'), true);
      except
        on E: Exception do
          AName := ChangeFileExt(AName, FormatDateTime('_YYYYMMDD', Now) + '.tmp');
      end;
    end;
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

function TruncInt(Value: Integer; Digit: Integer): Integer;
begin
  Result := Trunc(Value / Digit);
  Result := Trunc(Result * Digit);
end;

function CurrentProcessMemory: Cardinal;
var
  MemCounters: TProcessMemoryCounters;
begin
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then
    Result := MemCounters.WorkingSetSize
  else
    Result := 0;
end;

function FileVersion(const FileName: String): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';

  if not TFile.Exists(FileName) then
    Exit;

  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('v%d.%d.%d', [HiWord(dwFileVersionMS),
            // Major
            LoWord(dwFileVersionMS), // Minor
            HiWord(dwFileVersionLS) // Release
            ]);
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;

procedure CloudMessage(const ProjectCode, AppCode, TypeCode, ATitle, AMessage, AVersion: String;
  const AServer: TConnInfo);
var
  UDPClient: TIdUDPClient;
  SysInfo, Msg, DiskInfo: String;
  MBFactor, GBFactor: double;
  _Title: string;
begin
{$IFDEF DEBUG}
  {
    PrintDebug('XCloudLog,<%s> [%s] %s=%s,Host=%s', [TypeCode, AppCode, _Title, Msg,
    AServer.StringValue]);
    Exit;
  }
{$ENDIF}
  MBFactor := 1024 * 1024;
  GBFactor := MBFactor * 1024;

  SysInfo := Format('OS=%s,MemUsage=%.2fMB,TotalMem=%.2fGB,FreeMem=%.2fGB,IPAddress=%s,Server=%s',
    [GetOSVersionString, CurrentProcessMemory / MBFactor, GetTotalPhysicalMemory / GBFactor,
    GetFreePhysicalMemory / GBFactor, GetIPAddress(GetLocalComputerName), AServer.StringValue]);

  DiskInfo := Format('C_Free=%.2fGB,C_Size=%.2fGB,D_Free=%.2fGB,D_Size=%.2fGB',
    [DiskFree(3) / GBFactor, DiskSize(3) / GBFactor, DiskFree(4) / GBFactor, DiskSize(4) / GBFactor]);

  _Title := ATitle.Replace(' ', '_', [rfReplaceAll]);

  Msg := AMessage.Replace('"', '''');
  Msg := Format
    ('CloudLog,ProjectCode=%s,AppCode=%s,TypeCode=%s,ComputerName=%s,Title=%s Version="%s",LogMessage="%s",SysInfo="%s",DiskInfo="%s"',
    [ProjectCode, AppCode, TypeCode, GetLocalComputerName, _Title, AVersion, Msg, SysInfo, DiskInfo]);

  Msg := Msg.Replace('\', '\\');
  Msg := Msg.Replace(#13, ', ');
  Msg := Msg.Replace(#10, '');
  Msg := Msg.Replace(#9, ' '); // TAB

  if AServer.StringValue.IsEmpty then
    Exit;

  UDPClient := TIdUDPClient.Create(nil);
  try
    try
      UDPClient.Send(AServer.StringValue, AServer.IntegerValue, Msg, IndyTextEncoding_UTF8);
      PrintDebug('CloudLog,<%s> [%s] %s=%s,Host=%s', [TypeCode, AppCode, _Title, Msg, AServer.StringValue]);
    except
      on E: Exception do
        PrintDebug('CloudLog,E=' + E.Message);
    end;
  finally
    UDPClient.Free;
  end;
end;

function CompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): boolean;
var
  CS: TZCompressionStream;
begin
  CS := TZCompressionStream.Create(OutStream); // 스트림 생성
  try
    if Assigned(OnProgress) then
      CS.OnProgress := OnProgress;
    CS.CopyFrom(Stream, Stream.Size); // 여기서 압축이 진행됨
    // 테스트 결과 압축완료시 이벤트가 발생하지 않기 때문에
    // 완료시 한번 더 이벤트를 불러준다.
    if Assigned(OnProgress) then
      OnProgress(CS);
    Result := true;
  finally
    CS.Free;
  end;
end;

function Contains(Contents: string; const str: array of const): boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to High(str) do
  begin
    if Pos(str[I].VPWideChar, Contents) = 0 then
      Exit;
  end;

  Result := true;
end;

function IsGoodResponse(Text, Command: string; Response: array of const): boolean;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Text;

    Result := (SL.Strings[0] = Command) and (Contains(Text, Response));
  finally
    SL.Free;
  end;
end;

function DeCompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): boolean;
const
  BuffSize = 65535; // 버퍼 사이즈
var
  DS: TZDeCompressionStream;
  buff: PChar; // 임시 버퍼
  ReadSize: Integer; // 읽은 크기
begin
  if Stream = OutStream then
    // 입력 스트림과 출력스트림이 같으면 문제가 발생한다
    raise Exception.Create('입력 스트림과 출력 스트림이 같습니다');
  Stream.Position := 0;
  // 스트림 커서 초기화
  OutStream.Position := 0;
  // 인풋 스트림을 옵션으로 객체 생성.
  DS := TZDeCompressionStream.Create(Stream);
  try
    if Assigned(OnProgress) then
      DS.OnProgress := OnProgress;
    GetMem(buff, BuffSize);
    try
      // 버퍼 사이즈만큼 읽어온다. Read함수를 부르면 압축이 풀리게 된다.
      repeat
        ReadSize := DS.Read(buff^, BuffSize);
        if ReadSize <> 0 then
          OutStream.Write(buff^, ReadSize);
      until ReadSize < BuffSize;
      if Assigned(OnProgress) then
        OnProgress(DS);
      // Compress와 같은이유
      Result := true;
    finally
      FreeMem(buff)
    end;
  finally
    DS.Free;
  end;
end;

function Rev2Bytes(w: WORD): WORD;
asm
  XCHG   AL, AH
end;

function Rev4Bytes(Value: Int32): Int32; assembler;
asm
  MOV EAX, Value;
  BSWAP    EAX;
end;

function Rev4BytesF(Value: Single): Single; assembler;
var
  tmp1: PInteger;
  tmp2: PSingle;
begin
  tmp1 := @Value;
  tmp1^ := Rev4Bytes(tmp1^);
  tmp2 := @tmp1^;
  Result := tmp2^;
end;

function CheckHexStr(ASource: String): String;
begin
  if (Length(ASource) mod 2) = 0 then
    Result := ASource
  else
    Result := '0' + ASource;
end;

function HexStrToByte(const ASource: String; const AIndex: Integer): Byte;
var
  str: String;
  tmp: TIdBytes;
begin
  str := CheckHexStr(ASource);

  if Length(str) < AIndex + 1 then
  begin
    Result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 2);
  tmp := HexStrToBytes(str);
  CopyMemory(@Result, tmp, 1);
end;

function WordToBytes(AValue: WORD): TIdBytes;
begin
  Result := ToBytes(Rev2Bytes(AValue));
end;

function DWordToBytes(AValue: DWORD): TIdBytes;
begin
  Result := ToBytes(Rev4Bytes(AValue));
end;

function HexStrToWord(const ASource: string; const AIndex: Integer): WORD;
var
  str: string;
begin
  str := CheckHexStr(ASource);

  if Length(str) = 2 then
    str := '00' + str;

  if Length(str) < AIndex + 3 then
  begin
    Result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 4);

{$IF CompilerVersion  > 28} // Ver28 = XE7
  Result := BytesToUInt16(HexStrToBytes(str));
{$ELSE}
  Result := BytesToWord(HexStrToBytes(str));
{$ENDIF}
end;

function HexStrToBytes(const ASource: string; const AIndex: Integer): TIdBytes;
var
  I, j, n: Integer;
  c: char;
  b: Byte;
  str: string;
begin
  str := CheckHexStr(ASource);

  SetLength(Result, 0);

  j := 0;
  b := 0;
  // n := 0;

  for I := AIndex to Length(str) do
  begin
    c := ASource[I];
    case c of
      '0' .. '9':
        n := ord(c) - ord('0');
      'A' .. 'F':
        n := ord(c) - ord('A') + 10;
      'a' .. 'f':
        n := ord(c) - ord('a') + 10;
    else
      Continue;
    end;

    if j = 0 then
    begin
      b := n;
      j := 1;
    end
    else
    begin
      b := (b shl 4) + n;
      j := 0;

      AppendBytes(Result, ToBytes(b));
    end
  end;

  if j <> 0 then
    raise Exception.Create('Input contains an odd number of hexadecimal digits.[' + ASource + '/' +
      IntToStr(AIndex) + ']');
end;

{ TGlobalAbstract }

procedure TGlobalAbstract.ApplicationMessage(const AType: TMessageType; const ATitle: string;
  const AMessage: String);
begin
  if FIsFinalized then
    Exit;

  case AType of
    msDebug:
      _ApplicationMessage(MESSAGE_TYPE_DEBUG, ATitle, AMessage, [moDebugView, moLogFile]);
    msInfo:
      _ApplicationMessage(MESSAGE_TYPE_INFO, ATitle, AMessage);
    msError:
      _ApplicationMessage(MESSAGE_TYPE_ERROR, ATitle, AMessage);
    msWarning:
      _ApplicationMessage(MESSAGE_TYPE_WARNING, ATitle, AMessage);
  else
    _ApplicationMessage(MESSAGE_TYPE_UNKNOWN, ATitle, AMessage);
  end;
end;

procedure TGlobalAbstract.ApplicationMessage(const AType: TMessageType; const ATitle: string;
  const AFormat: String; const Args: array of const);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str);
end;

constructor TGlobalAbstract.Create;
begin
  FProjectCode := 'MyProject';
  FAppCode := 'MyApp';
  FExeName := '';
  FLogName := '';
  FLogServer.StringValue := '';
  FLogServer.IntegerValue := 8094;
  FIsInitialized := False;
  FIsFinalized := False;
  FUseCloudLog := False;
  FUseDebug := False;
end;

procedure TGlobalAbstract.Finalize;
begin
  ApplicationMessage(msInfo, 'Stop', 'StartTime=' + FStartTime.ToString);
  StopLogging;
end;

procedure TGlobalAbstract.FlushLog;

  procedure _PrintLog(ALog: TJdcLog);
  var
    Stream: TStreamWriter;
    LogName: string;
  begin
    LogName := ALog.LogName;
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
  MyLog := FMsgQueue.Dequeue;
  _PrintLog(MyLog);
end;

function TGlobalAbstract.GetLogName: string;
begin
  Result := FLogName;

  // Override this.
  // Result := ChangeFileExt(FLogName, FormatDateTime('_YYYYMMDD', Now) + '.log');
end;

procedure TGlobalAbstract.Initialize;
begin
  FStartTime := Now;
  StartLogging;
{$IFDEF WIN32}
  ApplicationMessage(msInfo, 'Start', '(x86)' + FExeName);
{$ENDIF}
{$IFDEF WIN64}
  ApplicationMessage(msInfo, 'Start', '(x64)' + FExeName);
{$ENDIF}
end;

procedure TGlobalAbstract.AppendLog(const AName: string; const AMsg: string);
begin
  FMsgQueue.Enqueue(TJdcLog.Create(AName, Now, AMsg));
end;

procedure TGlobalAbstract.SetUseDebug(const Value: boolean);
begin
  FUseDebug := Value;
end;

procedure TGlobalAbstract.StartLogging;
begin
  if FMsgQueue = nil then
    FMsgQueue := TQueue<TJdcLog>.Create;

  if FLogTask = nil then
  begin
    FLogTask := TThread.CreateAnonymousThread(
      procedure
      begin
        while not TThread.CurrentThread.CheckTerminated do
        begin
          Sleep(507);
          if FMsgQueue.Count = 0 then
            Continue;

          FlushLog;
        end;
      end);
    FLogTask.FreeOnTerminate := False;
    FLogTask.Start;
  end;
end;

procedure TGlobalAbstract.StopLogging;
begin
  if Assigned(FLogTask) then
  begin
    FLogTask.Terminate;
    FLogTask.WaitFor;
    FreeAndNil(FLogTask);
  end;
  FreeAndNilEx(FMsgQueue);
end;

procedure TGlobalAbstract._ApplicationMessage(const AType: string; const ATitle: string;
const AMessage: String; const AOutputs: TMsgOutputs);
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
    CloudMessage(FProjectCode, FAppCode, AType, ATitle, AMessage, FileVersion(FExeName), FLogServer);
end;

{ TConnInfo }

constructor TConnInfo.Create(AString: string; AInteger: Integer);
begin
  Self.StringValue := AString;
  Self.IntegerValue := AInteger;
end;

function TConnInfo.Equals(const ConnInfo: TConnInfo): boolean;
begin
  Result := Self.StringValue.Equals(ConnInfo.StringValue) and (Self.IntegerValue = ConnInfo.IntegerValue);
end;

function TConnInfo.ToString: string;
begin
  Result := Self.StringValue + ':' + Self.IntegerValue.ToString;
end;

function StrDefault(str: string; Default: string): string;
begin
  if str.IsEmpty then
    Result := Default
  else
    Result := str;
end;

{ TMemoLog }

constructor TMemoLog.Create(AMemo: TMemo; AMsg: string);
begin
  Self.Memo := AMemo;
  Self.Msg := AMsg;
  Self.Time := Now;
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

end.
