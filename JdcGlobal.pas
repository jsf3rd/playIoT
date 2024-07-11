// *******************************************************
//
// Jdc Global Library
//
// Copyright(c) 2020.
//
// jsf3rd@nate.com
//
//
// *******************************************************

unit JdcGlobal;

interface

uses
  Classes, SysUtils, ZLib, IdGlobal, IOUtils,
  System.StrUtils, IdUDPClient, IdContext, IdExceptionCore,
  UITypes, System.Generics.Collections,
  System.Json, REST.Json

{$IFDEF MSWINDOWS}
    , Winapi.Windows, JclFileUtils, Vcl.ExtCtrls, JclSysInfo, Winapi.psAPI, Vcl.StdCtrls,
  JclSvcCtrl, Vcl.ActnList, Vcl.Dialogs, Winapi.Shellapi, JvJclUtils
{$ENDIF}
    ;

type
  IExecuteFunc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    function Execute(AValue: String): T;
  End;

  IExecuteProc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    procedure Execute(AValue: T);
  End;

  TCanData = Array [0 .. 7] of Byte;

  TeCanMessage = packed record
    // TYPE Value
    // 0x04 STD DATA
    // 0x05 STD REMOTE
    // 0x06 EXT DATA
    // 0x07 EXT REMOTE
    // 0xFF Error Info
    _type: Byte;

    Id: UInt32;
    dlc: Byte;
    data: TCanData;
    function ToHex: string;
  end;

  TMessageType = (msDebug, msInfo, msError, msWarning, msSystem);

  TConnInfo = record
    StringValue: string;
    IntegerValue: Integer;
    Enable: Boolean;
    constructor Create(const AString: string; const AInteger: Integer); overload;
    constructor Create(const AHost: string); overload;
    function ToString: string;
    function Equals(const ConnInfo: TConnInfo): Boolean;
  end;

  TClientInfo = record
    Version: string;
    Url: string;
  end;

  TGlobalAbstract = class abstract
  protected
    FProjectCode: string;
    FAppCode: string;
    FPublisher: string;

    FIsInitialized: Boolean;
    FIsFinalized: Boolean;
    FExeName: String;
    FStartTime: TDateTime;

    procedure OnAfterLoggingEvent(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); virtual;
  public
    constructor Create; virtual;

    procedure Initialize; virtual;
    procedure Finalize; virtual;

    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String;
      const AMessage: String = ''); overload; virtual;
    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String; const AFormat: String;
      const Args: array of const); overload;

    property ExeName: String read FExeName;
    property ProjectCode: string read FProjectCode write FProjectCode;
    property AppCode: string read FAppCode write FAppCode;
    property Publisher: string read FPublisher write FPublisher;
  end;

  // 특정 자리수 이하 0 만들기
  // Value : 값, Digit : 자리수
function TruncInt(Value: Integer; Digit: Integer): Integer;

procedure CloudMessage(const ProjectCode, AppCode, TypeCode, ATitle, AMessage, AVersion: String;
  const AServer: TConnInfo);

// 데이터 압축..
function CompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;

// 데이터 압축 해제..
function DeCompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;

// 응답 검사..
function Contains(const Contents: string; const str: array of const): Boolean;
function IsGoodResponse(const Text, Command: string; const Response: array of const): Boolean;

// 매 Word 마다 Reverse
procedure RevEveryWord(const APointer: Pointer; const ASize: Integer; const AIndex: Integer = 0);

{$IFDEF MSWINDOWS}
// Reverse 2Btyes..
function Rev2Bytes(const w: WORD): WORD;

// Reverse 4Btyes..
function Rev4Bytes(const Value: Int32): Int32;

// Reverse 4Btyes..
function Rev4BytesF(const Value: Single): Single;

// Big endian
function WordToBytes(const AValue: WORD): TIdBytes;

// Big endian
function DWordToBytes(const AValue: UInt32): TIdBytes;

{$ENDIF}
// little endian
function HexStrToWord(const ASource: string; const AIndex: Integer = 1): WORD;

function HexStrToByte(const ASource: String; const AIndex: Integer = 1): Byte;
function HexStrToBytes(const ASource: string; const AIndex: Integer = 1): TIdBytes;

function IdBytesToHex(const AValue: TIdBytes; const ASpliter: String = ' '): String;
function BytesToHex(const AValue: TBytes; const ASpliter: String = ' '): String;

function IdBytesPos(const _SubIdBytes, IdBytes: TIdBytes; const AIndex: Integer = 0): Integer;
function SubIdBytes(const Buffer: TIdBytes; const AIndex: Integer = 0; const ALength: Integer = 0): TIdBytes;

function StrDefault(const str: string; const Default: string): string;

function ByteToA94(const AByte: Byte): String;
function A94ToByte(const AValue: String; const AIndex: Integer = 1): Byte;

function IdBytesToA94(const AValue: TIdBytes): string;
function A94ToIdBytes(const str: string): TIdBytes;

function StreamToA94(AStream: TStream): string;
function A94ToStream(const str: string): TStream;

// Thread Safe
procedure ThreadSafe(const AMethod: TThreadMethod); overload;
procedure ThreadSafe(const AThreadProc: TThreadProcedure); overload;

procedure ThreadSafeSync(const AMethod: TThreadMethod); overload;
procedure ThreadSafeSync(const AThreadProc: TThreadProcedure); overload;

procedure FreeAndNilEx(var Obj);

function DefaultFormatSettings: TFormatSettings;

function CurrentProcessMemory: Cardinal;
function FileVersion(const FileName: String): String;

{$IFDEF MSWINDOWS}
function IsFileInUse(const fName: string): Boolean;

// 서비스 관리
procedure StartService(const ServiceName: String; var OldStatus: TJclServiceState;
  const StartAction: TAction);
procedure StopService(const ServiceName: String; var OldStatus: TJclServiceState; const StopAction: TAction;
  hnd: HWND);
procedure UpdateServiceStatus(const ServiceName: String; var OldStatus: TJclServiceState;
  const StartAction, StopAction: TAction; const StatusEdit: TLabeledEdit);
{$ENDIF}
// Integer To Bit String
function IntToBin(Value: Cardinal; Digits: Integer): String;

// JsonValue에서 JsonObject의 Value 값만 추출
procedure ExtractValues(var AList: TStrings; const AValue: TJSONValue);

function PChar2String(AValue: PAnsiChar): WideString;
function String2PChar(AValue: WideString): PAnsiChar;

// JclDebug Rapper
{
  function GetModuleByLevel(const Level: Integer = 0): string;
  function GetLineByLevel(const Level: Integer = 0): Integer;
  function GetProcByLevel(const Level: Integer = 0; const OnlyProcedureName: Boolean = False): string;
  function GetCurrentProc: string;
}
function CopyStream(const AStream: TStream): TMemoryStream;

// 도분초 to Degree
function ConvertDegree(Value: string): double;

function BoolToStr(AValue: Boolean; T: String = 'True'; F: String = 'False'): string;

const
  LOG_PORT = 8094;
  LOCAL_SERVER = '\\localhost';

  COLOR_UDNS_YELLOW = $0030A3D1;

const
  DEBUG_LEVEL_ALL = 0;
  DEBUG_LEVEL_LOG = 1;
  DEBUG_LEVEL_ETC = 2;
  DEBUG_LEVEL_DB = 4;
  DEBUG_LEVEL_Comm = 8;
  DEBUG_LEVEL_JSON = 16;

implementation

uses JdcGlobal.ClassHelper, JdcLogging;

function PChar2String(AValue: PAnsiChar): WideString;
begin
  result := WideString(AnsiString(AValue));
end;

function String2PChar(AValue: WideString): PAnsiChar;
begin
  result := PAnsiChar(AnsiString(AValue));
end;

function BoolToStr(AValue: Boolean; T: String = 'True'; F: String = 'False'): string;
begin
  if AValue then
    result := T
  else
    result := F;
end;

function ConvertDegree(Value: string): double;
var
  frac: double;
begin
  result := StrToFloatDef(Value, 0);
  if result = 0 then
    Exit;

  frac := result - Trunc(result / 100) * 100;
  result := Trunc(result / 100);
  result := result + frac / 60;
end;

function CopyStream(const AStream: TStream): TMemoryStream;
begin
  result := TMemoryStream.Create;
  AStream.Position := 0;
  result.CopyFrom(AStream, AStream.Size);
end;

procedure ExtractValues(var AList: TStrings; const AValue: TJSONValue);
var
  MyPair: TJSONPair;
  MyValue: TJSONValue;
begin
  if AValue is TJSONNumber then
    AList.Add(Format('%f', [TJSONNumber(AValue).AsDouble]))
  else if AValue is TJSONObject then
  begin
    for MyPair in (AValue as TJSONObject) do
      ExtractValues(AList, MyPair.JsonValue);
  end
  else if AValue is TJSONArray then
  begin
    for MyValue in (AValue as TJSONArray) do
      ExtractValues(AList, MyValue);
  end
  else
    raise Exception.Create('ExtractValues,' + AValue.ToString);
end;

procedure RevEveryWord(const APointer: Pointer; const ASize: Integer; const AIndex: Integer);
var
  src: TIdBytes;
  tmp: Byte;
  Index: Integer;
begin
  SetLength(src, ASize);
  // CopyMemory(@src[0], APointer, ASize);
  Move(src[0], APointer^, ASize);
  Index := AIndex;
  while Index + 1 < ASize do
  begin
    tmp := src[Index];
    src[Index] := src[Index + 1];
    src[Index + 1] := tmp;
    Index := Index + 2;
  end;
  // CopyMemory(APointer, @src[0], ASize);
  Move(APointer^, src[0], ASize);
end;

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
  result := S;
end;

procedure FreeAndNilEx(var Obj);
begin
  try
    if Assigned(Pointer(Obj)) then
      FreeAndNil(Pointer(Obj));
  except
    on E: Exception do
    begin
      TLogging.Obj.ApplicationMessage(msError, 'FreeAndNilEx - ' + TObject(Obj).ClassName, E.Message);
    end;
  end;
end;

{$IFDEF MSWINDOWS}

// 파일 사용 유무
// https:// stackoverflow.com/questions/141302/checking-file-is-open-in-delphi
function IsFileInUse(const fName: string): Boolean;
var
  HFileRes: HFILE;
begin
  HFileRes := CreateFile(PChar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  result := (HFileRes = INVALID_HANDLE_VALUE);
  if not result then
    CloseHandle(HFileRes);
end;

procedure StartService(const ServiceName: String; var OldStatus: TJclServiceState;
  const StartAction: TAction);
begin
  OldStatus := ssUnknown;

  StartAction.Enabled := False;
  if StartServiceByName(LOCAL_SERVER, ServiceName) then
    Exit;

  MessageDlg('서비스를 시작하지 못했습니다.', TMsgDlgType.mtWarning, [mbOK], 0);
  StartAction.Enabled := True;
end;

procedure StopService(const ServiceName: String; var OldStatus: TJclServiceState; const StopAction: TAction;
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
  const StartAction, StopAction: TAction; const StatusEdit: TLabeledEdit);
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
        StartAction.Enabled := True;
      end;
    ssStartPending:
      StatusEdit.Text := '시작 중...';
    ssStopPending:
      StatusEdit.Text := '멈추는 중...';
    ssRunning:
      begin
        StatusEdit.Text := '시작됨.';
        StopAction.Enabled := True;
      end;
    ssContinuePending:
      StatusEdit.Text := '계속 중...';
    ssPausePending:
      StatusEdit.Text := '일시정지 중...';
    ssPaused:
      StatusEdit.Text := '일시정지됨.';
  end;
end;

{$ENDIF}

function CurrentProcessMemory: Cardinal;
{$IFDEF MSWINDOWS}
var
  MemCounters: TProcessMemoryCounters;
{$ENDIF}
begin
  result := 0;
{$IFDEF MSWINDOWS}
  MemCounters.cb := SizeOf(MemCounters);
  if GetProcessMemoryInfo(GetCurrentProcess, @MemCounters, SizeOf(MemCounters)) then
    result := MemCounters.WorkingSetSize
{$ENDIF}
end;

function FileVersion(const FileName: String): String;
{$IFDEF MSWINDOWS}
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
  iLastError: DWORD;
{$ENDIF}
begin
  result := 'v0.1';

{$IFDEF MSWINDOWS}
  if not TFile.Exists(FileName) then
    Exit;

  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  if VerInfoSize = 0 then
  begin
    iLastError := GetLastError;
    TLogging.Obj.ApplicationMessage(msError, 'FileVersion', SysErrorMessage(iLastError));
    Exit;
  end;

  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
    begin
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          result := Format('v%d.%d.%d', [HiWord(dwFileVersionMS),
            // Major
            LoWord(dwFileVersionMS), // Minor
            HiWord(dwFileVersionLS) // Release
            ]);
    end
    else
    begin
      iLastError := GetLastError;
      TLogging.Obj.ApplicationMessage(msError, 'FileVersion', SysErrorMessage(iLastError));
    end;
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
{$ENDIF}
end;

function DefaultFormatSettings: TFormatSettings;
begin
{$WARN SYMBOL_PLATFORM OFF}
{$IFDEF MSWINDOWS}
  result := TFormatSettings.Create(GetThreadLocale);
{$ELSE}
  result := TFormatSettings.Create(SysLocale.DefaultLCID);
{$ENDIF}
{$WARN SYMBOL_PLATFORM ON}
  result.ShortDateFormat := 'YYYY-MM-DD';
  result.LongDateFormat := 'YYYY-MM-DD';
  result.ShortTimeFormat := 'hh:mm:ss';
  result.LongTimeFormat := 'hh:mm:ss';
  result.DateSeparator := '-';
  result.TimeSeparator := ':';
end;

procedure ThreadSafe(const AMethod: TThreadMethod); overload;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    AMethod
  else
    TThread.Queue(nil, AMethod);
end;

procedure ThreadSafe(const AThreadProc: TThreadProcedure); overload;
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

procedure ThreadSafeSync(const AMethod: TThreadMethod); overload;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    AMethod
  else
    TThread.Synchronize(nil, AMethod);
end;

procedure ThreadSafeSync(const AThreadProc: TThreadProcedure); overload;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    AThreadProc
  else
    TThread.Synchronize(nil, AThreadProc);
end;

function IdBytesPos(const _SubIdBytes, IdBytes: TIdBytes; const AIndex: Integer = 0): Integer;
var
  Index: Integer;
  I: Integer;
begin
  result := -1;
  if Length(_SubIdBytes) = 0 then
    Exit;

  Index := ByteIndex(_SubIdBytes[0], IdBytes, AIndex);
  if Index = -1 then
    Exit;

  for I := 0 to Length(_SubIdBytes) - 1 do
  begin
    if IdBytes[Index + I] <> _SubIdBytes[I] then
      Exit(IdBytesPos(_SubIdBytes, IdBytes, Index + I));
  end;

  result := Index;
end;

function SubIdBytes(const Buffer: TIdBytes; const AIndex: Integer = 0; const ALength: Integer = 0): TIdBytes;
var
  _Length: Integer;
begin
  _Length := Length(Buffer);
  if ALength = 0 then
    SetLength(result, _Length - AIndex)
  else
    SetLength(result, ALength);

  CopyTIdBytes(Buffer, AIndex, result, 0, Length(result));
end;

function IdBytesToHex(const AValue: TIdBytes; const ASpliter: String): String;
var
  I: Integer;
begin
  result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    result := result + ByteToHex(AValue[I]) + ASpliter;
  end;
end;

function BytesToHex(const AValue: TBytes; const ASpliter: String): String;
var
  I: Integer;
begin
  result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    result := result + ByteToHex(AValue[I]) + ASpliter;
  end;
end;

function TruncInt(Value: Integer; Digit: Integer): Integer;
begin
  result := Trunc(Value / Digit);
  result := Trunc(result * Digit);
end;

procedure CloudMessage(const ProjectCode, AppCode, TypeCode, ATitle, AMessage, AVersion: String;
  const AServer: TConnInfo);
var
  UDPClient: TIdUDPClient;
  SysInfo, Msg, DiskInfo: String;
  _Title: string;
  ComName: string;
{$IFDEF MSWINDOWS}
  MBFactor, GBFactor: double;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  MBFactor := 1024 * 1024;
  GBFactor := MBFactor * 1024;
  SysInfo := Format('OS=%s,MemUsage=%.2fMB,TotalMem=%.2fGB,FreeMem=%.2fGB,IPAddress=%s,Server=%s',
    [GetOSVersionString, CurrentProcessMemory / MBFactor, GetTotalPhysicalMemory / GBFactor,
    GetFreePhysicalMemory / GBFactor, GetIPAddress(GetLocalComputerName), AServer.StringValue]);
  DiskInfo := Format('C_Free=%.2fGB,C_Size=%.2fGB,D_Free=%.2fGB,D_Size=%.2fGB',
    [DiskFree(3) / GBFactor, DiskSize(3) / GBFactor, DiskFree(4) / GBFactor, DiskSize(4) / GBFactor]);
  ComName := GetLocalComputerName;
{$ELSE}
  SysInfo := Format('Server=%S ', [AServer.StringValue]);
  DiskInfo := '';
  ComName := 'MyCom';
{$ENDIF}
  _Title := ATitle.Replace(' ', '_', [rfReplaceAll]);

  Msg := AMessage.Replace('"', '''');
  Msg := Format
    ('CloudLog,ProjectCode=%s,AppCode=%s,TypeCode=%s,ComputerName=%s,Title=%s Version="%s",LogMessage="%s",SysInfo="%s",DiskInfo="%s"',
    [ProjectCode, AppCode, TypeCode, ComName, _Title, AVersion, Msg, SysInfo, DiskInfo]);

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
      PrintDebug('[%s] <%s> [%s] %s=%s,Host=%s', ['CloudLog', TypeCode, AppCode, _Title, Msg,
        AServer.StringValue]);
    except
      on E: Exception do
        PrintDebug('[%s] Server=%s,E=%s', ['CloudLog', AServer.ToString, E.Message]);
    end;
  finally
    UDPClient.Free;
  end;
end;

function CompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;
var
  CS: TZCompressionStream;
begin
  CS := TZCompressionStream.Create(OutStream); // 스트림 생성
  try
    if Assigned(OnProgress) then
      CS.OnProgress := OnProgress;
    CS.CopyFrom(Stream, Stream.Size);
    // 여기서 압축이 진행됨
    // 테스트 결과 압축완료시 이벤트가 발생하지 않기 때문에
    // 완료시 한번 더 이벤트를 불러준다.
    if Assigned(OnProgress) then
      OnProgress(CS);
    result := True;
  finally
    CS.Free;
  end;
end;

function Contains(const Contents: string; const str: array of const): Boolean;
var
  I: Integer;
begin
  result := False;

  for I := 0 to High(str) do
  begin
    if Pos(str[I].VPWideChar, Contents) = 0 then
      Exit;
  end;

  result := True;
end;

function IsGoodResponse(const Text, Command: string; const Response: array of const): Boolean;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Text;

    result := (SL.Strings[0] = Command) and (Contains(Text, Response));
  finally
    SL.Free;
  end;
end;

function DeCompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;
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
      result := True;
    finally
      FreeMem(buff)
    end;
  finally
    DS.Free;
  end;
end;

{$IFDEF MSWINDOWS}

function Rev2Bytes(const w: WORD): WORD;
asm
  XCHG   AL, AH
end;

function Rev4Bytes(const Value: Int32): Int32; assembler;
asm
  MOV EAX, Value;
  BSWAP    EAX;
end;

function Rev4BytesF(const Value: Single): Single; assembler;
var
  tmp1: PInteger;
  tmp2: PSingle;
begin
  tmp1 := @Value;
  tmp1^ := Rev4Bytes(tmp1^);
  tmp2 := @tmp1^;
  result := tmp2^;
end;

function WordToBytes(const AValue: WORD): TIdBytes;
begin
  result := ToBytes(Rev2Bytes(AValue));
end;

function DWordToBytes(const AValue: UInt32): TIdBytes;
begin
  result := ToBytes(Rev4Bytes(AValue));
end;

{$ENDIF}

function CheckHexStr(ASource: String): String;
begin
  if (Length(ASource) mod 2) = 0 then
    result := ASource
  else
    result := '0' + ASource;
end;

function HexStrToByte(const ASource: String; const AIndex: Integer): Byte;
var
  str: String;
  tmp: TIdBytes;
begin
  str := CheckHexStr(ASource);

  if Length(str) < AIndex + 1 then
  begin
    result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 2);
  tmp := HexStrToBytes(str);
  // CopyMemory(@Result, tmp, 1);
  Move(result, tmp, 1);
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
    result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 4);

{$IF CompilerVersion  > 28} // Ver28 = XE7
  result := BytesToUInt16(HexStrToBytes(str));
{$ELSE}
  result := BytesToWord(HexStrToBytes(str));
{$ENDIF}
end;

function HexStrToBytes(const ASource: string; const AIndex: Integer): TIdBytes;
var
  I, j, n: Integer;
  c: Char;
  b: Byte;
  str: string;
begin
  str := CheckHexStr(ASource);

  SetLength(result, 0);

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

      AppendBytes(result, ToBytes(b));
    end
  end;

  if j <> 0 then
    raise Exception.Create('Input contains an odd number of hexadecimal digits.[' + ASource + '/' +
      IntToStr(AIndex) + ']');
end;

{ TGlobalAbstract }

procedure TGlobalAbstract.ApplicationMessage(const AType: TMessageType; const ATitle, AFormat: String;
  const Args: array of const);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str);
end;

procedure TGlobalAbstract.ApplicationMessage(const AType: TMessageType; const ATitle, AMessage: String);
begin
  TLogging.Obj.ApplicationMessage(AType, ATitle, AMessage);
end;

constructor TGlobalAbstract.Create;
begin
  FStartTime := Now;
  FProjectCode := 'MyProject';
  FAppCode := 'MyApp';
  FPublisher := 'UDNS';
  FExeName := ParamStr(0);
  FIsInitialized := False;
  FIsFinalized := False;

  TLogging.Obj.OnAfterLogging := OnAfterLoggingEvent;
end;

procedure TGlobalAbstract.Finalize;
begin
  TLogging.Obj.ApplicationMessage(msInfo, 'Stop', 'StartTime=' + FStartTime.ToString);
  TLogging.Obj.StopLogging;
end;

procedure TGlobalAbstract.Initialize;
begin
  TLogging.Obj.StartLogging;
{$IFDEF WIN32}
  TLogging.Obj.ApplicationMessage(msInfo, 'Start', '(x86)' + FExeName);
{$ENDIF}
{$IFDEF WIN64}
  TLogging.Obj.ApplicationMessage(msInfo, 'Start', '(x64)' + FExeName);
{$ENDIF}
  TLogging.Obj.PrintUseDebug;
  TLogging.Obj.PrintUseCloudLog;
end;

procedure TGlobalAbstract.OnAfterLoggingEvent(const AType: TMessageType; const ATitle, AMessage: String);
begin
  // null method;
end;

{ TConnInfo }

constructor TConnInfo.Create(const AString: string; const AInteger: Integer);
begin
  Self.StringValue := AString;
  Self.IntegerValue := AInteger;
  Self.Enable := True;
end;

constructor TConnInfo.Create(const AHost: string);
begin
  Self := TConnInfo.Create(AHost.Split([':'])[0], AHost.Split([':'])[1].ToInteger);
end;

function TConnInfo.Equals(const ConnInfo: TConnInfo): Boolean;
begin
  result := Self.StringValue.Equals(ConnInfo.StringValue) and (Self.IntegerValue = ConnInfo.IntegerValue);
end;

function TConnInfo.ToString: string;
begin
  result := Self.StringValue + ':' + Self.IntegerValue.ToString;
end;

function StrDefault(const str: string; const Default: string): string;
begin
  if str.IsEmpty then
    result := Default
  else
    result := str;
end;

function ByteToA94(const AByte: Byte): String;
begin
  if AByte < 93 then
    result := Chr(32 + AByte)
  else if AByte < 175 then
    result := Chr(125) + Char(32 + AByte - 92)
  else
    result := Chr(126) + Char(32 + AByte - 174)
end;

function A94ToByte(const AValue: String; const AIndex: Integer = 1): Byte;
var
  c: Char;
  b: Byte;
  calc: Integer;
begin
  c := AValue[AIndex];
  b := ord(c);

  calc := 256;
  if b < 125 then
    calc := b - 32
  else if b = 125 then
  begin
    if Length(AValue) >= AIndex + 1 then
      calc := 92 + A94ToByte(AValue, AIndex + 1);
  end
  else if b = 126 then
  begin
    if Length(AValue) >= AIndex + 1 then
      calc := 174 + A94ToByte(AValue, AIndex + 1);
  end;

  if calc > 255 then
    raise Exception.Create('This is not A94 Format.(' + AValue + ')');

  result := calc;
end;

function IdBytesToA94(const AValue: TIdBytes): string;
var
  I: Integer;
begin
  result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    result := result + ByteToA94(AValue[I]);
  end;
end;

function A94ToIdBytes(const str: string): TIdBytes;
var
  Index: Integer;
  b: Byte;
begin
  SetLength(result, 0);

  Index := 1;
  while Index <= Length(str) do
  begin
    b := A94ToByte(str, Index);
    AppendByte(result, b);

    if b > 92 then
      Index := Index + 2
    else
      Index := Index + 1
  end;
end;

function StreamToA94(AStream: TStream): string;
var
  buff: TIdBytes;
begin
  if AStream = nil then
    Exit('');

  SetLength(buff, AStream.Size);
  AStream.Position := 0;
  AStream.Read(buff[0], AStream.Size);
  result := IdBytesToA94(buff);
  SetLength(buff, 0);
end;

function A94ToStream(const str: string): TStream;
var
  buff: TIdBytes;
begin
  if str = '' then
    Exit(nil);

  buff := A94ToIdBytes(str);
  result := TMemoryStream.Create;
  result.Write(buff[0], Length(buff));
end;

{ TeCanMessage }

function TeCanMessage.ToHex: string;
var
  I: Integer;
begin
  result := '';
  for I := Low(Self.data) to High(Self.data) do
    result := result + ' ' + ByteToHex(Self.data[I]);

  result := IntToHex(Self.Id) + ' ' + ByteToHex(Self.dlc) + result;
end;

end.
