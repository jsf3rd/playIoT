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
  Classes, SysUtils, Winapi.Windows, ZLib, IdGlobal, IOUtils, JclFileUtils, Vcl.ExtCtrls,
  System.StrUtils, IdUDPClient, JclSysInfo, Winapi.psAPI, IdContext, IdExceptionCore, Vcl.StdCtrls,
  JclSvcCtrl, Vcl.ActnList, Vcl.Dialogs, Winapi.Shellapi, UITypes, System.Generics.Collections,
  System.Json, REST.Json, JvJclUtils;

type
  IExecuteFunc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    function Execute(AValue: String): T;
  End;

  IExecuteProc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    procedure Execute(AValue: T);
  End;

  TMessageType = (msDebug, msInfo, msError, msWarning, msSystem);

  TConnInfo = record
    StringValue: string;
    IntegerValue: Integer;
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
    procedure ApplicationMessage(const AType: TMessageType; const ATitle: String;
      const AFormat: String; const Args: array of const); overload;

    property ExeName: String read FExeName;
    property ProjectCode: string read FProjectCode write FProjectCode;
    property AppCode: string read FAppCode write FAppCode;
  end;

  // Ư�� �ڸ��� ���� 0 �����
  // Value : ��, Digit : �ڸ���
function TruncInt(Value: Integer; Digit: Integer): Integer;

function CurrentProcessMemory: Cardinal;
function FileVersion(const FileName: String): String;
procedure CloudMessage(const ProjectCode, AppCode, TypeCode, ATitle, AMessage, AVersion: String;
  const AServer: TConnInfo);

// ������ ����..
function CompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;

// ������ ���� ����..
function DeCompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;

// ���� �˻�..
function Contains(const Contents: string; const str: array of const): Boolean;
function IsGoodResponse(const Text, Command: string; const Response: array of const): Boolean;

// �� Word ���� Reverse
procedure RevEveryWord(const APointer: Pointer; const ASize: Integer; const AIndex: Integer = 0);

// Reverse 2Btyes..
function Rev2Bytes(const w: WORD): WORD;

// Reverse 4Btyes..
function Rev4Bytes(const Value: Int32): Int32;

// Reverse 4Btyes..
function Rev4BytesF(const Value: Single): Single;

// Big endian
function WordToBytes(const AValue: WORD): TIdBytes;

// Big endian
function DWordToBytes(const AValue: DWORD): TIdBytes;

// little endian
function HexStrToWord(const ASource: string; const AIndex: Integer = 1): WORD;

function HexStrToByte(const ASource: String; const AIndex: Integer = 1): Byte;
function HexStrToBytes(const ASource: string; const AIndex: Integer = 1): TIdBytes;

function IdBytesToHex(const AValue: TIdBytes; const ASpliter: String = ' '): String;
function BytesToHex(const AValue: TBytes; const ASpliter: String = ' '): String;

function IdBytesPos(const SubIdBytes, IdBytes: TIdBytes; const AIndex: Integer = 0): Integer;

function DefaultFormatSettings: TFormatSettings;

function StrDefault(const str: string; const Default: string): string;

// Thread Safe
procedure ThreadSafe(const AMethod: TThreadMethod); overload;
procedure ThreadSafe(const AThreadProc: TThreadProcedure); overload;

procedure ThreadSafeSync(const AMethod: TThreadMethod); overload;
procedure ThreadSafeSync(const AThreadProc: TThreadProcedure); overload;

procedure FreeAndNilEx(var Obj);

// ���� ����
procedure StartService(const ServiceName: String; var OldStatus: TJclServiceState;
  const StartAction: TAction);
procedure StopService(const ServiceName: String; var OldStatus: TJclServiceState;
  const StopAction: TAction; hnd: HWND);
procedure UpdateServiceStatus(const ServiceName: String; var OldStatus: TJclServiceState;
  const StartAction, StopAction: TAction; const StatusEdit: TLabeledEdit);

// Integer To Bit String
function IntToBin(Value: Cardinal; Digits: Integer): String;

// JsonValue���� JsonObject�� Value ���� ����
procedure ExtractValues(var AList: TStrings; const AValue: TJSONValue);

// JclDebug Rapper
{
  function GetModuleByLevel(const Level: Integer = 0): string;
  function GetLineByLevel(const Level: Integer = 0): Integer;
  function GetProcByLevel(const Level: Integer = 0; const OnlyProcedureName: Boolean = False): string;
  function GetCurrentProc: string;
}
function IsFileInUse(const fName: string): Boolean;
function CopyStream(const AStream: TStream): TMemoryStream;

const
  LOG_PORT = 8094;
  LOCAL_SERVER = '\\localhost';

const
  DEBUG_LEVEL_ALL = 0;
  DEBUG_LEVEL_LOG = 1;
  DEBUG_LEVEL_ETC = 2;
  DEBUG_LEVEL_DB = 4;
  DEBUG_LEVEL_Comm = 8;
  DEBUG_LEVEL_JSON = 16;

implementation

uses JdcGlobal.ClassHelper, JdcLogging;

function CopyStream(const AStream: TStream): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  AStream.Position := 0;
  Result.LoadFromStream(AStream);
end;

// ���� ��� ����
// https:// stackoverflow.com/questions/141302/checking-file-is-open-in-delphi
function IsFileInUse(const fName: string): Boolean;
var
  HFileRes: HFILE;
begin
  HFileRes := CreateFile(PChar(fName), GENERIC_READ or GENERIC_WRITE, 0, nil, OPEN_EXISTING,
    FILE_ATTRIBUTE_NORMAL, 0);
  Result := (HFileRes = INVALID_HANDLE_VALUE);
  if not Result then
    CloseHandle(HFileRes);
end;

{
  function GetModuleByLevel(const Level: Integer): string;
  begin
  Result := ModuleByLevel(Level + 1) + '';
  end;

  function GetLineByLevel(const Level: Integer): Integer;
  begin
  Result := LineByLevel(Level + 1) + 0;
  end;

  function GetCurrentProc: string;
  begin
  Result := ProcByLevel(1, True) + '';
  end;

  function GetProcByLevel(const Level: Integer; const OnlyProcedureName: Boolean): string;
  begin
  // �ٷ� ���� �ϴ� ��� Release���� Level + 1 �����ϴ� ���� �߻�
  Result := ProcByLevel(Level + 1, OnlyProcedureName) + '';
  end;
}
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
  CopyMemory(@src[0], APointer, ASize);

  Index := AIndex;
  while Index + 1 < ASize do
  begin
    tmp := src[Index];
    src[Index] := src[Index + 1];
    src[Index + 1] := tmp;
    Index := Index + 2;
  end;
  CopyMemory(APointer, @src[0], ASize);
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
  Result := S;
end;

procedure FreeAndNilEx(var Obj);
begin
  try
    if Assigned(Pointer(Obj)) then
      FreeAndNil(Pointer(Obj));
  except
    on E: Exception do
    begin
      TLogging.Obj.ApplicationMessage(msError, 'FreeAndNilEx - ' + TObject(Obj).ClassName,
        E.Message);
    end;
  end;
end;

procedure StartService(const ServiceName: String; var OldStatus: TJclServiceState;
  const StartAction: TAction);
begin
  OldStatus := ssUnknown;

  StartAction.Enabled := False;
  if StartServiceByName(LOCAL_SERVER, ServiceName) then
    Exit;

  MessageDlg('���񽺸� �������� ���߽��ϴ�.', TMsgDlgType.mtWarning, [mbOK], 0);
  StartAction.Enabled := True;
end;

procedure StopService(const ServiceName: String; var OldStatus: TJclServiceState;
  const StopAction: TAction; hnd: HWND);
begin
  OldStatus := ssUnknown;
  StopAction.Enabled := False;
  if StopServiceByName(LOCAL_SERVER, ServiceName) then
    Exit;

  if MessageDlg('�˸� : ���񽺸� �������� ���߽��ϴ�.' + #13#10 + '������ �����Ͻðڽ��ϱ�?', TMsgDlgType.mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
    ShellExecute(hnd, 'open', 'taskkill', PWideChar(' -f -im ' + ServiceName + '.exe'),
      nil, SW_HIDE);
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
      StatusEdit.Text := '�˼�����(��ϵ� ���񽺰� �����ϴ�).';
    ssStopped:
      begin
        StatusEdit.Text := '������.';
        StartAction.Enabled := True;
      end;
    ssStartPending:
      StatusEdit.Text := '���� ��...';
    ssStopPending:
      StatusEdit.Text := '���ߴ� ��...';
    ssRunning:
      begin
        StatusEdit.Text := '���۵�.';
        StopAction.Enabled := True;
      end;
    ssContinuePending:
      StatusEdit.Text := '��� ��...';
    ssPausePending:
      StatusEdit.Text := '�Ͻ����� ��...';
    ssPaused:
      StatusEdit.Text := '�Ͻ�������.';
  end;

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
  iLastError: DWORD;
begin
  Result := 'v0.1';

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
          Result := Format('v%d.%d.%d', [HiWord(dwFileVersionMS),
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
end;

procedure CloudMessage(const ProjectCode, AppCode, TypeCode, ATitle, AMessage, AVersion: String;
  const AServer: TConnInfo);
var
  UDPClient: TIdUDPClient;
  SysInfo, Msg, DiskInfo: String;
  MBFactor, GBFactor: double;
  _Title: string;
begin
  MBFactor := 1024 * 1024;
  GBFactor := MBFactor * 1024;

  SysInfo := Format('OS=%s,MemUsage=%.2fMB,TotalMem=%.2fGB,FreeMem=%.2fGB,IPAddress=%s,Server=%s',
    [GetOSVersionString, CurrentProcessMemory / MBFactor, GetTotalPhysicalMemory / GBFactor,
    GetFreePhysicalMemory / GBFactor, GetIPAddress(GetLocalComputerName), AServer.StringValue]);

  DiskInfo := Format('C_Free=%.2fGB,C_Size=%.2fGB,D_Free=%.2fGB,D_Size=%.2fGB',
    [DiskFree(3) / GBFactor, DiskSize(3) / GBFactor, DiskFree(4) / GBFactor,
    DiskSize(4) / GBFactor]);

  _Title := ATitle.Replace(' ', '_', [rfReplaceAll]);

  Msg := AMessage.Replace('"', '''');
  Msg := Format
    ('CloudLog,ProjectCode=%s,AppCode=%s,TypeCode=%s,ComputerName=%s,Title=%s Version="%s",LogMessage="%s",SysInfo="%s",DiskInfo="%s"',
    [ProjectCode, AppCode, TypeCode, GetLocalComputerName, _Title, AVersion, Msg, SysInfo,
    DiskInfo]);

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
  CS := TZCompressionStream.Create(OutStream); // ��Ʈ�� ����
  try
    if Assigned(OnProgress) then
      CS.OnProgress := OnProgress;
    CS.CopyFrom(Stream, Stream.Size);
    // ���⼭ ������ �����
    // �׽�Ʈ ��� ����Ϸ�� �̺�Ʈ�� �߻����� �ʱ� ������
    // �Ϸ�� �ѹ� �� �̺�Ʈ�� �ҷ��ش�.
    if Assigned(OnProgress) then
      OnProgress(CS);
    Result := True;
  finally
    CS.Free;
  end;
end;

function Contains(const Contents: string; const str: array of const): Boolean;
var
  I: Integer;
begin
  Result := False;

  for I := 0 to High(str) do
  begin
    if Pos(str[I].VPWideChar, Contents) = 0 then
      Exit;
  end;

  Result := True;
end;

function IsGoodResponse(const Text, Command: string; const Response: array of const): Boolean;
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

function DeCompressStream(Stream: TStream; OutStream: TStream; OnProgress: TNotifyEvent): Boolean;
const
  BuffSize = 65535; // ���� ������
var
  DS: TZDeCompressionStream;
  buff: PChar; // �ӽ� ����
  ReadSize: Integer; // ���� ũ��
begin
  if Stream = OutStream then
    // �Է� ��Ʈ���� ��½�Ʈ���� ������ ������ �߻��Ѵ�
    raise Exception.Create('�Է� ��Ʈ���� ��� ��Ʈ���� �����ϴ�');
  Stream.Position := 0;
  // ��Ʈ�� Ŀ�� �ʱ�ȭ
  OutStream.Position := 0;
  // ��ǲ ��Ʈ���� �ɼ����� ��ü ����.
  DS := TZDeCompressionStream.Create(Stream);
  try
    if Assigned(OnProgress) then
      DS.OnProgress := OnProgress;
    GetMem(buff, BuffSize);
    try
      // ���� �����ŭ �о�´�. Read�Լ��� �θ��� ������ Ǯ���� �ȴ�.
      repeat
        ReadSize := DS.Read(buff^, BuffSize);
        if ReadSize <> 0 then
          OutStream.Write(buff^, ReadSize);
      until ReadSize < BuffSize;
      if Assigned(OnProgress) then
        OnProgress(DS);
      // Compress�� ��������
      Result := True;
    finally
      FreeMem(buff)
    end;
  finally
    DS.Free;
  end;
end;

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

function WordToBytes(const AValue: WORD): TIdBytes;
begin
  Result := ToBytes(Rev2Bytes(AValue));
end;

function DWordToBytes(const AValue: DWORD): TIdBytes;
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

procedure TGlobalAbstract.ApplicationMessage(const AType: TMessageType;
  const ATitle, AFormat: String; const Args: array of const);
var
  str: string;
begin
  FmtStr(str, AFormat, Args);
  ApplicationMessage(AType, ATitle, str);
end;

procedure TGlobalAbstract.ApplicationMessage(const AType: TMessageType;
  const ATitle, AMessage: String);
begin
  TLogging.Obj.ApplicationMessage(AType, ATitle, AMessage);
end;

constructor TGlobalAbstract.Create;
begin
  FStartTime := Now;
  FProjectCode := 'MyProject';
  FAppCode := 'MyApp';
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

procedure TGlobalAbstract.OnAfterLoggingEvent(const AType: TMessageType;
  const ATitle, AMessage: String);
begin
  // null method;
end;

{ TConnInfo }

constructor TConnInfo.Create(const AString: string; const AInteger: Integer);
begin
  Self.StringValue := AString;
  Self.IntegerValue := AInteger;
end;

constructor TConnInfo.Create(const AHost: string);
begin
  Self.StringValue := AHost.Split([':'])[0];
  Self.IntegerValue := AHost.Split([':'])[1].ToInteger;
end;

function TConnInfo.Equals(const ConnInfo: TConnInfo): Boolean;
begin
  Result := Self.StringValue.Equals(ConnInfo.StringValue) and
    (Self.IntegerValue = ConnInfo.IntegerValue);
end;

function TConnInfo.ToString: string;
begin
  Result := Self.StringValue + ':' + Self.IntegerValue.ToString;
end;

function StrDefault(const str: string; const Default: string): string;
begin
  if str.IsEmpty then
    Result := Default
  else
    Result := str;
end;

end.
