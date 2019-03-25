{ ******************************************************* }
{ }
{ NETPOD 컴포넌트 }
{ }
{ Copyright (C) 2019 playIoT }
{ by jsf3rd }
{ }
{ ******************************************************* }

unit _Netpod;

interface

uses
  SysUtils, Classes, Windows, Messages, Registry, ProcessViewer,
  System.Generics.Collections, Math, DllInc, System.DateUtils;

const
  NETPOD_VERSION = '2.1.0.0'; // 컴포넌트 버전
  PODMNG = 'PODMNG.EXE';

type
  TPodData = array of array of Single;

  TNetPod = class;
  TBeforeReceiveData = procedure(Sender: TObject; const Pid: Integer) of Object;
  TAfterReceiveData = procedure(Sender: TObject; const Pid: Integer; const SDate: TDateTime;
    const Data: TPodData; const SampleCount: Integer; var Accept: boolean) of Object;
  TOnLog = procedure(Sender: TObject; AType: string; AMsg: string) of Object;

  TNetPod = class(TComponent)
  private
    FDLLHandle: THandle;

    FVersion: string;
    FOwner: TComponent;
    FLastSample: TDictionary<Integer, Int64>; // 마지막 읽은 샘플 번호

    FNP_GetStatus: TNP_GetStatus;
    FNP_SetStatus: TNP_SetStatus;
    FNP_GetPodInfo: TNP_GetPodInfo;
    FNP_GetChannelInfo: TNP_GetChannelInfo;
    FNP_GetPodList: TNP_GetPodList;
    FNP_ChannelBufRead: TNP_ChannelBufRead;
    FNP_GetBufParam: TNP_GetBufParam;

    FLocked: boolean;
    FSampleRate: Integer;
    FOnBeforeReceiveData: TBeforeReceiveData;
    FOnAfterReceiveData: TAfterReceiveData;
    FOnLog: TOnLog;

    procedure SetVersion(const Value: string);

    procedure LoadDLL;
    procedure FreeDLL;

    procedure _OnLog(AType: string; AMsg: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReceiveNextData(Pid: Integer; Count: Integer);
    procedure ReceiveManual(Pid, ChCount: Integer; DateTime: TDateTime; LateSample: Int64;
      SampleCount: Integer);
    function GetBufInfo(Pid: Integer): TBufParamStruct;

    procedure Lock;
    procedure UnLock;
    procedure Scan;
    procedure RunPodMng;
    procedure Stop;
    procedure Run;
    procedure ScanNet;
    function Scanned: boolean;
    function IsRunning: boolean;
    function IsCallback: boolean;
    procedure ManualScan;
    procedure ConnectNetpod(IP: string);
    procedure ConnectNDACS(IP: string);
    procedure ManualConnect(IsNetpod: boolean; IP: string);
    function IsAlivePodMng: boolean;
    procedure KillPodMng;

    function GetInitCommand(const Index: Integer): String;
    procedure SetInitCommand(const Index: Integer; const Value: String);

    function GetAccess: string;
    procedure SetAccess(const Value: string);

    function SetStatus(stat: Integer): Integer;
    function GetStatus(stat: Integer): Integer;

    function PodList: TArray<Integer>;

  published
    property Locked: boolean read FLocked default false;
    property Version: string read FVersion write SetVersion;
    property SampleRate: Integer read FSampleRate write FSampleRate;

    property OnBeforeReceiveData: TBeforeReceiveData read FOnBeforeReceiveData
      write FOnBeforeReceiveData;
    property OnAfterReceiveData: TAfterReceiveData read FOnAfterReceiveData
      write FOnAfterReceiveData;
    property OnLog: TOnLog read FOnLog write FOnLog;

  end;

implementation

uses
  Dialogs, ScanNetwork;

{ TNetpod }

constructor TNetPod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FLastSample := TDictionary<Integer, Int64>.Create;

  FLocked := false;
  FVersion := NETPOD_VERSION;
  FSampleRate := 100;

  LoadDLL;
end;

destructor TNetPod.Destroy;
begin
  FreeDLL;
  inherited;
end;

procedure TNetPod.FreeDLL;
begin
  if FDLLHandle <> 0 then
  begin
    try
      FreeLibrary(FDLLHandle);
      FDLLHandle := 0;
    except
      on E: Exception do
    end;
  end;
end;

function TNetPod.GetBufInfo(Pid: Integer): TBufParamStruct;
begin
  FNP_GetBufParam(Pid, result);
end;

function TNetPod.GetInitCommand(const Index: Integer): String;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod\InitCommands', True);
    result := reg.ReadString('Command' + Index.ToString);
  finally
    reg.CloseKey;
    reg.Free;
  end;
end;

{ -------------------------------------------------------------------------
  계측 데이터 취득 메인
  ------------------------------------------------------------------------- }
procedure TNetPod.ReceiveManual(Pid, ChCount: Integer; DateTime: TDateTime; LateSample: Int64;
  SampleCount: Integer);
var
  i, rlt: Integer;
  Data: TPodData;
  Accept: boolean;
begin
  SetLength(Data, ChCount, SampleCount);

  for i := 0 to ChCount - 1 do
  begin
    // ------------------------------------------------------------------------------
    // 채널의 데이터 읽기
    // ------------------------------------------------------------------------------
    rlt := FNP_ChannelBufRead( // Read data
      Pid, i, NP_PROC, LateSample - SampleCount, SampleCount, Data[i]);
    if rlt > 0 then
      exit;
  end;

  // ------------------------------------------------------------------------------
  // 전체 데이터 취득 완료 이벤트 호출
  // ------------------------------------------------------------------------------
  if Assigned(@FOnAfterReceiveData) then
    FOnAfterReceiveData(Self, Pid, DateTime, Data, SampleCount, Accept);
end;

procedure TNetPod.ReceiveNextData(Pid: Integer; Count: Integer);

  function HexTimesValue(Value: Int64): Int64; // 가장 가까운 16의 배수
  begin
    result := ((Value div 16) + 1) * 16;
  end;

var
  i: Integer;
  Info: TBufParamStruct;
  SampleCount: Int64;
  Data: TPodData;
  rlt: Integer;

  Accept: boolean;
begin
  if Count = 0 then
  begin
    _OnLog('WARNING', 'No Channel');
    exit;
  end;

  // ------------------------------------------------------------------------------
  // 버퍼 정보 읽기
  // ------------------------------------------------------------------------------
  rlt := FNP_GetBufParam(Pid, Info);
  if rlt > 0 then
  begin
    _OnLog('ERROR', 'FNP_GetBufParam=' + rlt.ToString + ',' + Info.ToString);
    exit;
  end;

  if FLastSample.ContainsKey(Pid) then
  begin
    SampleCount := Info.LatestSample - FLastSample.Items[Pid];
    SampleCount := Min(SampleCount, Info.TotalCount);
  end
  else
  begin
    SampleCount := Min(Info.TotalCount, HexTimesValue(FSampleRate));
    FLastSample.Add(Pid, Info.LatestSample - SampleCount);
  end;

  if (SampleCount mod 16) <> 0 then
    SampleCount := Min(Info.TotalCount, HexTimesValue(SampleCount));

  // _OnLog('DEBUG', Format('Pid=%d,Count=%d,Info=%s', [Pid, SampleCount, Info.ToString]));

  if SampleCount = 0 then
    exit;

  try
    // 배열 초기화
    SetLength(Data, Count, SampleCount);
    for i := 0 to Count - 1 do
      ZeroMemory(Data[i], SizeOf(Data[i]));
  except
    on E: Exception do
    begin
      FLastSample.Items[Pid] := 0;
      raise Exception.Create(Format('E=%s,Pid=%d,SampleCount=%d,LastSample=%d,Info=%s',
        [E.Message, Pid, SampleCount, FLastSample.Items[Pid], Info.ToString]));
    end;
  end;

  // 데이터 취득전 이벤트 호출
  if @FOnBeforeReceiveData <> nil then
    FOnBeforeReceiveData(Self, Pid);

  // 각 채널의 데이터를 취득
  for i := 0 to Count - 1 do
  begin
    // ------------------------------------------------------------------------------
    // 채널의 데이터 읽기
    // ------------------------------------------------------------------------------
    rlt := FNP_ChannelBufRead( // Read data
      Pid, i, NP_PROC, Info.LatestSample - SampleCount, SampleCount, Data[i]);
    if rlt > 0 then
    begin
      _OnLog('ERROR', 'FNP_ChannelBufRead=' + rlt.ToString);
      exit;
    end;
  end;

  // ------------------------------------------------------------------------------
  // 전체 데이터 취득 완료 이벤트 호출
  // ------------------------------------------------------------------------------
  if Assigned(@FOnAfterReceiveData) then
    FOnAfterReceiveData(Self, Pid, Info.LatestDateTime, Data, SampleCount, Accept);
  // 전체 데이터 취득 이벤트 호출

  if Accept then
    FLastSample.Items[Pid] := Info.LatestSample
  else
    _OnLog('DEBUG', 'Denyed, ' + Info.LatestDateTimeStr);
end;

procedure TNetPod.LoadDLL;
begin
  FDLLHandle := LoadLibrary(NETPOD_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  @FNP_GetStatus := GetProcAddress(FDLLHandle, 'NP_GetStatus');
  @FNP_SetStatus := GetProcAddress(FDLLHandle, 'NP_SetStatus');
  @FNP_GetPodInfo := GetProcAddress(FDLLHandle, 'NP_GetPodInfo');
  @FNP_GetChannelInfo := GetProcAddress(FDLLHandle, 'NP_GetChannelInfo');
  @FNP_GetPodList := GetProcAddress(FDLLHandle, 'NP_GetPodList');
  @FNP_ChannelBufRead := GetProcAddress(FDLLHandle, 'NP_ChannelBufRead');
  @FNP_GetBufParam := GetProcAddress(FDLLHandle, 'NP_GetBufParam');
end;

procedure TNetPod.Lock;
begin
  FLocked := True;
end;

procedure TNetPod.UnLock;
begin
  FLocked := false;
end;

procedure TNetPod._OnLog(AType, AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AType, AMsg);
end;

{ -------------------------------------------------------------------------
  네트워크에서 넷포드 검색
  ------------------------------------------------------------------------- }
procedure TNetPod.Scan;
begin
  frmScanNetwork := TfrmScanNetwork.Create(Self);
  try
    frmScanNetwork.ShowModal;
  finally
    if Assigned(frmScanNetwork) then
      frmScanNetwork.Free;
  end;
end;

{ -------------------------------------------------------------------------
  포드매니져 실행
  ------------------------------------------------------------------------- }
procedure TNetPod.RunPodMng;
var
  rlt: Integer;
begin
  if not IsRunning then
  begin
    rlt := FNP_SetStatus(NP_RUNPODMNG);
    _OnLog('DEBUG', 'NP_RUNPODMNG=' + rlt.ToString);
  end;
end;

{ -------------------------------------------------------------------------
  넷포드 검색되었는지 여부
  ------------------------------------------------------------------------- }
function TNetPod.Scanned: boolean;
begin
  // 실행 중 podmng.exe가 종료되어도 true를 반환함
  result := (FNP_GetStatus(NP_ISINITSCAN) <> 0);
end;

procedure TNetPod.ScanNet;
var
  rlt: Integer;
begin
  rlt := FNP_SetStatus(NP_SCANNET);
  _OnLog('DEBUG', 'NP_SCANNET=' + rlt.ToString);
end;

{ -------------------------------------------------------------------------
  포드매니져 동작 여부
  ------------------------------------------------------------------------- }
function TNetPod.IsRunning: boolean;
var
  rlt: Integer;
begin
  rlt := FNP_GetStatus(NP_ISRUNNING);
  result := rlt <> 0;
  // _OnLog('DEBUG', 'NP_ISRUNNING=' + rlt.ToString);
end;

function TNetPod.IsAlivePodMng: boolean;
begin
  result := IsFileActive(UpperCase(PODMNG));
end;

function TNetPod.IsCallback: boolean;
begin
  result := FNP_GetStatus(NP_ISCALLBACK) <> 0;
end;

{ -------------------------------------------------------------------------
  계측 시작
  ------------------------------------------------------------------------- }
procedure TNetPod.Run;
var
  rlt: Integer;
begin
  rlt := FNP_SetStatus(NP_STARTRUN);
  _OnLog('DEBUG', 'NP_STARTRUN=' + rlt.ToString);
end;

{ -------------------------------------------------------------------------------
  계측 종료
  ------------------------------------------------------------------------------- }
procedure TNetPod.Stop;
var
  rlt: Integer;
begin
  rlt := FNP_SetStatus(NP_STOPRUN);
  _OnLog('DEBUG', 'NP_STOPRUN=' + rlt.ToString);
end;

procedure TNetPod.SetVersion(const Value: string);
begin
end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.ConnectNDACS
  작성자 : isul
  작성일 : 2007.06.19
  인자   : IP: string
  결과   : None
  설명   : 포드매니져를 이용하여 Netpod 또는 NDACS에 IP로 연결하기
  ------------------------------------------------------------------------------- }
procedure TNetPod.ManualConnect(IsNetpod: boolean; IP: string);
var
  h, c: HWND;
begin
  // ------------------------------------------------------------------------------
  // 기본 설정 창 찾기
  // ------------------------------------------------------------------------------
  h := FindWindow(nil, 'NetPod Configuration');

  if h = 0 then
    exit;

  // ------------------------------------------------------------------------------
  // IP로 연결하는 창 띄우기
  // ------------------------------------------------------------------------------
  if IsNetpod then
  begin
    PostMessage(h, WM_COMMAND, 22, 0);
    // spy++로 포드매니저의 메뉴 클릭해서 찾아냈음^^
    Sleep(300);
    h := FindWindow(nil, 'ConHost');
  end
  else
  begin
    PostMessage(h, WM_COMMAND, 23, 0); // spy++로 포드매니저의 메뉴 클릭해서 찾아냈음^^
    Sleep(300);
    h := FindWindow(nil, 'Connect to Instrument');
  end;

  // ------------------------------------------------------------------------------
  // 아이피 입력
  // ------------------------------------------------------------------------------
  c := FindWindowEx(h, 0, 'TEdit', nil);
  if c = 0 then
    exit;

  SendMessage(c, WM_SETTEXT, 0, LongInt(PChar(IP)));

  // ------------------------------------------------------------------------------
  // 확인 버튼 클릭
  // ------------------------------------------------------------------------------
  Sleep(100);

  c := FindWindowEx(h, 0, 'TButton', 'Connect');
  if c = 0 then
    exit;

  PostMessage(c, WM_LBUTTONDOWN, 10, 10);
  Sleep(100);
  PostMessage(c, WM_LBUTTONUP, 10, 10);
end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.ConnectNDACS
  작성자 : isul
  작성일 : 2007.06.19
  인자   : IP: string
  결과   : None
  설명   : 포드매니져를 이용하여 NDACS에 IP로 연결하기
  ------------------------------------------------------------------------------- }
procedure TNetPod.ConnectNDACS(IP: string);
begin
  ManualConnect(false, IP);
end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.ConnectNDACS
  작성자 : isul
  작성일 : 2007.06.19
  인자   : IP: string
  결과   : None
  설명   : 포드매니져를 이용하여 Netpod에 IP로 연결하기
  ------------------------------------------------------------------------------- }
procedure TNetPod.ConnectNetpod(IP: string);
begin
  ManualConnect(True, IP);
end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.ManualScan
  작성자 : isul
  작성일 : 2007.06.19
  인자   : None
  결과   : None
  설명   : 포드매니져를 이용한 넷포드 스캔
  ------------------------------------------------------------------------------- }
procedure TNetPod.ManualScan;
var
  h: HWND;
begin
  h := FindWindow(nil, 'NetPod Configuration');

  if h = 0 then
    exit;

  PostMessage(h, WM_COMMAND, 18, 0); // spy++로 찾았음^^
end;

function TNetPod.PodList: TArray<Integer>;
var
  _List: TArray<Integer>;
  MyElem: Integer;
begin
  SetLength(_List, 100);
  FNP_GetPodList(_List);

  SetLength(result, 0);
  for MyElem in _List do
  begin
    if MyElem > 0 then
      result := result + [MyElem]
  end;

end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.KillPodMng
  작성자 : isul
  작성일 : 2007.06.19
  인자   : None
  결과   : None
  설명   : 포드 매니져 강제 종료
  ------------------------------------------------------------------------------- }
procedure TNetPod.KillPodMng;
var
  h: HWND;
begin
  h := FindWindow('PodMngClass', 'Pod Manager');
  if h = 0 then
    exit;

  if Self.IsRunning then
    Self.Stop;

  _OnLog('DEBUG', 'KillPodMng - WM_CLOSE');
  PostMessage(h, WM_CLOSE, 0, 0);
  PostMessage(h, WM_QUIT, 0, 0);
end;

{ -------------------------------------------------------------------------------
  프로시저: TNetpod.GetAutoScanOnStartup
  작    성: isul
  작 성 일: 2007.09.20
  인    자: None
  결    과: Boolean
  설    명: 포드매니져 시작시 자동으로 스캔으로 설정되어 있는지 검사
  ------------------------------------------------------------------------------- }
function TNetPod.GetAccess: string;
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod', True);
    result := reg.ReadString('Access');
  finally
    reg.CloseKey;
    reg.Free;
  end;
end;

function TNetPod.GetStatus(stat: Integer): Integer;
begin
  result := FNP_GetStatus(stat);
end;

{ -------------------------------------------------------------------------------
  프로시저: TNetpod.SetAutoScanOnStartup
  작    성: isul
  작 성 일: 2007.09.20
  인    자: const Value: Boolean
  결    과: None
  설    명: 포드매니져 시작시 자동으로 스캔하도록 설정
  ------------------------------------------------------------------------------- }
procedure TNetPod.SetAccess(const Value: string);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod', True);
    reg.WriteString('Access', Value);
  finally
    reg.CloseKey;
    reg.Free;
  end;

end;

procedure TNetPod.SetInitCommand(const Index: Integer; const Value: String);
var
  reg: TRegistry;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod\InitCommands', True);
    reg.WriteString('Command' + Index.ToString, Value);
  finally
    reg.CloseKey;
    reg.Free;
  end;
end;

function TNetPod.SetStatus(stat: Integer): Integer;
begin
  result := FNP_SetStatus(stat);
end;

end.
