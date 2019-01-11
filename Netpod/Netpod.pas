{ ******************************************************* }
{ }
{ NETPOD 컴포넌트 }
{ }
{ Copyright (C) 2019 playIoT }
{ by jsf3rd }
{ }
{ ******************************************************* }

unit Netpod;

interface

uses
  SysUtils, Classes, Windows, Messages, Registry, ProcessViewer,
  System.Generics.Collections, Math, DllInc, System.DateUtils;

const
  NETPOD_VERSION = '2.1.0.0'; // 컴포넌트 버전
  PODMNG = 'PODMNG.EXE';

type
  TPodData = array of array of Single;

  TNetpod = class;
  TBeforeReceiveData = procedure(Sender: TObject; const Pid: Integer) of Object;
  TAfterReceiveData = procedure(Sender: TObject; const Pid: Integer; const SDate: TDateTime;
    const Data: TPodData; const SampleCount: Integer; var Accept: boolean) of Object;
  TOnLog = procedure(Sender: TObject; AType: string; AMsg: string) of Object;

  TNetpod = class(TComponent)
  private
    FDLLHandle: THandle;

    FVersion: string;
    FOwner: TComponent;
    FLatestSample: TDictionary<Integer, Int64>; // 마지막 읽은 샘플 번호

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
    function Scanned: boolean;
    function IsRunning: boolean;
    function IsCallback: boolean;
    procedure ManualScan;
    procedure ConnectNetpod(IP: string);
    procedure ConnectNDACS(IP: string);
    procedure ManualConnect(IsNetpod: boolean; IP: string);
    procedure KillPodMng;
    function GetAutoScanOnStartupPodmng: boolean;
    procedure SetAutoScanOnStartupPodmng(const Value: boolean);
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

constructor TNetpod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FLatestSample := TDictionary<Integer, Int64>.Create;

  FLocked := false;
  FVersion := NETPOD_VERSION;
  FSampleRate := 100;

  LoadDLL;
end;

destructor TNetpod.Destroy;
begin
  FreeDLL;

  inherited;
end;

procedure TNetpod.FreeDLL;
begin
  if FDLLHandle <> 0 then
  begin
    try
      FDLLHandle := 0;
    except
      on E: Exception do
    end;
  end;
end;

function TNetpod.GetBufInfo(Pid: Integer): TBufParamStruct;
begin
  FNP_GetBufParam(Pid, result);
end;

{ -------------------------------------------------------------------------
  계측 데이터 취득 메인
  ------------------------------------------------------------------------- }
procedure TNetpod.ReceiveManual(Pid, ChCount: Integer; DateTime: TDateTime; LateSample: Int64;
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

procedure TNetpod.ReceiveNextData(Pid: Integer; Count: Integer);

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
  // result := false;

  if Count = 0 then
  begin
    _OnLog('WARNING', 'No Channel');
    exit;
  end;

  // ------------------------------------------------------------------------------
  // 버퍼 정보 읽기
  // ------------------------------------------------------------------------------
  FNP_GetBufParam(Pid, Info);
  {
    if rlt > 0 then
    begin
    _OnLog('ERROR', 'FNP_GetBufParam=' + rlt.ToString+','+Info.ToString);
    exit;
    end;
  }
  _OnLog('ERROR', 'FNP_GetBufParam=' + Info.ToString);

  if FLatestSample.ContainsKey(Pid) then
    SampleCount := Info.LatestSample - FLatestSample.Items[Pid]
  else
  begin
    FLatestSample.Add(Pid, Info.LatestSample - HexTimesValue(FSampleRate));
    SampleCount := Min(Info.SampleCount, HexTimesValue(FSampleRate));
  end;

  if (SampleCount mod 16) <> 0 then
    SampleCount := Min(Info.SampleCount, HexTimesValue(SampleCount));

  if SampleCount = 0 then
    exit;

  SetLength(Data, Count, SampleCount);
  // 배열 초기화
  for i := 0 to Count - 1 do
    ZeroMemory(Data[i], SizeOf(Data[i]));

  // 데이터 취득전 이벤트 호출
  if @FOnBeforeReceiveData <> nil then
    FOnBeforeReceiveData(Self, Pid);

  // 각 채널의 데이터를 취득
  _OnLog('DEBUG', Format('Pid=%d,Latest=%d,Count=%d,Date=%s', [Pid, Info.LatestSample,
    SampleCount, Info.LatestDateTimeStr]));

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
    FLatestSample.Items[Pid] := Info.LatestSample
  else
    _OnLog('ERROR', 'Denyed, ' + Info.LatestDateTimeStr);
end;

procedure TNetpod.LoadDLL;
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

procedure TNetpod.Lock;
begin
  FLocked := True;
end;

procedure TNetpod.UnLock;
begin
  FLocked := false;
end;

procedure TNetpod._OnLog(AType, AMsg: string);
begin
  if Assigned(FOnLog) then
    FOnLog(Self, AType, AMsg);
end;

{ -------------------------------------------------------------------------
  네트워크에서 넷포드 검색
  ------------------------------------------------------------------------- }
procedure TNetpod.Scan;
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
procedure TNetpod.RunPodMng;
begin
  if not IsRunning then
    FNP_SetStatus(NP_RUNPODMNG);
end;

{ -------------------------------------------------------------------------
  넷포드 검색되었는지 여부
  ------------------------------------------------------------------------- }
function TNetpod.Scanned: boolean;
begin
  // 실행 중 podmng.exe가 종료되어도 true를 반환함
  result := (FNP_GetStatus(NP_ISINITSCAN) <> 0);
end;

{ -------------------------------------------------------------------------
  포드매니져 실행 여부
  ------------------------------------------------------------------------- }
function TNetpod.IsRunning: boolean;
begin
  result := (FNP_GetStatus(NP_ISRUNNING) <> 0) and IsFileActive(UpperCase(PODMNG));
end;

function TNetpod.IsCallback: boolean;
begin
  result := FNP_GetStatus(NP_ISCALLBACK) <> 0;
end;

{ -------------------------------------------------------------------------
  계측 시작
  ------------------------------------------------------------------------- }
procedure TNetpod.Run;
begin
  FNP_SetStatus(NP_STARTRUN);
end;

{ -------------------------------------------------------------------------------
  계측 종료
  ------------------------------------------------------------------------------- }
procedure TNetpod.Stop;
begin
  FNP_SetStatus(NP_STOPRUN);
end;

procedure TNetpod.SetVersion(const Value: string);
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
procedure TNetpod.ManualConnect(IsNetpod: boolean; IP: string);
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
procedure TNetpod.ConnectNDACS(IP: string);
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
procedure TNetpod.ConnectNetpod(IP: string);
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
procedure TNetpod.ManualScan;
var
  h: HWND;
begin
  h := FindWindow(nil, 'NetPod Configuration');

  if h = 0 then
    exit;

  PostMessage(h, WM_COMMAND, 18, 0); // spy++로 찾았음^^
end;

function TNetpod.PodList: TArray<Integer>;
begin
  SetLength(result, 100);
  FNP_GetPodList(result);
end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.KillPodMng
  작성자 : isul
  작성일 : 2007.06.19
  인자   : None
  결과   : None
  설명   : 포드 매니져 강제 종료
  ------------------------------------------------------------------------------- }
procedure TNetpod.KillPodMng;
var
  h: HWND;
begin
  {
    if FOwner is TForm then
    begin

    h := FindWindow(nil, PChar((FOwner as TForm).Caption));
    if h <> 0 then
    PostMessage(h, WM_QUIT, 0, 0);
    end;
  }

  h := FindWindow('PodMngClass', 'Pod Manager');
  if h = 0 then
    exit;

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
function TNetpod.GetAutoScanOnStartupPodmng: boolean;
var
  reg: TRegistry;
  sl: TStringList;
  i: Integer;
begin
  result := false;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod\InitCommands', True);
    sl := TStringList.Create;
    reg.GetValueNames(sl);
    for i := 0 to sl.Count - 1 do
      if reg.ReadString(sl.Strings[i]) = 'SCANNET D' then
      begin
        result := True;
        exit;
      end;
    sl.Free;
  finally
    reg.CloseKey;
    reg.Free;
  end;
end;

function TNetpod.GetStatus(stat: Integer): Integer;
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
procedure TNetpod.SetAutoScanOnStartupPodmng(const Value: boolean);
var
  reg: TRegistry;
  sl: TStringList;
  i: Integer;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod\InitCommands', True);
    sl := TStringList.Create;
    reg.GetValueNames(sl);
    for i := 0 to sl.Count - 1 do
      if reg.ReadString(sl.Strings[i]) = 'SCANNET D' then
        exit;

    if Value then
      reg.WriteString('Command' + IntToStr(sl.Count), 'SCANNET D');
    sl.Free;
  finally
    reg.CloseKey;
    reg.Free;
  end;
end;

function TNetpod.SetStatus(stat: Integer): Integer;
begin
  result := FNP_SetStatus(stat);
end;

end.
