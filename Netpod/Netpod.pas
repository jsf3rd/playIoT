{ ******************************************************* }
{ }
{ NETPOD 컴포넌트 }
{ }
{ Copyright (C) 2007 (주)에이티맥스 }
{ by isul }
{ }
{ ******************************************************* }

unit Netpod;

interface

uses
  SysUtils, Classes, Windows, Forms, Messages, Registry, ProcessViewer;

const
  NETPOD_VERSION = '2.0.0.0'; // 컴포넌트 버전
  // MAX_SIZE        = 1024 * 100 - 1;     // 약 17분 저장 가능
  MAX_SIZE = 640 * 100 - 1; // 약 10분 저장 가능
  SAMPLE_SIZE = 1024; // 한 번에 취득할 최대 데이터 크기
  PODMNG = 'PODMNG.EXE';

type
  TChData = packed array [0 .. MAX_SIZE] of Single;
  PChData = ^TChData;

type
  TNetpod = class;
  TBeforeReceiveData = procedure(Sender: TObject) of Object;
  TBeforeReceiveChannelData = procedure(Sender: TObject; Channel: Integer) of Object;
  TAfterReceiveChannelData = procedure(Sender: TObject; Channel: Integer; Data: Pointer;
    SampleNo: Int64; SDate: string; SampleCount: Integer) of Object;
  // TAfterReceiveData = procedure(Sender: TObject; Data: Pointer; SampleNo: Int64; SDate: string) of Object;
  TAfterReceiveData = procedure(Sender: TObject; Data: Pointer) of Object;
  TOnError = procedure(Sender: TObject; ErrMsg: string) of Object;

  TChThread = class(TThread)
  private
    FPod: Integer;
    FChannel: Integer;
    FLength: Integer;
    procedure GetData;
  protected
    procedure Execute; override;
  public
    results: TChData;
    SampleNo: Int64;
    SDateTime: string;
    constructor Create(APod, AChannel, ALength: Integer);
    destructor Destroy; override;
  end;

  TNetpodCollectionItem = class(TCollectionItem)
  private
    FPod: Integer;
    FChannel: Integer;
    FSensorID: Integer;
    FChThread: TChThread;
    FTag: Int64;
    procedure SetPod(const Value: Integer);
    procedure SetChannel(const Value: Integer);
    procedure SetSensorID(const Value: Integer);
    procedure SetTag(const Value: Int64);
  public
    procedure AssignParameter(const APod, AChannel: Integer; ANetpod: TNetpod); virtual;
  published
    destructor Destroy; override;
    property Pod: Integer read FPod write SetPod;
    property Channel: Integer read FChannel write SetChannel;
    property SensorID: Integer read FSensorID write SetSensorID;
    property ChThread: TChThread read FChThread write FChThread;
    property Tag: Int64 read FTag write SetTag;
  end;

  TNetpodCollection = class(TCollection)
  protected
    function GetItem(Index: Integer): TNetpodCollectionItem; virtual;
    procedure SetItem(Index: Integer; Value: TNetpodCollectionItem); virtual;
  public
    constructor Create;
    function IndexOf(const APod, AChannel: Integer): Integer; virtual;
    function IndexOfSensor(const ASensorID: Integer): Integer; virtual;
    function Add: TNetpodCollectionItem;
    procedure AddParameter(const APod, AChannel: Integer; ANetpod: TNetpod);
    procedure DeleteParameter(const idx: Integer); overload;

    property Items[Index: Integer]: TNetpodCollectionItem read GetItem write SetItem;
  end;

  TNetpod = class(TComponent)
  private
    FItems: TNetpodCollection;
    FLength: Integer; // 전체 데이터 크기(분석용)

    FLocked: Boolean;
    FUseThread: Boolean;
    FData: array of array of Single;
    FOnBeforeReceiveData: TBeforeReceiveData;
    FOnBeforeReceiveChannelData: TBeforeReceiveChannelData;
    FOnAfterReceiveChannelData: TAfterReceiveChannelData;
    FOnAfterReceiveData: TAfterReceiveData;
    FOnError: TOnError;
    FVersion: string;
    FOwner: TComponent;
    FLastSample: array of Int64; // 마지막 읽은 샘플 번호
    function SaveChannelData(const Channel: Integer; PResult: Pointer): Boolean; overload;
    function SaveChannelData(const Channel: Integer; PResult: Pointer; const Count: Integer)
      : Boolean; overload;
    procedure SetFLength(Value: Integer);
    procedure SetUseThread(const Value: Boolean);
    procedure SetVersion(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ReceiveLastData: Boolean;
    function ReceiveNextData: Boolean;
    procedure Lock;
    procedure UnLock;
    procedure Scan;
    procedure RunPodMng;
    procedure Stop;
    procedure Run;
    function Scanned: Boolean;
    function IsRunning: Boolean;
    function IsCallback: Boolean;
    function GetBufferCount: Int64;
    procedure ManualScan;
    procedure ConnectNetpod(IP: string);
    procedure ConnectNDACS(IP: string);
    procedure ManualConnect(IsNetpod: Boolean; IP: string);
    procedure KillPodMng;
    procedure CopyItemsFrom(ANetpod: TNetpod);
    function GetAutoScanOnStartupPodmng: Boolean;
    procedure SetAutoScanOnStartupPodmng(const Value: Boolean);

    function PodList: TArray<Integer>;
  published
    property Items: TNetpodCollection read FItems write FItems;
    property Length: Integer read FLength write SetFLength default 1024;
    // property DataLength: Integer read FDataLength write SetFDataLength default 1024;
    property Locked: Boolean read FLocked default false;
    property UseThread: Boolean read FUseThread write SetUseThread default false;
    property Version: string read FVersion write SetVersion;

    property OnBeforeReceiveData: TBeforeReceiveData read FOnBeforeReceiveData
      write FOnBeforeReceiveData;
    property OnBeforeReceiveChannelData: TBeforeReceiveChannelData
      read FOnBeforeReceiveChannelData write FOnBeforeReceiveChannelData;
    property OnAfterReceiveChannelData: TAfterReceiveChannelData
      read FOnAfterReceiveChannelData write FOnAfterReceiveChannelData;
    property OnAfterReceiveData: TAfterReceiveData read FOnAfterReceiveData
      write FOnAfterReceiveData;
    property OnError: TOnError read FOnError write FOnError;
  end;

procedure DebugA(msg: string); overload;
procedure DebugA(const fmt: string; const Args: array of const); overload;

implementation

uses
  Dialogs, DllInc, ScanNetwork;

procedure DebugA(msg: string);
begin
  OutputDebugString(PChar('::TNetpod:: ' + msg));
end;

procedure DebugA(const fmt: string; const Args: array of const);
begin
  DebugA(Format(fmt, Args));
end;

{ TNetpodCollectionItem }

destructor TNetpodCollectionItem.Destroy;
begin
  if FChThread <> nil then
  begin
    FreeAndNil(FChThread);
    DebugA('TNetpod:: 채널 쓰레드[%d-%d] 제거 완료', [Pod, Channel]);
  end;

  inherited;
end;

procedure TNetpodCollectionItem.AssignParameter(const APod, AChannel: Integer;
  ANetpod: TNetpod);
begin
  Pod := APod;
  Channel := AChannel;

  if ANetpod.UseThread then
  begin
    FChThread := TChThread.Create(APod, AChannel, ANetpod.Length);
    DebugA('TNetpod:: 채널 쓰레드[%d-%d] 생성 완료', [APod, AChannel]);
  end;
end;

procedure TNetpodCollectionItem.SetPod(const Value: Integer);
begin
  if FPod <> Value then
    FPod := Value;
end;

procedure TNetpodCollectionItem.SetChannel(const Value: Integer);
begin
  if FChannel <> Value then
    FChannel := Value;
end;

procedure TNetpodCollectionItem.SetSensorID(const Value: Integer);
begin
  if FSensorID <> Value then
    FSensorID := Value;
end;

procedure TNetpodCollectionItem.SetTag(const Value: Int64);
begin
  if FTag <> Value then
    FTag := Value;
end;

{ TNetpodCollection }

function TNetpodCollection.Add: TNetpodCollectionItem;
begin
  Result := TNetpodCollectionItem(inherited Add);
end;

procedure TNetpodCollection.AddParameter(const APod, AChannel: Integer; ANetpod: TNetpod);
begin
  Add.AssignParameter(APod, AChannel, ANetpod);
end;

constructor TNetpodCollection.Create;
begin
  inherited Create(TNetpodCollectionItem);
end;

procedure TNetpodCollection.DeleteParameter(const idx: Integer);
begin
  Items[idx].Free;
end;

function TNetpodCollection.GetItem(Index: Integer): TNetpodCollectionItem;
begin
  Result := TNetpodCollectionItem(inherited GetItem(Index));
end;

function TNetpodCollection.IndexOf(const APod, AChannel: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    if (Items[Result].Pod = APod) and (Items[Result].Channel = AChannel) then
      exit;
  Result := -1;
end;

function TNetpodCollection.IndexOfSensor(const ASensorID: Integer): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].SensorID = ASensorID then
      exit;
  Result := -1;
end;

procedure TNetpodCollection.SetItem(Index: Integer; Value: TNetpodCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

{ TNetpod }

constructor TNetpod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FItems := TNetpodCollection.Create;
  FLength := 1024; // 기본값 설정(다지인타임)

  // FLastSample := 0;

  FLocked := false;
  FUseThread := false;
  FVersion := NETPOD_VERSION;
end;

destructor TNetpod.Destroy;
begin
  SetLength(FData, 0);
  FItems.Free;

  inherited;
end;

{ -------------------------------------------------------------------------
  계측 데이터 취득 메인
  ------------------------------------------------------------------------- }
function TNetpod.ReceiveLastData: Boolean;
var
  i: Integer;
  Info: TBufParamStruct;
  // results: packed array[0..MAX_SIZE] of Single;
  results: TChData;
  Done: Boolean;
  stime: SYSTEMTIME;
  SDate: string;
  SampleNo: Int64;
begin
  Result := false;

  if FItems.Count = 0 then
    exit;

  if @FOnBeforeReceiveData <> nil then
    FOnBeforeReceiveData(Self); // 데이터 취득전 이벤트 호출

  try
    SetLength(FData, FItems.Count, FLength);

    // 각 채널의 데이터를 취득
    for i := 0 to FItems.Count - 1 do
    begin
      if @FOnBeforeReceiveChannelData <> nil then
        FOnBeforeReceiveChannelData(Self, i); // 각 채널 데이터 취득전 이벤트 호출

      if FUseThread then
        FItems.Items[i].ChThread.Resume // 데이터 취득 쓰레드 동작
      else
      begin
        // 버퍼 정보 읽기
        if NP_GetBufParam(FItems.Items[i].Pod, Info) > 0 then
          // Loop for as many channels there are in a specified Pod
          exit;

        SampleNo := Info.LatestSample - FLength;

        FileTimeToSystemTime(Info.LatestTime, stime);
        SDate := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', SystemTimeToDateTime(stime) + 9 /
          24); // 표준시에 9시간 더함
        // SDate := inttostr(stime.wYear) + '-' + inttostr(stime.wMonth) + '-' + inttostr(stime.wDay) + ' ' + inttostr(stime.whour + 9) + ':' + inttostr(stime.wMinute) + ':' + inttostr(stime.wSecond) + ':' + inttostr(stime.wMilliseconds);

        // 채널의 데이터 읽기
        if NP_ChannelBufRead( // Read data
          FItems.Items[i].Pod, FItems.Items[i].Channel, NP_PROC, Info.LatestSample - FLength,
          // 가장 최근의 데이터 FLength개 읽기
          FLength, @results) > 0 then
          exit;

        // DebugA('TNetpod:: Before FOnAfterReceiveChannelData: ' + FloatToStr(results[FLength - 1]);
        if @FOnAfterReceiveChannelData <> nil then
          FOnAfterReceiveChannelData(Self, i, @results, SampleNo, SDate, FLength);
        // 채널 데이터 취득 이벤트 호출

        // 전체 채널 배열에 해당 채널의 데이터 넣기
        SaveChannelData(i, @results);
        Result := true;
      end;

      Application.ProcessMessages;
    end;

    if FUseThread then
    begin
      // 모든 채널의 쓰레드가 데이터를 취득 완료할 때까지 기다림
      Done := false;
      while not Done do
      begin
        Done := true;
        for i := 0 to FItems.Count - 1 do
          Done := Done and FItems.Items[i].ChThread.Suspended;
        Sleep(10);
        Application.ProcessMessages;
      end;

      SampleNo := FItems.Items[FItems.Count - 1].ChThread.SampleNo;
      SDate := FItems.Items[FItems.Count - 1].ChThread.SDateTime;

      // 각 채널의 데이터를 배열에 넣음
      for i := 0 to FItems.Count - 1 do
        SaveChannelData(i, @FItems.Items[i].ChThread.results);
      Result := true;
    end;

    // ------------------------------------------------------------------------------
    // 전체 데이터 취득 완료 이벤트 호출
    // ------------------------------------------------------------------------------
    DebugA('TNetpod:: Before FOnAfterReceiveData');
    if @FOnAfterReceiveData <> nil then
      // FOnAfterReceiveData(Self, FData, SampleNo, SDate);                         // 전체 데이터 취득 이벤트 호출
      FOnAfterReceiveData(Self, FData); // 전체 데이터 취득 이벤트 호출
  finally
    SetLength(FData, 0, 0);
    SetLength(FData, 0);
  end;
end;

{ -------------------------------------------------------------------------
  전체 채널 배열에 해당 채널의 데이터 넣기
  ------------------------------------------------------------------------- }
function TNetpod.SaveChannelData(const Channel: Integer; PResult: Pointer): Boolean;
begin
  Result := true;

  try
    CopyMemory(FData[Channel], PResult, SizeOf(Single) * FLength);
    // CopyMemory(@FData[Channel][0], PResult, SizeOf(Single) * FLength);
  except
    on E: Exception do
    begin
      Result := false;
      if @FOnError <> nil then
        FOnError(Self, Format('SaveChannelData(Channel=%d; PResult) -> %s',
          [Channel, E.Message])); // 에러 이벤트 호출
    end;
  end;
end;

{ -------------------------------------------------------------------------------
  프로시저: TNetpod.ReceiveNextData
  작    성: isul
  작 성 일: 2007.09.20
  인    자: None
  결    과: Boolean
  설    명: 마지막 읽은 다음 데이터부터 끝까지 읽기
  ------------------------------------------------------------------------------- }
function TNetpod.ReceiveNextData: Boolean;
var
  i: Integer;
  Info: TBufParamStruct;
  results: TChData;
  stime, ltime: SYSTEMTIME;
  SDate: string;
  { StartSample, } SampleCount: Int64;
begin
  Result := false;

  // 버퍼에 한 번에 읽을 크기보다 데이터가 적게 있으면 벗어남
  if GetBufferCount < SAMPLE_SIZE then
    exit;

  if FItems.Count = 0 then
    exit;

  if @FOnBeforeReceiveData <> nil then
    FOnBeforeReceiveData(Self); // 데이터 취득전 이벤트 호출

  if High(FData) = -1 then
  begin
    DebugA('FData 메모리 할당');
    SetLength(FData, FItems.Count, FLength);

    // 배열 초기화
    for i := 0 to FItems.Count - 1 do
      ZeroMemory(FData[i], SizeOf(FData[i]));
  end;

  if High(FLastSample) = -1 then
  begin
    DebugA('FLastSample 메모리 할당 및 초기화');
    SetLength(FLastSample, FItems.Count);
    for i := 0 to FItems.Count - 1 do
      FLastSample[i] := 0;
  end;

  Pid := 0;
  // 각 채널의 데이터를 취득
  DebugA('FItems.Count: %d', [FItems.Count]);
  for i := 0 to FItems.Count - 1 do
  begin
    if @FOnBeforeReceiveChannelData <> nil then
      FOnBeforeReceiveChannelData(Self, i); // 각 채널 데이터 취득전 이벤트 호출

    // ------------------------------------------------------------------------------
    // 버퍼 정보 읽기
    // ------------------------------------------------------------------------------
    if NP_GetBufParam(FItems.Items[i].Pod, Info) > 0 then
      // Loop for as many channels there are in a specified Pod
      exit;

    // 계측일 구하기
    FileTimeToSystemTime(Info.LatestTime, stime);
    SDate := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', SystemTimeToDateTime(stime) + 9 / 24);
    // 표준시에 9시간 더함

    // ------------------------------------------------------------------------------
    // 채널의 데이터 읽기
    // ------------------------------------------------------------------------------
    if NP_ChannelBufRead( // Read data
      FItems.Items[i].Pod, FItems.Items[i].Channel, NP_PROC, Info.LatestSample - SAMPLE_SIZE,
      SAMPLE_SIZE, @results) > 0 then
      exit;

    // ------------------------------------------------------------------------------
    // 읽은 데이터에서 저장할 데이터 수 구하기
    // ------------------------------------------------------------------------------
    if FLastSample[i] = 0 then
      SampleCount := SAMPLE_SIZE // 맨 처음에는 전체 데이터 저장
    else
      SampleCount := (Info.LatestSample - FLastSample[i]);

    FLastSample[i] := Info.LatestSample;

    DebugA('%d-%d, SampleCount=%d, FLastSample=%d',
      [FItems.Items[i].Pod, FItems.Items[i].Channel, SampleCount, FLastSample[i]]);

    if SampleCount = 0 then
      Continue;

    // if i = 0 then
    // StartSample := Info.LatestSample - SampleCount;

    // ------------------------------------------------------------------------------
    // 채널 데이터 취득 이벤트 호출 (최근 데이터만 전송)
    // ------------------------------------------------------------------------------
    if @FOnAfterReceiveChannelData <> nil then
    begin
      DebugA('TNetpod:: Before FOnAfterReceiveChannelData(%d): %.6f',
        [i, results[0 { SAMPLE_SIZE - 1 } ]]);
      FOnAfterReceiveChannelData(Self, i, @results[SAMPLE_SIZE - SampleCount],
        Info.LatestSample - SampleCount, SDate, SampleCount); // 채널 데이터 취득 이벤트 호출
    end;

    // ------------------------------------------------------------------------------
    // 전체 채널 배열에 해당 채널의 데이터 넣기
    // ------------------------------------------------------------------------------
    SaveChannelData(i, @results[SAMPLE_SIZE - SampleCount], SampleCount);
    Result := true;

    Application.ProcessMessages;
  end;

  // ------------------------------------------------------------------------------
  // 전체 데이터 취득 완료 이벤트 호출
  // ------------------------------------------------------------------------------
  if (@FOnAfterReceiveData <> nil) then
  begin
    DebugA('Before FOnAfterReceiveData');
    // FOnAfterReceiveData(Self, FData, StartSample, SDate);                         // 전체 데이터 취득 이벤트 호출
    FOnAfterReceiveData(Self, FData); // 전체 데이터 취득 이벤트 호출
  end;
end;

{ -------------------------------------------------------------------------
  전체 채널 배열에 해당 채널의 데이터 넣기
  ------------------------------------------------------------------------- }
function TNetpod.SaveChannelData(const Channel: Integer; PResult: Pointer;
  const Count: Integer): Boolean;
begin
  // DebugA('TNetpod.SaveChannelData(Channel=%d; PResult, Count=%d)', [Channel, Count]);
  Result := true;
  if Count < 0 then
    exit;

  try
    // 데이터 Count만큼 옆으로 밀기
    CopyMemory(@FData[Channel][0], @FData[Channel][Count], SizeOf(Single) * (FLength - Count));

    // 뒤에 새로운 데이터 붙이기
    CopyMemory(@FData[Channel][FLength - Count], PResult, SizeOf(Single) * Count);
  except
    on E: Exception do
    begin
      Result := false;
      if @FOnError <> nil then
        FOnError(Self, Format('SaveChannelData(Channel=%d; PResult; Count=%d) -> %s',
          [Channel, Count, E.Message])); // 에러 이벤트 호출
    end;
  end;
end;

procedure TNetpod.Lock;
begin
  FLocked := true;
end;

procedure TNetpod.UnLock;
begin
  FLocked := false;
end;

procedure TNetpod.SetFLength(Value: Integer);
begin
  if Value > MAX_SIZE then
  begin
    FLength := MAX_SIZE;
    raise Exception.CreateFmt('에러 발생: Length는 %d보다 작아야됩니다.', [MAX_SIZE]);
  end;

  FLength := Value;
end;

procedure TNetpod.SetUseThread(const Value: Boolean);
begin
  FUseThread := Value;
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
    NP_SetStatus(NP_RUNPODMNG);
end;

{ -------------------------------------------------------------------------
  넷포드 검색되었는지 여부
  ------------------------------------------------------------------------- }
function TNetpod.Scanned: Boolean;
begin
  // 실행 중 podmng.exe가 종료되어도 true를 반환함
  Result := (NP_GetStatus(NP_ISINITSCAN) <> 0);
end;

{ -------------------------------------------------------------------------
  포드매니져 실행 여부
  ------------------------------------------------------------------------- }
function TNetpod.IsRunning: Boolean;
begin
  Result := (NP_GetStatus(NP_ISRUNNING) <> 0) and IsFileActive(UpperCase(PODMNG));
end;

function TNetpod.IsCallback: Boolean;
begin
  Result := NP_GetStatus(NP_ISCALLBACK) <> 0;
end;

{ -------------------------------------------------------------------------
  계측 시작
  ------------------------------------------------------------------------- }
procedure TNetpod.Run;
begin
  NP_SetStatus(NP_STARTRUN);
end;

{ -------------------------------------------------------------------------------
  계측 종료
  ------------------------------------------------------------------------------- }
procedure TNetpod.Stop;
begin
  NP_SetStatus(NP_STOPRUN);
end;

{ -------------------------------------------------------------------------------
  모듈명 : TNetpod.GetBufferCount
  작성자 : isul
  작성일 : 2007.06.20
  인자   : None
  결과   : Integer
  설명   : 포드매니져의 버퍼수 구하기
  ------------------------------------------------------------------------------- }
function TNetpod.GetBufferCount: Int64;
var
  Info: TBufParamStruct;
begin
  if FItems.Count = 0 then
    raise Exception.Create('no items');

  NP_GetBufParam(FItems.Items[0].Pod, Info);
  Result := Info.LatestSample - Info.StartSample;
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
procedure TNetpod.ManualConnect(IsNetpod: Boolean; IP: string);
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
    PostMessage(h, WM_COMMAND, 22, 0); // spy++로 포드매니저의 메뉴 클릭해서 찾아냈음^^
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
  ManualConnect(true, IP);
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
  SetLength(Result, 100);
  NP_GetPodList(Result);
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
  모듈명 : TNetpod.CopyItemsFrom
  작성자 : isul
  작성일 : 2007.08.03
  인자   : ANetpodCollection: TNetpodCollection
  결과   : None
  설명   : 아이템 복사
  ------------------------------------------------------------------------------- }
procedure TNetpod.CopyItemsFrom(ANetpod: TNetpod);
var
  i: Integer;
begin
  Self.Items.Clear;
  for i := 0 to ANetpod.Items.Count - 1 do
    with ANetpod.Items.Items[i] do
    begin
      Self.Items.AddParameter(Pod, Channel, ANetpod);
      Self.Items.Items[i].FSensorID := SensorID;
    end;
end;

{ -------------------------------------------------------------------------------
  프로시저: TNetpod.GetAutoScanOnStartup
  작    성: isul
  작 성 일: 2007.09.20
  인    자: None
  결    과: Boolean
  설    명: 포드매니져 시작시 자동으로 스캔으로 설정되어 있는지 검사
  ------------------------------------------------------------------------------- }
function TNetpod.GetAutoScanOnStartupPodmng: Boolean;
var
  reg: TRegistry;
  sl: TStringList;
  i: Integer;
begin
  Result := false;

  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod\InitCommands', true);
    sl := TStringList.Create;
    reg.GetValueNames(sl);
    for i := 0 to sl.Count - 1 do
      if reg.ReadString(sl.Strings[i]) = 'SCANNET D' then
      begin
        Result := true;
        exit;
      end;
  finally
    sl.Free;
    reg.CloseKey;
    reg.Free;
  end;
end;

{ -------------------------------------------------------------------------------
  프로시저: TNetpod.SetAutoScanOnStartup
  작    성: isul
  작 성 일: 2007.09.20
  인    자: const Value: Boolean
  결    과: None
  설    명: 포드매니져 시작시 자동으로 스캔하도록 설정
  ------------------------------------------------------------------------------- }
procedure TNetpod.SetAutoScanOnStartupPodmng(const Value: Boolean);
var
  reg: TRegistry;
  sl: TStringList;
  i: Integer;
begin
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CURRENT_USER;
    reg.LazyWrite := false;
    reg.OpenKey('Software\NetPod\InitCommands', true);
    sl := TStringList.Create;
    reg.GetValueNames(sl);
    i := 0;
    for i := 0 to sl.Count - 1 do
      if reg.ReadString(sl.Strings[i]) = 'SCANNET D' then
        exit;

    if Value then
      reg.WriteString('Command' + IntToStr(i), 'SCANNET D');
  finally
    sl.Free;
    reg.CloseKey;
    reg.Free;
  end;
end;

{ TChThread }

constructor TChThread.Create(APod, AChannel, ALength: Integer);
begin
  inherited Create(true);

  FPod := APod;
  FChannel := AChannel;
  FLength := ALength;
end;

destructor TChThread.Destroy;
begin

  inherited;
end;

procedure TChThread.Execute;
begin
  inherited;

  while not Terminated do
  begin
    GetData;
    Suspend;
    WaitForSingleObject(Handle, 100);
  end;
end;

procedure TChThread.GetData;
var
  Info: TBufParamStruct;
  stime: SYSTEMTIME;
begin
  NP_GetBufParam(FPod, Info);

  SampleNo := Info.LatestSample;
  FileTimeToSystemTime(Info.LatestTime, stime);
  SDateTime := FormatDateTime('yyyy-mm-dd hh:nn:ss:zzz', SystemTimeToDateTime(stime) + 9 / 24);
  // 표준시에 9시간 더함
  // SDateTime := inttostr(stime.wYear) + '-' + inttostr(stime.wMonth) + '-' + inttostr(stime.wDay) + ' ' + inttostr(stime.whour + 9) + ':' + inttostr(stime.wMinute) + ':' + inttostr(stime.wSecond) + ':' + inttostr(stime.wMilliseconds);

  NP_ChannelBufRead( // Read data
    FPod, FChannel, NP_PROC, Info.LatestSample - FLength, FLength, @results);
end;

end.
