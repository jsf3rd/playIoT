unit JdcNetSDK;

interface

uses System.Classes, System.SysUtils, Winapi.Windows, System.IOUtils, System.Generics.Collections,
  JdcLogging, JdcGlobal, System.DateUtils, Vcl.ExtCtrls, Vcl.Forms, Vcl.Controls, System.Threading;

const
  DEFAULT_CHANNEL = 1;

  NETSDK_LIVE_STREAM_INDEX_MAIN = 0;
  NETSDK_LIVE_STREAM_INDEX_SUB = 1;
  NETSDK_LIVE_STREAM_INDEX_THIRD = 2;

  NETSDK_TRANSPROTOCOL_RTPUDP = 0;
  NETSDK_TRANSPROTOCOL_RTPTCP = 1;

  NETSDK_EXCEPTION_NO_DATA = 'No Data(3387)';

type
  TPlayerAbstract = class;
  TOnException = procedure(const AInfo: TPlayerAbstract; const AMsg: String) of object;

  TSnapConfig = record
    IP: string;
    Port: Int32;
    ID: string;
    PW: string;
    CutNum: string;
    StreamType: Int32;
  end;

  TCCTVInfo = record
    Enable: Boolean;
    IP: AnsiString;
    Port: Int32;
    ID: AnsiString;
    Password: AnsiString;

    constructor Create(AConfig: TSnapConfig);
    function Equals(AInfo: TCCTVInfo): Boolean;
  end;

  TPlayerAbstract = class abstract
  private
    FOnException: TOnException;
  protected
    FID: string;
    FLoggingIn: Boolean;
    FUserID: IntPtr;
    FRealHandle: IntPtr;
    FCCTVInfo: TCCTVInfo;
    FRetryCount: Int32;

    FTick: Cardinal;

    FData: TStream;
    FLastCapture: Cardinal;

    FPlayControl: TWinControl;
    FChannel: Int32;
    FStreamType: Integer;

    FAliveTask: TThread;

    procedure CheckAlive;
    procedure SetData(const Value: TStream);

    function _StopRealPlay: Boolean; virtual; abstract;
    function _Logout: Boolean; virtual; abstract;
    function _RealPlay: IntPtr; virtual; abstract;
  public
    constructor Create(AID: String);
    destructor Destroy; override;
    procedure Reset;

    function Played: Boolean; virtual;
    function Logined: Boolean; virtual;

    function Login: Boolean; virtual; abstract;
    function Logout: Boolean;
    function RealPlay: Boolean; overload;
    function RealPlay(AControl: TWinControl; AChannel: Int32 = DEFAULT_CHANNEL;
      AStreamType: Int32 = NETSDK_LIVE_STREAM_INDEX_MAIN): Boolean; overload;
    function StopRealPlay: Boolean;
    function CapturePicture(const szFileName: String): Boolean; virtual; abstract;

    function ToString: string; override;
    function GetLastError: String; virtual; abstract;

    property UserID: IntPtr read FUserID write FUserID;
    property CCTVInfo: TCCTVInfo read FCCTVInfo write FCCTVInfo;
    property ID: string read FID;
    property Retry: Int32 read FRetryCount write FRetryCount;
    property Tick: Cardinal read FTick write FTick;
    property LastCapture: Cardinal read FLastCapture write FLastCapture;
    property Data: TStream read FData write SetData;
    property LoggingIn: Boolean read FLoggingIn write FLoggingIn;

    property OnException: TOnException read FOnException write FOnException;
  end;

  TNetAbstract = class abstract
  private
    FOnException: TOnException;
    procedure FreeDLL;
  protected
    FInited: Boolean;
    FDLLHandle: THandle;
    FPlayerDic: TDictionary<String, TPlayerAbstract>;

    function InitLib: Boolean; virtual; abstract;
    procedure LoadDLL(const APath: string); virtual; abstract;
    function SetLogPath(const szPath: string): Boolean; virtual;
    function CreatePlayer(const AID: string): TPlayerAbstract; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    function Initialize(const ALibPath: string; const ALogPath: string): Boolean;

    function GetLastError: string; virtual; abstract;
    function Cleanup: Boolean; virtual; abstract;

    function AddPlayer(const AID: String): TPlayerAbstract;
    function GetPlayer(const AID: String): TPlayerAbstract;
    procedure DelPlayer(const AID: String);
    function GetKeys: TArray<String>;
    function GetPlayers: TArray<TPlayerAbstract>;

    property OnException: TOnException read FOnException write FOnException;
  end;

implementation

{ TCCTVInfo }

constructor TCCTVInfo.Create(AConfig: TSnapConfig);
begin
  Self.Enable := True;
  Self.IP := AnsiString(AConfig.IP);
  Self.Port := AConfig.Port;
  Self.ID := AnsiString(AConfig.ID);
  Self.Password := AnsiString(AConfig.PW);
end;

function TCCTVInfo.Equals(AInfo: TCCTVInfo): Boolean;
begin
  Result := (Self.IP = AInfo.IP) and (Self.Port = AInfo.Port) and (Self.ID = AInfo.ID) and
    (Self.Password = AInfo.Password)
end;

{ TPlayerInfo }

constructor TPlayerAbstract.Create(AID: String);
begin
  FLoggingIn := False;
  FID := AID;
  FUserID := -1;
  FRealHandle := -1;
  FRetryCount := 0;
  FTick := 0;
  FData := TMemoryStream.Create;
  FLastCapture := 0;
  Reset;

  FAliveTask := TThread.CreateAnonymousThread(
    procedure
    begin
      try
        CheckAlive();
      except
        on E: Exception do
          TLogging.Obj.ApplicationMessage(msError, 'CheckAlive', E.Message);
      end;

    end);
  FAliveTask.FreeOnTerminate := False;
  FAliveTask.Start;
end;

destructor TPlayerAbstract.Destroy;
begin
  if Assigned(FAliveTask) then
  begin
    FAliveTask.Terminate;
    FAliveTask.WaitFor;
    FreeAndNil(FAliveTask);
  end;

  Self.StopRealPlay;
  Self.Logout;

  if Assigned(FData) then
    FreeAndNil(FData);

  inherited;
end;

function TPlayerAbstract.Logined: Boolean;
begin
  Result := FUserID > 0;
end;

function TPlayerAbstract.Logout: Boolean;
begin
  Result := False;
  if not Logined then
    Exit;

  if Self.Played then
    Self.StopRealPlay;

  Result := _Logout;
  if Result then
  begin
    TLogging.Obj.ApplicationMessage(msInfo, 'Net_Logout', 'ID=%s,UserID=%d', [FID, FUserID]);
    Reset;
  end
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'Net_Logout', 'ID=%s,E=%s', [FID, GetLastError]);
end;

function TPlayerAbstract.Played: Boolean;
begin
  Result := FRealHandle > 0;
end;

procedure TPlayerAbstract.CheckAlive();
begin
  while not TThread.CurrentThread.CheckTerminated do
  begin
    Sleep(501);

    if not Self.Played then
      Continue;

    if Abs(GetTickCount - FTick) < 5000 then
      Continue;

    FTick := GetTickCount;

    // No Data Àç½ÃÀÛ!
    if Assigned(FOnException) then
      FOnException(Self, NETSDK_EXCEPTION_NO_DATA);
  end;
end;

function TPlayerAbstract.RealPlay: Boolean;
begin
  Result := RealPlay(FPlayControl, FChannel, FStreamType);
end;

function TPlayerAbstract.RealPlay(AControl: TWinControl; AChannel, AStreamType: Int32): Boolean;
begin
  if not Self.Logined then
    raise Exception.Create('Logout');

  if Self.Played then
    StopRealPlay;

  ThreadSafeSync(
    procedure
    begin
      FPlayControl := AControl;
      FPlayControl.Invalidate;
      FChannel := AChannel;
      FStreamType := AStreamType;
      FRealHandle := _RealPlay;
    end);

  Result := Self.Played;
  if Result then
  begin
    FTick := GetTickCount;
    FRetryCount := 0;
    TLogging.Obj.ApplicationMessage(msInfo, 'Net_RealPlay', 'ID=%s,UserID=%d,RealHandle=%d',
      [FID, FUserID, FRealHandle]);
  end
  else
  begin
    FRetryCount := FRetryCount + 1;
    TLogging.Obj.ApplicationMessage(msWarning, 'Net_RealPlay', 'ID=%s,UserID=%d,TryCount=%d,E=%s',
      [FID, FUserID, FRetryCount, GetLastError]);
  end;
end;

procedure TPlayerAbstract.Reset;
begin
  FUserID := -1;
  FRealHandle := -1;
end;

procedure TPlayerAbstract.SetData(const Value: TStream);
begin
  if Assigned(FData) then
    FreeAndNil(FData);
  FData := Value;
end;

function TPlayerAbstract.StopRealPlay: Boolean;
var
  ret: Boolean;
begin
  Result := True;
  if not Self.Played then
    Exit;

  ThreadSafeSync(
    procedure
    begin
      ret := _StopRealPlay;
      if Assigned(FPlayControl) then
        FPlayControl.Invalidate;
    end);

  if ret then
  begin
    TLogging.Obj.ApplicationMessage(msInfo, 'StopRealPlay', 'ID=%d,RealHandle=%d', [FUserID, FRealHandle]);
    FRealHandle := -1;
  end
  else
  begin
    Result := False;
    TLogging.Obj.ApplicationMessage(msWarning, 'NH_StopRealPlay', 'ID=%d,RealHandle=%d,E=%s',
      [FUserID, FRealHandle, GetLastError]);
  end;
end;

function TPlayerAbstract.ToString: string;
begin
  Result := format('ID=%s', [FID]);
end;

{ TNetAbstract }

function TNetAbstract.AddPlayer(const AID: String): TPlayerAbstract;
begin
  if not FPlayerDic.ContainsKey(AID) then
  begin
    Result := Self.CreatePlayer(AID);
    Result.OnException := Self.OnException;
    FPlayerDic.add(AID, Result);
  end
  else
    Result := FPlayerDic.Items[AID];
end;

constructor TNetAbstract.Create;
begin
  FInited := False;
  FDLLHandle := 0;
  FPlayerDic := TDictionary<String, TPlayerAbstract>.Create;
end;

procedure TNetAbstract.DelPlayer(const AID: String);
begin
  if not FPlayerDic.ContainsKey(AID) then
    Exit;
  FPlayerDic.Items[AID].Free;
  FPlayerDic.Remove(AID);
end;

destructor TNetAbstract.Destroy;
var
  MyPlayer: TPlayerAbstract;
begin
  try
    Cleanup;
    for MyPlayer in FPlayerDic.Values do
    begin
      MyPlayer.OnException := nil;
      MyPlayer.Free;
    end;
    FPlayerDic.Free;
  except
    on E: Exception do
  end;
  FreeDLL;

  inherited;
end;

procedure TNetAbstract.FreeDLL;
begin
  if FDLLHandle > 0 then
  begin
    try
      TLogging.Obj.ApplicationMessage(msDebug, 'NET_FreeDLL', 'Handle=%d', [FDLLHandle]);
      FDLLHandle := 0;
    except
      on E: Exception do
    end;
  end;
end;

function TNetAbstract.GetKeys: TArray<String>;
begin
  Result := FPlayerDic.Keys.ToArray;
end;

function TNetAbstract.GetPlayer(const AID: String): TPlayerAbstract;
begin
  if FPlayerDic.ContainsKey(AID) then
    Result := FPlayerDic.Items[AID]
  else
    Result := nil;
end;

function TNetAbstract.GetPlayers: TArray<TPlayerAbstract>;
begin
  Result := FPlayerDic.Values.ToArray;
end;

function TNetAbstract.Initialize(const ALibPath, ALogPath: string): Boolean;
begin
  if not TDirectory.Exists(ALibPath) then
    raise Exception.Create('path not exist. [' + ALibPath + ']');

  LoadDLL(ALibPath);
  SetLogPath(ALogPath);

  if not FInited then
    FInited := Self.InitLib;
  Result := FInited;
  if Result then
    TLogging.Obj.ApplicationMessage(msDebug, 'NET_Init')
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'NET_Init', GetLastError);
end;

function TNetAbstract.SetLogPath(const szPath: string): Boolean;
begin
  Result := False;
end;

end.
