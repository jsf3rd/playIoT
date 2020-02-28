unit JdcDacoM.Server;

interface

uses System.SysUtils, System.Classes, JdcDacoM.Protocol, JdcDacoM.Common, IdContext, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdTCPServer, JdcGlobal, System.Generics.Collections, IdGlobal,
  Winapi.Windows, System.JSON, REST.JSON, JdcGlobal.ClassHelper, System.DateUtils, IdException,
  IdExceptionCore, IdStack, Math;

type
  TMySession = class;

  TOnData = procedure(ASession: TMySession; AID: Integer; const APower: TPowerModule) of object;
  TOnRequest = procedure(AContext: TIdContext; const AParam: TRequestParam) of object;

  TMySession = class
  strict private
    FSessionTime: TDateTime; // 작업 시간
    FRequestTime: TDateTime; // TCP 요청시간

    FModule: TModuleArray;
    FIDList: TIDArray40;
    FCurIndex: Integer;
    FMacAddress: string;

    FOnData: TOnData;
    FOnRequest: TOnRequest;
    FContext: TIdContext;
    FOnLog: TLogProc;
    FUseDebug: Boolean;

    function GetFirst(AIndex: Integer): TModuleFirst;
    procedure SetFirst(AIndex: Integer; const Value: TModuleFirst);
    function GetSecond(AIndex: Integer): TModuleSecond;
    procedure SetSecond(AIndex: Integer; const Value: TModuleSecond);
    function GetModule(AIndex: Integer): TModule;

  public
    constructor Create(AContext: TIdContext);
    destructor Destroy; override;

    function ModuleList: String;
    function GetNextID: Integer;

    procedure OnIDTable(ABuff: TIdBytes);
    procedure OnModuleFirst(ABuff: TIdBytes);
    procedure OnModuleSecond(ABuff: TIdBytes);
    procedure OnPowerModule(ABuff: TIdBytes);
    procedure OnError(ABuff: TIdBytes);

    procedure RequestModule(AID: Integer; AOffSet, ALen: UInt16);
    procedure RequestNextModule;
    procedure RequestPower;
    procedure RequestIDTable;

    procedure Clear;

    property RequestTime: TDateTime read FRequestTime write FRequestTime;
    property IDList: TIDArray40 read FIDList write FIDList;
    property First[AIndex: Integer]: TModuleFirst read GetFirst write SetFirst;
    property Second[AIndex: Integer]: TModuleSecond read GetSecond write SetSecond;
    property Module[AIndex: Integer]: TModule read GetModule;

    property CurIndex: Integer read FCurIndex write FCurIndex;
    property MacAddress: string read FMacAddress write FMacAddress;
    property SessionTime: TDateTime read FSessionTime write FSessionTime;

    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnData: TOnData read FOnData write FOnData;
    property OnLog: TLogProc read FOnLog write FOnLog;
    property UseDebug: Boolean read FUseDebug write FUseDebug;
  end;

  TDacoMServer = class
  strict private
    FTCPServer: TIdTCPServer;
    FOnLog: TLogProc;
    FUseDebug: Boolean;

    FSubList: TList<TOnData>;

    constructor Create;
    procedure TCPServerConnect(AContext: TIdContext);
    procedure TCPServerDisconnect(AContext: TIdContext);
    procedure TCPServerExecute(AContext: TIdContext);
    procedure TCPServerException(AContext: TIdContext; AException: Exception);

    procedure _RequestData(ATime: TDateTime);
    procedure OnRequest(AContext: TIdContext; const AParam: TRequestParam);
    procedure OnData(ASession: TMySession; AID: Integer; const APower: TPowerModule);
    procedure ParsePacket(ASession: TMySession; const ABuff: TIdBytes);

    procedure PrintLog(const AType: TMessageType; const ATitle: String; const AMessage: String = '');
      overload;
    procedure PrintLog(const AType: TMessageType; const ATitle: String; const AFormat: String;
      const Args: array of const); overload;

  public
    class function Obj: TDacoMServer;

    procedure Add(AProc: TOnData);
    procedure Remove(AProc: TOnData);

    procedure Start(APort: Integer = 8900);
    procedure Stop;

    function ClientCount: Integer;

    destructor Destroy; override;

    procedure RequestData(ATime: TDateTime);
    function Started: Boolean;

    property OnLog: TLogProc read FOnLog write FOnLog;
    property UseDebug: Boolean read FUseDebug write FUseDebug;
  end;

implementation

var
  MyObj: TDacoMServer = nil;

const
  READ_TIME_OUT = 3000;

  { TDacoMServer }

procedure TDacoMServer.Add(AProc: TOnData);
begin
  FSubList.Add(AProc);
end;

function TDacoMServer.ClientCount: Integer;
begin
  result := FTCPServer.Contexts.Count;
end;

constructor TDacoMServer.Create;
begin
  FUseDebug := False;
  FTCPServer := TIdTCPServer.Create(nil);
  FTCPServer.OnExecute := TCPServerExecute;
  FTCPServer.OnConnect := TCPServerConnect;
  FTCPServer.OnDisconnect := TCPServerDisconnect;
  FTCPServer.OnException := TCPServerException;

  FSubList := TList<TOnData>.Create;
end;

destructor TDacoMServer.Destroy;
begin
  FreeAndNil(FSubList);
  FreeAndNil(FTCPServer);
  inherited;
end;

class function TDacoMServer.Obj: TDacoMServer;
begin
  if MyObj = nil then
    MyObj := TDacoMServer.Create;
  result := MyObj;
end;

procedure TDacoMServer.OnData(ASession: TMySession; AID: Integer; const APower: TPowerModule);
var
  MyEvent: TOnData;
begin
  if ASession.MacAddress = '' then
  begin
    PrintLog(msWarning, 'NoMac');
    Exit;
  end;

  for MyEvent in FSubList do
    MyEvent(ASession, AID, APower);
end;

procedure TDacoMServer.OnRequest(AContext: TIdContext; const AParam: TRequestParam);
var
  buff: TIdBytes;
  Session: TMySession;
begin
  buff := TModbus.TcpCommand(AParam);
  sleep(11);
  try
    AContext.Write(buff);
    Session := TMySession(AContext.Data);
    Session.RequestTime := Now;
    PrintLog(msDebug, 'SEND', 'Port=%d,Msg=%s', [AContext.PeerPort, IdBytesToHex(buff)]);
  except
    on E: Exception do
      PrintLog(msError, 'SEND', 'IP=%s,buff=%s,E=%s', [AContext.PeerIP, IdBytesToHex(buff), E.Message]);
  end;
end;

procedure TDacoMServer.ParsePacket(ASession: TMySession; const ABuff: TIdBytes);
begin
  case TModbus.GetProtocolType(ABuff) of
    ptIDTable:
      ASession.OnIDTable(ABuff);
    ptModuleFirst:
      ASession.OnModuleFirst(ABuff);
    ptModuleSecond:
      ASession.OnModuleSecond(ABuff);
    ptPowerModule:
      ASession.OnPowerModule(ABuff);
    ptError:
      ASession.OnError(ABuff);
  else
    PrintLog(msWarning, 'WrongProtocol');
  end;
end;

procedure TDacoMServer.PrintLog(const AType: TMessageType; const ATitle, AFormat: String;
  const Args: array of const);
begin
  PrintLog(AType, ATitle, Format(AFormat, Args));
end;

procedure TDacoMServer.Remove(AProc: TOnData);
begin
  FSubList.Remove(AProc);
end;

procedure TDacoMServer.RequestData(ATime: TDateTime);
begin
  try
    _RequestData(ATime);
  except
    on E: Exception do
      PrintLog(msError, 'RequestData', E.Message);
  end;
end;

procedure TDacoMServer.PrintLog(const AType: TMessageType; const ATitle, AMessage: String);
begin
  if not Assigned(FOnLog) then
    Exit;

  if (AType = msDebug) and (FUseDebug = False) then
    Exit;

  FOnLog(AType, ATitle, AMessage);
end;

procedure TDacoMServer.Start(APort: Integer);
begin
  if FTCPServer.Active then
    Exit;

  FTCPServer.DefaultPort := APort;
  FTCPServer.Active := True;
  OnLog(msInfo, 'OpenDacoMServer', Format('Port=%d', [FTCPServer.DefaultPort]));
end;

function TDacoMServer.Started: Boolean;
begin
  result := FTCPServer.Active;
end;

procedure TDacoMServer.Stop;
begin
  if not FTCPServer.Active then
    Exit;

  FTCPServer.Active := False;
  OnLog(msInfo, 'CloseDacoMServer');
end;

procedure TDacoMServer.TCPServerConnect(AContext: TIdContext);
var
  MySession: TMySession;
begin
  PrintLog(msInfo, 'Connected', AContext.PeerInfo);

  MySession := TMySession.Create(AContext);
  MySession.OnRequest := OnRequest;
  MySession.OnData := OnData;
  MySession.OnLog := PrintLog;
  MySession.UseDebug := FUseDebug;

  AContext.Data := MySession;
  AContext.ReadTimeout := READ_TIME_OUT;

  // for MacAddress
  MySession.RequestPower;
end;

procedure TDacoMServer.TCPServerDisconnect(AContext: TIdContext);
begin
  if Assigned(AContext.Data) and (AContext.Data is TMySession) then
    AContext.Data.Free;

  PrintLog(msInfo, 'Disconnected', AContext.PeerInfo);
  AContext.Data := nil;
end;

procedure TDacoMServer.TCPServerException(AContext: TIdContext; AException: Exception);
begin
  if AException is EIdReadTimeout then
  begin
    PrintLog(msWarning, 'ReadTimeout', 'NoResponse.');
    Exit;
  end;

  if AException is EIdConnClosedGracefully then
    Exit;

  if AException is EIdNotConnected then
    Exit;

  if AException is EIdClosedSocket then
    Exit;

  if AException is EIdSocketError then
    Exit;

  PrintLog(msError, 'TCPException', 'E=%S,EClass=%s', [AException.Message, AException.ClassName]);
end;

procedure TDacoMServer.TCPServerExecute(AContext: TIdContext);
var
  buff: TIdBytes;
  Header: TMBAP;
  DataLen: Integer;

  MySession: TMySession;
  tmp: Integer;
begin
  if not Assigned(AContext.Data) then
    raise Exception.Create('NoSession');

  MySession := TMySession(AContext.Data);
  SetLength(buff, 0);
  try
    AContext.ReadBytes(buff, SizeOf(Header));
  except
    on E: EIdReadTimeout do
    begin
      if MySession.RequestTime = 0 then
        Exit;

      if AContext.ReadTimeout = READ_TIME_OUT then
      begin
        tmp := Abs(READ_TIME_OUT - MilliSecondsBetween(Now, MySession.RequestTime));
        if tmp < 10 then
          raise;

        PrintLog(msInfo, 'ExtraWait', 'Port=%d,%dms', [AContext.PeerPort, tmp]);
        AContext.ReadTimeout := Min(tmp, READ_TIME_OUT - 1);
        Exit;
      end;

      raise;
    end;

    on E: Exception do
      raise;
  end;

  PrintLog(msDebug, 'RECV', 'Port=%d,Len=%d,Msg=%s', [AContext.PeerPort, Length(buff), IdBytesToHex(buff)]);

  CopyMemory(@Header, @buff[0], SizeOf(Header));
  Header.Reverse;

  PrintLog(msDebug, 'HEADER', TJson.RecordToJsonString(Header));

  DataLen := Header.Length;
  if TModbus.InvalidDataLen(DataLen) then
  begin
    AContext.FlushBuffer(PrintLog);
    Exit;
  end;

  SetLength(buff, 0);
  AContext.ReadTimeout := READ_TIME_OUT;
  AContext.ReadBytes(buff, DataLen);
  MySession.RequestTime := 0;

  PrintLog(msDebug, 'RECV', 'Port=%d,Len=%d,Msg=%s', [AContext.PeerPort, Length(buff), IdBytesToHex(buff)]);
  ParsePacket(MySession, buff);
end;

procedure TDacoMServer._RequestData(ATime: TDateTime);
var
  List: TList;
  Context: TIdContext;
  MyContext: Pointer;

  MySession: TMySession;
begin
  List := FTCPServer.Contexts.LockList;
  try
    for MyContext in List do
    begin
      Context := TIdContext(MyContext);

      if not Assigned(Context.Data) then
        Continue;

      if not(Context.Data is TMySession) then
        Continue;

      MySession := TMySession(Context.Data);
      MySession.Clear;
      MySession.SessionTime := ATime;
      MySession.RequestNextModule;
    end;
  finally
    FTCPServer.Contexts.UnlockList;
  end;
end;

{ TMySession }

constructor TMySession.Create(AContext: TIdContext);
var
  I: Integer;
begin
  FContext := AContext;
  FUseDebug := False;
  FCurIndex := 0;
  FRequestTime := 0;
  FMacAddress := '';

  for I := Low(FIDList) to High(FIDList) do
  begin
    FIDList[I].Hi := 0;
    FIDList[I].Low := 0;
  end;
end;

destructor TMySession.Destroy;
begin
  inherited;
end;

procedure TMySession.OnError(ABuff: TIdBytes);
var
  ErrorCode: TErrorCode;
begin
  CopyMemory(@ErrorCode, @ABuff[0], SizeOf(TErrorCode));
  OnLog(msDebug, 'Error', TJson.RecordToJsonString(ErrorCode));
end;

function TMySession.GetModule(AIndex: Integer): TModule;
begin
  result := FModule[AIndex];
end;

function TMySession.GetFirst(AIndex: Integer): TModuleFirst;
begin
  result := FModule[AIndex].First;
end;

function TMySession.GetSecond(AIndex: Integer): TModuleSecond;
begin
  result := FModule[AIndex].Second;
end;

procedure TMySession.OnIDTable(ABuff: TIdBytes);
var
  IDTable: TIDTable;
begin
  CopyMemory(@IDTable, @ABuff[0], SizeOf(TIDTable));
  if FUseDebug then
    OnLog(msDebug, 'IDTable', TJson.RecordToJsonString(IDTable));

  CopyMemory(@Self.IDList[0], @IDTable.ID[0], SizeOf(TIDArray40));
  OnLog(msDebug, 'ModuleList', Self.ModuleList);
end;

procedure TMySession.OnModuleFirst(ABuff: TIdBytes);
var
  ModuleFirst: TModuleFirst;
begin
  CopyMemory(@ModuleFirst, @ABuff[0], SizeOf(TModuleFirst));
  ModuleFirst.Data.Reverse;
  Self.First[ModuleFirst.UnitID] := ModuleFirst;

  if FUseDebug then
    OnLog(msDebug, 'First', TJson.RecordToJsonString(ModuleFirst));

  RequestModule(ModuleFirst.UnitID, TModbus.WORD_COUNT_FIRST, TModbus.WORD_COUNT_SECOND);
end;

function TMySession.ModuleList: String;
var
  I: Integer;
begin
  result := '';
  for I := Low(FIDList) to High(FIDList) do
  begin
    if FIDList[I].Hi > 0 then
      result := Format('%s%d,', [result, FIDList[I].Hi]);
  end;
end;

procedure TMySession.OnModuleSecond(ABuff: TIdBytes);
var
  PM: TPowerModule;
  ModuleSecond: TModuleSecond;
begin
  CopyMemory(@ModuleSecond, @ABuff[0], SizeOf(TModuleSecond));
  ModuleSecond.Data.Reverse;
  Self.Second[ModuleSecond.UnitID] := ModuleSecond;

  if FUseDebug then
    OnLog(msDebug, 'Second', TJson.RecordToJsonString(ModuleSecond));

  if ModuleSecond.UnitID = TModbus.MAIN_UNIT_ID then
    RequestPower
  else
  begin
    // 분기모듈
    PM.Header.UnitID := 0; // null object
    FOnData(Self, ModuleSecond.UnitID, PM);

    // Next 데이터 요청
    RequestNextModule;
  end;
end;

procedure TMySession.OnPowerModule(ABuff: TIdBytes);
var
  PowerModule: TPowerModule;
begin
  CopyMemory(@PowerModule, @ABuff[0], SizeOf(TPowerModule));
  PowerModule.Data.Reverse;

  if FUseDebug then
    OnLog(msDebug, 'Power', TJson.RecordToJsonString(PowerModule));

  if FMacAddress = '' then
  begin
    FMacAddress := ToHex(ToBytes(PowerModule.Data.EthernetMac1_1)) + '-' +
      ToHex(ToBytes(PowerModule.Data.EthernetMac1_2));
    OnLog(msDebug, 'MacAddress', FMacAddress);
    RequestIDTable;
    Exit;
  end;

  // 전력미터
  FOnData(Self, TModbus.MAIN_UNIT_ID, PowerModule);
  RequestNextModule;
end;

procedure TMySession.RequestIDTable;
begin
  OnRequest(FContext, TRequestParam.Create(TModbus.IDTABLE_UNITID, TModbus.IDTABLE_ADDRESS,
    TModbus.WORD_COUNT_IDTABLE));
end;

procedure TMySession.RequestModule(AID: Integer; AOffSet, ALen: UInt16);
var
  Addr: UInt16;
  MySession: TMySession;
begin
  if AID <= 0 then
  begin
    MySession := FContext.Data as TMySession;
    if MinuteOf(MySession.SessionTime) = 0 then
      RequestIDTable;
    Exit;
  end;

  Addr := TModbus.GetAddress(AID, TDacoM.DEVICE_1000APS) + AOffSet;
  OnRequest(FContext, TRequestParam.Create(AID, Addr, ALen));
end;

procedure TMySession.RequestNextModule;
begin
  RequestModule(GetNextID, 0, TModbus.WORD_COUNT_FIRST);
end;

procedure TMySession.RequestPower;
begin
  OnRequest(FContext, TRequestParam.Create(TModbus.POWER_UNIT_ID, TModbus.POWER_ADDRESS,
    TModbus.WORD_COUNT_POWER));
end;

procedure TMySession.Clear;
begin
  FCurIndex := -1;
  FillChar(FModule, SizeOf(FModule), #0);
end;

function TMySession.GetNextID: Integer;
var
  I: Integer;
begin
  if Length(FIDList) - 1 <= FCurIndex then
    Exit(-1);

  for I := FCurIndex + 1 to High(FIDList) do
  begin
    if FIDList[I].Hi = 0 then
      Continue;

    FCurIndex := I;
    Exit(FIDList[I].Hi);
  end;
  result := -1;
end;

procedure TMySession.SetFirst(AIndex: Integer; const Value: TModuleFirst);
begin
  FModule[AIndex].First := Value;
end;

procedure TMySession.SetSecond(AIndex: Integer; const Value: TModuleSecond);
begin
  FModule[AIndex].Second := Value;
end;

end.
