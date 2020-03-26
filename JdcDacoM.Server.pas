// *******************************************************
//
// DACO-M 1000P TCP Server
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@e-daco.net
//
// *******************************************************

unit JdcDacoM.Server;

interface

uses System.SysUtils, System.Classes, JdcDacoM.Protocol, JdcDacoM.Common, IdContext, IdBaseComponent,
  IdComponent, IdCustomTCPServer, IdTCPServer, JdcGlobal, System.Generics.Collections, IdGlobal,
  Winapi.Windows, System.JSON, REST.JSON, JdcGlobal.ClassHelper, System.DateUtils, IdException,
  IdExceptionCore, IdStack, Math, JdcGlobal.DSCommon;

type
  TMySession = class;

  TOnPowerData = procedure(ATime: TDateTime; AMac: string; AData: TPowerData) of object; // Power 데이터 수신 이벤트
  TOnIOList = procedure(ATime: TDateTime; AMac: string; AData: TIOList) of object; // IOControl 데이터 수신 이벤트
  TOnModuleData = procedure(ASession: TMySession; AID: Integer) of object; // 분기모듈 데이터 수신 이벤트
  TOnMeterData = procedure(ASession: TMySession) of object; // 모든 모듈 데이터 수집 완료 이벤트
  TOnRequest = procedure(AContext: TIdContext; const AParam: TRequestParam) of object;

  TMySession = class
  strict private
    FMeasuringTime: TDateTime; // 작업 시간
    FRequestTime: TDateTime; // TCP 요청시간

    FModule: TModuleArray;
    FModuleCount: Integer;
    FPowerData: TPowerData;

    FIDList: TIDArray40;
    FCurIndex: Integer;
    FMacAddress: string;

    FOnPowerData: TOnPowerData;
    FOnIOList: TOnIOList;
    FOnModuleData: TOnModuleData;
    FOnMeterData: TOnMeterData;

    FOnRequest: TOnRequest;
    FContext: TIdContext;

    function GetPart1(AIndex: Integer): TDataPart1;
    procedure SetPart1(AIndex: Integer; const Value: TDataPart1);
    function GetPart2(AIndex: Integer): TDataPart2;
    procedure SetPart2(AIndex: Integer; const Value: TDataPart2);
    function GetModule(AIndex: Integer): TModuleData;

  public
    constructor Create(AContext: TIdContext);
    destructor Destroy; override;

    function ModuleList: String;
    function GetNextID: Integer;

    procedure OnIDTable(ABuff: TIdBytes);
    procedure OnModulePart1(ABuff: TIdBytes);
    procedure OnModulePart2(ABuff: TIdBytes);
    procedure OnPowerModule(ABuff: TIdBytes);
    procedure OnIOControl(ABuff: TIdBytes);
    procedure OnError(ABuff: TIdBytes);

    procedure OnResponse(var ABuff: TIdBytes);

    function BuffToRecord<T: record >(var ABuff: TIdBytes; ReverseIndex: Integer = -1;
      AType: TMessageType = msSystem): T;

    procedure RequestModule(AID: Integer; AOffSet, ALen: UInt16);
    procedure RequestNextModule;

    procedure RequestPower;

    procedure RequestIDTable;

    procedure RequestFromPowerData; // Power 데이터를 시작으로 모든데이터 요청
    procedure RequestPowerDataOnly; // Power 데이터 만 요청하기

    property RequestTime: TDateTime read FRequestTime write FRequestTime;
    property IDList: TIDArray40 read FIDList write FIDList;
    property ModuleCount: Integer read FModuleCount;
    property Part1[AIndex: Integer]: TDataPart1 read GetPart1 write SetPart1;
    property Part2[AIndex: Integer]: TDataPart2 read GetPart2 write SetPart2;
    property Module[AIndex: Integer]: TModuleData read GetModule;
    property PowerData: TPowerData read FPowerData write FPowerData;

    property CurIndex: Integer read FCurIndex write FCurIndex;
    property MacAddress: string read FMacAddress write FMacAddress;
    property MeasuringTime: TDateTime read FMeasuringTime write FMeasuringTime;

    property OnRequest: TOnRequest read FOnRequest write FOnRequest;
    property OnPowerData: TOnPowerData read FOnPowerData write FOnPowerData;
    property OnIOList: TOnIOList read FOnIOList write FOnIOList;
    property OnModuleData: TOnModuleData read FOnModuleData write FOnModuleData;
    property OnMeterData: TOnMeterData read FOnMeterData write FOnMeterData;
  end;

  TDacoMServer = class
  strict private
    FTCPServer: TIdTCPServer;

    FPowerSub: TList<TOnPowerData>;
    FModuleSub: TList<TOnModuleData>;
    FMeterSub: TList<TOnMeterData>;

    constructor Create;
    procedure TCPServerConnect(AContext: TIdContext);
    procedure TCPServerDisconnect(AContext: TIdContext);
    procedure TCPServerExecute(AContext: TIdContext);
    procedure TCPServerException(AContext: TIdContext; AException: Exception);

    procedure _RequestData(ATime: TDateTime);
    procedure OnRequest(AContext: TIdContext; const AParam: TRequestParam);
    procedure OnModuleData(ASession: TMySession; AID: Integer);
    procedure OnMeterData(ASession: TMySession);
    procedure OnPowerData(ATime: TDateTime; AMac: string; AData: TPowerData);
    procedure ParsePacket(ASession: TMySession; const ABuff: TIdBytes);
  public
    class function Obj: TDacoMServer;

    procedure Add(AProc: TOnPowerData); overload;
    procedure Remove(AProc: TOnPowerData); overload;
    procedure Add(AProc: TOnModuleData); overload;
    procedure Remove(AProc: TOnModuleData); overload;
    procedure Add(AProc: TOnMeterData); overload;
    procedure Remove(AProc: TOnMeterData); overload;

    procedure Start(APort: Integer = 8900);
    procedure Stop;

    function ClientCount: Integer;

    destructor Destroy; override;

    procedure RequestData(ATime: TDateTime);
    function Started: Boolean;
  end;

implementation

uses JdcLogging;

var
  MyObj: TDacoMServer = nil;

const
  READ_TIME_OUT = 3000;

  { TDacoMServer }

procedure TDacoMServer.Add(AProc: TOnModuleData);
begin
  FModuleSub.Add(AProc);
end;

procedure TDacoMServer.Add(AProc: TOnMeterData);
begin
  FMeterSub.Add(AProc);
end;

procedure TDacoMServer.Add(AProc: TOnPowerData);
begin
  FPowerSub.Add(AProc);
end;

function TDacoMServer.ClientCount: Integer;
begin
  result := FTCPServer.Contexts.Count;
end;

constructor TDacoMServer.Create;
begin
  FTCPServer := TIdTCPServer.Create(nil);
  FTCPServer.OnExecute := TCPServerExecute;
  FTCPServer.OnConnect := TCPServerConnect;
  FTCPServer.OnDisconnect := TCPServerDisconnect;
  FTCPServer.OnException := TCPServerException;

  FPowerSub := TList<TOnPowerData>.Create;
  FModuleSub := TList<TOnModuleData>.Create;
  FMeterSub := TList<TOnMeterData>.Create;
end;

destructor TDacoMServer.Destroy;
begin
  FreeAndNil(FPowerSub);
  FreeAndNil(FModuleSub);
  FreeAndNil(FMeterSub);
  FreeAndNil(FTCPServer);
  inherited;
end;

class function TDacoMServer.Obj: TDacoMServer;
begin
  if MyObj = nil then
    MyObj := TDacoMServer.Create;
  result := MyObj;
end;

procedure TDacoMServer.OnMeterData(ASession: TMySession);
var
  MyEvent: TOnMeterData;
begin
  for MyEvent in FMeterSub do
    MyEvent(ASession);
end;

procedure TDacoMServer.OnModuleData(ASession: TMySession; AID: Integer);
var
  MyEvent: TOnModuleData;
begin
  for MyEvent in FModuleSub do
    MyEvent(ASession, AID);
end;

procedure TDacoMServer.OnPowerData(ATime: TDateTime; AMac: string; AData: TPowerData);
var
  MyEvent: TOnPowerData;
begin
  for MyEvent in FPowerSub do
    MyEvent(ATime, AMac, AData);
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
    TLogging.Obj.ApplicationMessage(msSystem, 'SEND', 'Port=%d,Msg=%s',
      [AContext.PeerPort, IdBytesToHex(buff)]);
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msError, 'SEND', 'IP=%s,buff=%s,E=%s',
        [AContext.PeerIP, IdBytesToHex(buff), E.Message]);
  end;
end;

procedure TDacoMServer.ParsePacket(ASession: TMySession; const ABuff: TIdBytes);
begin
  case TModbus.GetProtocolType(ABuff) of
    ptIDTable:
      ASession.OnIDTable(ABuff);
    ptModulePart1:
      ASession.OnModulePart1(ABuff);
    ptModulePart2:
      ASession.OnModulePart2(ABuff);
    ptPowerModule:
      ASession.OnPowerModule(ABuff);
    ptIOControl:
      ASession.OnIOControl(ABuff);
    ptError:
      ASession.OnError(ABuff);
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'WrongProtocol', IdBytesToHex(ABuff));
  end;
end;

procedure TDacoMServer.Remove(AProc: TOnModuleData);
begin
  FModuleSub.Remove(AProc);
end;

procedure TDacoMServer.Remove(AProc: TOnMeterData);
begin
  FMeterSub.Remove(AProc);
end;

procedure TDacoMServer.Remove(AProc: TOnPowerData);
begin
  FPowerSub.Remove(AProc);
end;

procedure TDacoMServer.RequestData(ATime: TDateTime);
begin
  try
    _RequestData(ATime);
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msError, 'RequestData', E.Message);
  end;
end;

procedure TDacoMServer.Start(APort: Integer);
begin
  if FTCPServer.Active then
    Exit;

  FTCPServer.Bindings.Clear;
  FTCPServer.DefaultPort := APort;
  FTCPServer.Active := True;
  TLogging.Obj.ApplicationMessage(msInfo, 'DacoMServer', Format('Opened,Port=%d', [FTCPServer.DefaultPort]));
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
  TLogging.Obj.ApplicationMessage(msInfo, 'DacoMServer', 'Closed');
end;

procedure TDacoMServer.TCPServerConnect(AContext: TIdContext);
var
  MySession: TMySession;
begin
  TLogging.Obj.ApplicationMessage(msInfo, 'Connected', AContext.PeerInfo);

  MySession := TMySession.Create(AContext);
  MySession.OnRequest := OnRequest;
  MySession.OnPowerData := OnPowerData;
  MySession.OnModuleData := OnModuleData;
  MySession.OnMeterData := OnMeterData;

  AContext.Data := MySession;
  AContext.ReadTimeout := READ_TIME_OUT;

  // for MacAddress
  MySession.RequestPowerDataOnly;
end;

procedure TDacoMServer.TCPServerDisconnect(AContext: TIdContext);
begin
  if Assigned(AContext.Data) and (AContext.Data is TMySession) then
    AContext.Data.Free;

  TLogging.Obj.ApplicationMessage(msInfo, 'Disconnected', AContext.PeerInfo);
  AContext.Data := nil;
end;

procedure TDacoMServer.TCPServerException(AContext: TIdContext; AException: Exception);
begin
  if AException is EIdReadTimeout then
  begin
    TLogging.Obj.ApplicationMessage(msDebug, 'ReadTimeout', 'NoResponse.');
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

  TLogging.Obj.ApplicationMessage(msError, 'TCPException', 'E=%S,EClass=%s',
    [AException.Message, AException.ClassName]);
end;

procedure TDacoMServer.TCPServerExecute(AContext: TIdContext);
var
  buff: TIdBytes;
  Header: TMBAPHeader;
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

        TLogging.Obj.ApplicationMessage(msInfo, 'ExtraWait', 'Port=%d,%dms', [AContext.PeerPort, tmp]);
        AContext.ReadTimeout := Min(tmp, READ_TIME_OUT - 1);
        Exit;
      end;

      raise;
    end;

    on E: Exception do
      raise;
  end;

  TLogging.Obj.ApplicationMessage(msSystem, 'RECV', 'Port=%d,Len=%d,Msg=%s',
    [AContext.PeerPort, Length(buff), IdBytesToHex(buff)]);

  CopyMemory(@Header, @buff[0], SizeOf(Header));
  RevEveryWord(@Header, SizeOf(TMBAPHeader));

  TLogging.Obj.ApplicationMessage(msSystem, 'HEADER', TJson.RecordToJsonString(Header));

  DataLen := Header.Length;
  if TModbus.InvalidDataLen(DataLen) then
  begin
    AContext.FlushBuffer;
    Exit;
  end;

  SetLength(buff, 0);
  AContext.ReadTimeout := READ_TIME_OUT;
  AContext.ReadBytes(buff, DataLen);
  MySession.RequestTime := 0;

  TLogging.Obj.ApplicationMessage(msSystem, 'RECV', 'Port=%d,Len=%d,Msg=%s',
    [AContext.PeerPort, Length(buff), IdBytesToHex(buff)]);
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
      MySession.MeasuringTime := ATime;
      MySession.RequestFromPowerData;
    end;
  finally
    FTCPServer.Contexts.UnlockList;
  end;
end;

{ TMySession }

function TMySession.BuffToRecord<T>(var ABuff: TIdBytes; ReverseIndex: Integer; AType: TMessageType): T;
begin
  CopyMemory(@result, @ABuff[0], SizeOf(T));

  if ReverseIndex > -1 then
    RevEveryWord(@result, SizeOf(T), ReverseIndex);

  SetLength(ABuff, 0);
  TLogging.Obj.ApplicationMessage(AType, TDSCommon.GetRecordName<T>.Substring(1),
    TJson.RecordToJsonString(result));
end;

constructor TMySession.Create(AContext: TIdContext);
var
  I: Integer;
begin
  FContext := AContext;
  FModuleCount := 0;
  FCurIndex := 0;
  FRequestTime := 0;
  FMeasuringTime := Now;
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
begin
  BuffToRecord<TErrorCode>(ABuff, -1, msWarning);
end;

function TMySession.GetModule(AIndex: Integer): TModuleData;
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetModule, ' + AIndex.ToString);

  result := FModule[AIndex];
end;

function TMySession.GetPart1(AIndex: Integer): TDataPart1;
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetPart1, ' + AIndex.ToString);

  result := FModule[AIndex].Part1;
end;

function TMySession.GetPart2(AIndex: Integer): TDataPart2;
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetPart2, ' + AIndex.ToString);

  result := FModule[AIndex].Part2;
end;

procedure TMySession.OnIDTable(ABuff: TIdBytes);
var
  IDTable: TIDTable;
begin
  CopyMemory(@IDTable, @ABuff[0], SizeOf(TIDTable));
  TLogging.Obj.ApplicationMessage(msSystem, 'IDTable', TJson.RecordToJsonString(IDTable));

  CopyMemory(@Self.IDList[0], @IDTable.ID[0], SizeOf(TIDArray40));
  FModuleCount := Length(Self.ModuleList.Split([',']));
  TLogging.Obj.ApplicationMessage(msInfo, 'ModuleList', Format('Port=%d,Count=%d,List=%s',
    [FContext.PeerPort, FModuleCount, ModuleList]));
end;

procedure TMySession.OnIOControl(ABuff: TIdBytes);
var
  IOControl: TIOControl;
begin
  IOControl := BuffToRecord<TIOControl>(ABuff, 3, msSystem);
  if Assigned(FOnIOList) then
    FOnIOList(FRequestTime, MacAddress, IOControl.Data);
end;

procedure TMySession.OnModulePart1(ABuff: TIdBytes);
var
  ModulePart1: TModulePart1;
begin
  ModulePart1 := BuffToRecord<TModulePart1>(ABuff, 3);
  Self.Part1[ModulePart1.UnitId] := ModulePart1.Data;
  RequestModule(ModulePart1.UnitId, TModbus.WORD_COUNT_PART1, TModbus.WORD_COUNT_PART2);
end;

function TMySession.ModuleList: String;
var
  I: Integer;
  str: TStringList;
begin
  str := TStringList.Create;
  for I := Low(FIDList) to High(FIDList) do
  begin
    if FIDList[I].Hi > 0 then
      str.Add(Format('%d(%x)', [FIDList[I].Hi, FIDList[I].Hi]));
  end;

  result := str.CommaText;
  str.Free;
end;

procedure TMySession.OnModulePart2(ABuff: TIdBytes);
var
  ModulePart2: TModulePart2;
begin
  ModulePart2 := BuffToRecord<TModulePart2>(ABuff, 3);
  Self.Part2[ModulePart2.UnitId] := ModulePart2.Data;

  if Assigned(FOnModuleData) then
    FOnModuleData(Self, ModulePart2.UnitId);

  // Next 데이터 요청
  RequestNextModule;
end;

procedure TMySession.OnPowerModule(ABuff: TIdBytes);
var
  PowerModule: TPowerModule;
begin
  PowerModule := BuffToRecord<TPowerModule>(ABuff, 3);
  if FMacAddress = '' then
  begin
    FMacAddress := ToHex(ToBytes(PowerModule.Data.EthernetMac1_1)) + '-' +
      ToHex(ToBytes(PowerModule.Data.EthernetMac1_2));
    TLogging.Obj.ApplicationMessage(msInfo, 'AddMeter',
      Format('Port=%d,Mac=%s', [Self.FContext.PeerPort, FMacAddress]));
    RequestIDTable;
  end;

  if Assigned(FOnPowerData) then
    FOnPowerData(FRequestTime, MacAddress, PowerModule.Data);

  Self.PowerData := PowerModule.Data;
  RequestNextModule;
end;

procedure TMySession.OnResponse(var ABuff: TIdBytes);
begin
  BuffToRecord<TResponse>(ABuff, 2, msInfo);
end;

procedure TMySession.RequestIDTable;
begin
  OnRequest(FContext, TRequestParam.Create(TModbus.IDTABLE_UNITID, TModbus.READ_REGISTER,
    TModbus.IDTABLE_ADDRESS, TModbus.WORD_COUNT_IDTABLE));
end;

procedure TMySession.RequestModule(AID: Integer; AOffSet, ALen: UInt16);
var
  Addr: UInt16;
  MySession: TMySession;
begin
  if AID <= 0 then
  begin
    MySession := FContext.Data as TMySession;
    if MinuteOf(MySession.MeasuringTime) = 0 then
      RequestIDTable; // 1시간에 한번씩 IDTable Update

    // 다음 모듈 없음 - 모든 모듈 데이터 수집 완료
    if Assigned(FOnMeterData) and (FModuleCount > 0) then
      FOnMeterData(Self);
    Exit;
  end;

  Addr := TModbus.GetAddress(AID, TDacoM.DEVICE_1000APS) + AOffSet;
  OnRequest(FContext, TRequestParam.Create(AID, TModbus.READ_REGISTER, Addr, ALen));
end;

procedure TMySession.RequestNextModule;
begin
  RequestModule(GetNextID, 0, TModbus.WORD_COUNT_PART1);
end;

procedure TMySession.RequestPower;
begin
  OnRequest(FContext, TRequestParam.Create(TModbus.POWER_UNIT_ID, TModbus.READ_REGISTER,
    TModbus.POWER_ADDRESS, TModbus.WORD_COUNT_POWER));
end;

procedure TMySession.RequestPowerDataOnly;
begin
  FCurIndex := Length(FIDList);
  RequestPower;
end;

procedure TMySession.RequestFromPowerData;
begin

  // 미터 요청 파라미터를 초기화 한다.
  FCurIndex := -1;
  FillChar(FModule, SizeOf(FModule), #0);
  FillChar(FPowerData, SizeOf(FPowerData), #0);

  // 먼저 파워 데이터를 요청한다.
  RequestPower;
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

procedure TMySession.SetPart1(AIndex: Integer; const Value: TDataPart1);
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index SetPart1, ' + AIndex.ToString);

  FModule[AIndex].Part1 := Value;
end;

procedure TMySession.SetPart2(AIndex: Integer; const Value: TDataPart2);
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index SetPart2, ' + AIndex.ToString);

  FModule[AIndex].Part2 := Value;
  FModule[AIndex].UnitId := AIndex;
end;

end.
