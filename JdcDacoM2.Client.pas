// *******************************************************
//
// DACO-M 1000P TCP Client v2
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@e-daco.net
//
// *******************************************************

unit JdcDacoM2.Client;

interface

uses System.SysUtils, System.Classes, JdcDacoM2.Protocol, JdcDacoM2.Common, IdContext, IdBaseComponent,
  IdComponent, IdTCPClient, JdcGlobal, System.Generics.Collections, IdGlobal,
  Winapi.Windows, System.JSON, REST.JSON, JdcGlobal.ClassHelper, System.DateUtils, IdException,
  IdExceptionCore, IdStack, Math, JdcGlobal.DSCommon, System.Threading;

type
  TMySession = class;

  TOnPowerData = procedure(ATime: TDateTime; AHost: string; const AData: TPowerData) of object;
  // Power 데이터 수신 이벤트
  TOnModuleData = procedure(ASession: TMySession; UnitID: Integer) of object; // 분기모듈 데이터 수신 이벤트
  TOnMeterData = procedure(ASession: TMySession) of object; // 모든 모듈 데이터 수집 완료 이벤트
  TOnRequest = procedure(ASession: TMySession; const AParam: TRequestParam) of object;

  TMySession = class
  strict private
    FTcpClinet: TIdTCPClient;

    FMeasuringTime: TDateTime; // 작업 시간
    FModule: TModuleArray;
    FModuleCount: Integer;
    FPowerData: TPowerData;

    FIDList: TIDArray40;
    FCurIndex: Integer;
    FConnInfo: TConnInfo;

    FOnPowerData: TOnPowerData;
    FOnModuleData: TOnModuleData;
    FOnMeterData: TOnMeterData;

    procedure RequestPower;

    function GetPart1(AIndex: Integer): TDataPart1;
    procedure SetPart1(AIndex: Integer; const Value: TDataPart1);
    function GetPart2(AIndex: Integer): TDataPart2;
    procedure SetPart2(AIndex: Integer; const Value: TDataPart2);
    function GetModule(AIndex: Integer): TModuleData;
    procedure SetModule(AIndex: Integer; const Value: TModuleData);

    procedure OnConnected(ASender: TObject);
    procedure OnDisconnected(ASender: TObject);

    procedure ParsePacket(const ABuff: TIdBytes);
  private
    FName: string;
    procedure _Request(AParam: TRequestParam);
  public
    constructor Create(AName: String; AConn: TConnInfo);
    destructor Destroy; override;

    procedure Connect;
    function Connected: boolean;

    procedure Disconnect;

    function ModuleStrings: String;
    function ModuleList: TArray<Integer>;
    function GetNextID: Integer;

    procedure OnIDTable(const ABuff: TIdBytes);
    procedure OnModulePart1(const ABuff: TIdBytes);
    procedure OnModulePart2(const ABuff: TIdBytes);
    procedure OnPowerModule(const ABuff: TIdBytes);
    procedure OnSystemModule(const ABuff: TIdBytes);
    procedure OnSubUnit(const ABuff: TIdBytes);
    procedure OnIOControl(const ABuff: TIdBytes);
    procedure OnError(const ABuff: TIdBytes);

    procedure OnResponse(var ABuff: TIdBytes);

    function BuffToRecord<T: record >(const ABuff: TIdBytes; ReverseIndex: Integer = -1;
      AType: TMessageType = msSystem): T;

    procedure RequestModule(UnitID: Integer; AOffSet, ALen: UInt16);
    procedure RequestNextModule;

    procedure Request(AParam: TRequestParam);
    procedure RequestIDTable;
    procedure RequestFromPowerData; // Power 데이터를 시작으로 모든데이터 요청
    procedure RequestSystemInfo;
    procedure RequestSubInfo;

    function Host: string;

    property Name: string read FName write FName;
    property IDList: TIDArray40 read FIDList write FIDList;
    property ModuleCount: Integer read FModuleCount;
    property Part1[AIndex: Integer]: TDataPart1 read GetPart1 write SetPart1;
    property Part2[AIndex: Integer]: TDataPart2 read GetPart2 write SetPart2;
    property Module[AIndex: Integer]: TModuleData read GetModule write SetModule;
    property PowerData: TPowerData read FPowerData write FPowerData;

    property CurIndex: Integer read FCurIndex write FCurIndex;
    property MeasuringTime: TDateTime read FMeasuringTime write FMeasuringTime;

    property OnPowerData: TOnPowerData read FOnPowerData write FOnPowerData;
    property OnModuleData: TOnModuleData read FOnModuleData write FOnModuleData;
    property OnMeterData: TOnMeterData read FOnMeterData write FOnMeterData;
  end;

  TDacoObserver = class
  public
    procedure OnMeterData(ASession: TMySession); virtual;
    procedure OnPowerData(ATime: TDateTime; AHost: string; const AData: TPowerData); virtual;
    procedure OnModuleData(ASession: TMySession; UnitID: Integer); virtual; abstract;
  end;

  TDacoMClient = class
  strict private
    FTCPClient: TDictionary<String, TMySession>;

    FObserves: TList<TDacoObserver>;

    constructor Create;
    procedure _RequestData(ATime: TDateTime; AHost: string = '');

    procedure OnMeterData(ASession: TMySession);
    procedure OnModuleData(ASession: TMySession; AID: Integer);
    procedure OnPowerData(ATime: TDateTime; AHost: string; const AData: TPowerData);
  public
    class function Obj: TDacoMClient;

    procedure Add(AObserver: TDacoObserver);
    procedure Remove(AObserver: TDacoObserver);

    procedure AddMeter(AName: string; AConn: TConnInfo);
    procedure Start;
    procedure Stop;

    function ClientCount: Integer;

    destructor Destroy; override;

    procedure RequestData(ATime: TDateTime; AHost: string = '');
    function GetIDList(AHost: string): TArray<Integer>;
    function GetOnlineHost: TArray<String>;
    function Started: boolean;
  end;

implementation

uses JdcLogging;

var
  MyObj: TDacoMClient = nil;

  { TDacoMServer }

procedure TDacoMClient.Add(AObserver: TDacoObserver);
begin
  FObserves.Add(AObserver);
end;

procedure TDacoMClient.AddMeter(AName: string; AConn: TConnInfo);
var
  MySession: TMySession;
begin
  if FTCPClient.ContainsKey(AConn.ToString) then
    Exit;

  MySession := TMySession.Create(AName, AConn);
  MySession.OnPowerData := OnPowerData;
  MySession.OnModuleData := OnModuleData;
  MySession.OnMeterData := OnMeterData;
  FTCPClient.Add(AConn.ToString, MySession);
end;

function TDacoMClient.ClientCount: Integer;
var
  MyClient: TMySession;
begin
  result := 0;
  for MyClient in FTCPClient.Values do
  begin
    if MyClient.Connected then
      result := result + 1;
  end;
end;

constructor TDacoMClient.Create;
begin
  FTCPClient := TDictionary<String, TMySession>.Create;
  FObserves := TList<TDacoObserver>.Create;
end;

destructor TDacoMClient.Destroy;
begin
  FreeAndNil(FObserves);
  FreeAndNil(FTCPClient);
  inherited;
end;

function TDacoMClient.GetIDList(AHost: string): TArray<Integer>;
begin
  SetLength(result, 0);
  if FTCPClient.ContainsKey(AHost) then
    Exit;

  result := FTCPClient.Items[AHost].ModuleList;
end;

function TDacoMClient.GetOnlineHost: TArray<String>;
var
  MySession: TMySession;
begin
  result := [];
  for MySession in FTCPClient.Values do
  begin
    if MySession.Connected then
      result := result + [MySession.Host]
  end;
end;

class function TDacoMClient.Obj: TDacoMClient;
begin
  if MyObj = nil then
    MyObj := TDacoMClient.Create;
  result := MyObj;
end;

procedure TDacoMClient.OnMeterData(ASession: TMySession);
var
  MyObserver: TDacoObserver;
begin
  for MyObserver in FObserves do
    MyObserver.OnMeterData(ASession);
end;

procedure TDacoMClient.OnModuleData(ASession: TMySession; AID: Integer);
var
  MyObserver: TDacoObserver;
begin
  for MyObserver in FObserves do
    MyObserver.OnModuleData(ASession, AID);
end;

procedure TDacoMClient.OnPowerData(ATime: TDateTime; AHost: string; const AData: TPowerData);
var
  MyObserver: TDacoObserver;
begin
  for MyObserver in FObserves do
    MyObserver.OnPowerData(ATime, AHost, AData);
end;

procedure TDacoMClient.Remove(AObserver: TDacoObserver);
begin
  FObserves.Remove(AObserver);
end;

procedure TDacoMClient.RequestData(ATime: TDateTime; AHost: string);
begin
  try
    _RequestData(ATime, AHost);
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msWarning, 'RequestData', DACO_TAG + 'Mac=%s,E=%s', [AHost, E.Message]);
  end;
end;

procedure TDacoMClient.Start;
var
  MySession: TMySession;
begin
  for MySession in FTCPClient.Values do
    MySession.Connect;

  TLogging.Obj.ApplicationMessage(msInfo, 'Start', DACO_TAG + 'Count=%d', [FTCPClient.Count]);
end;

function TDacoMClient.Started: boolean;
begin
  result := ClientCount > 0;
end;

procedure TDacoMClient.Stop;
var
  MyClient: TMySession;
begin
  for MyClient in FTCPClient.Values do
  begin
    MyClient.Free;
  end;
  FTCPClient.Clear;

  TLogging.Obj.ApplicationMessage(msInfo, 'Closed', DACO_TAG);
end;

procedure TDacoMClient._RequestData(ATime: TDateTime; AHost: string);
var
  MySession: TMySession;
begin
  for MySession in FTCPClient.Values do
  begin
    if (AHost <> '') and (MySession.Host <> AHost) then
      Continue;

    MySession.MeasuringTime := ATime;
    MySession.RequestFromPowerData;
  end;
end;

{ TMySession }

function TMySession.BuffToRecord<T>(const ABuff: TIdBytes; ReverseIndex: Integer; AType: TMessageType): T;
begin
  CopyMemory(@result, @ABuff[0], SizeOf(T));

  if ReverseIndex > -1 then
    RevEveryWord(@result, SizeOf(T), ReverseIndex);

  TLogging.Obj.ApplicationMessage(AType, TDSCommon.GetRecordName<T>.Substring(1),
    DACO_TAG + TJson.RecordToJsonString(result));
end;

constructor TMySession.Create(AName: String; AConn: TConnInfo);
var
  I: Integer;
begin
  FName := AName;
  FConnInfo := AConn;
  FModuleCount := 0;
  FCurIndex := 0;
  FMeasuringTime := Now;

  for I := Low(FIDList) to High(FIDList) do
  begin
    FIDList[I].Hi := 0;
    FIDList[I].Low := 0;
  end;
end;

destructor TMySession.Destroy;
begin
  if Self.Connected then
  begin
    FTcpClinet.Disconnect;
    FreeAndNil(FTcpClinet);
  end;

  inherited;
end;

procedure TMySession.Disconnect;
begin
  FTcpClinet.Disconnect;
end;

procedure TMySession.OnConnected(ASender: TObject);
begin
  TLogging.Obj.ApplicationMessage(msInfo, 'Connected', DACO_TAG + Self.Host);
  RequestIDTable;
end;

procedure TMySession.OnDisconnected(ASender: TObject);
begin
  TLogging.Obj.ApplicationMessage(msInfo, 'Disconnected', DACO_TAG + Self.Host);
end;

procedure TMySession.OnError(const ABuff: TIdBytes);
begin
  BuffToRecord<TErrorCode>(ABuff, -1, msWarning);
end;

function TMySession.GetModule(AIndex: Integer): TModuleData;
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetModule, Index=' + AIndex.ToString);

  result := FModule[AIndex];
end;

function TMySession.GetPart1(AIndex: Integer): TDataPart1;
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetPart1, Index=' + AIndex.ToString);

  result := FModule[AIndex].Part1;
end;

function TMySession.GetPart2(AIndex: Integer): TDataPart2;
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetPart2, Index=' + AIndex.ToString);

  result := FModule[AIndex].Part2;
end;

function TMySession.Host: string;
begin
  result := FConnInfo.ToString;
end;

procedure TMySession.OnIDTable(const ABuff: TIdBytes);
var
  IDTable: TIDTable;
begin
  CopyMemory(@IDTable, @ABuff[0], SizeOf(TIDTable));
  TLogging.Obj.ApplicationMessage(msSystem, 'IDTable', DACO_TAG + TJson.RecordToJsonString(IDTable));

  CopyMemory(@Self.IDList[0], @IDTable.ID[0], SizeOf(TIDArray40));
  FModuleCount := Length(Self.ModuleList);
  TLogging.Obj.ApplicationMessage(msInfo, 'ModuleList', Format(DACO_TAG + 'Host=%s,Count=%d,List=%s',
    [Self.Host, FModuleCount, Self.ModuleStrings]));
end;

procedure TMySession.OnIOControl(const ABuff: TIdBytes);
begin
  //
end;

procedure TMySession.OnModulePart1(const ABuff: TIdBytes);
var
  UnitID: Byte;
  ModulePart1: TModulePart1;
begin
  UnitID := FIDList[FCurIndex].Hi;
  ModulePart1 := BuffToRecord<TModulePart1>(ABuff, 3);
  Self.Part1[UnitID] := ModulePart1.Data;
  RequestModule(UnitID, TModbus.WORD_COUNT_PART1, TModbus.WORD_COUNT_PART2);
end;

function TMySession.ModuleList: TArray<Integer>;
var
  I: Integer;
begin
  result := [];
  for I := Low(FIDList) to High(FIDList) do
  begin
    if FIDList[I].Hi > 0 then
      result := result + [FIDList[I].Hi];
  end;
end;

function TMySession.ModuleStrings: String;
var
  I: Integer;
  str: TStringList;
  Module: TArray<Integer>;
begin
  Module := Self.ModuleList;

  str := TStringList.Create;
  try
    for I := Low(Module) to High(Module) do
    begin
      str.Add(Format('%d(%x)', [Module[I], Module[I]]));
    end;
    result := str.CommaText;
  finally
    str.Free;
  end;
end;

procedure TMySession.OnModulePart2(const ABuff: TIdBytes);
var
  UnitID: Byte;
  ModulePart2: TModulePart2;
begin
  UnitID := FIDList[FCurIndex].Hi;
  ModulePart2 := BuffToRecord<TModulePart2>(ABuff, 3);
  Self.Part2[UnitID] := ModulePart2.Data;

  if Assigned(FOnModuleData) then
    FOnModuleData(Self, UnitID);

  // Next 데이터 요청
  RequestNextModule;
end;

procedure TMySession.OnPowerModule(const ABuff: TIdBytes);
var
  PowerModule: TPowerModule;
begin
  PowerModule := BuffToRecord<TPowerModule>(ABuff, 3);
  if Assigned(FOnPowerData) then
    FOnPowerData(FMeasuringTime, Self.Host, PowerModule.Data);

  Self.PowerData := PowerModule.Data;
  RequestNextModule;
end;

procedure TMySession.OnResponse(var ABuff: TIdBytes);
begin

end;

procedure TMySession.OnSubUnit(const ABuff: TIdBytes);
var
  SubUnit: TSubModule;
begin
  SubUnit := BuffToRecord<TSubModule>(ABuff, 3);
end;

procedure TMySession.OnSystemModule(const ABuff: TIdBytes);
var
  SystemModule: TSystemModule;
begin
  SystemModule := BuffToRecord<TSystemModule>(ABuff, 3);
  TLogging.Obj.ApplicationMessage(msInfo, 'SystemModule', DACO_TAG + TJson.RecordToJsonString(SystemModule));
end;

procedure TMySession.ParsePacket(const ABuff: TIdBytes);
begin
  case TModbus.GetProtocolType(ABuff) of
    ptSubUnit:
      OnSubUnit(ABuff);
    ptSystemModule:
      OnSystemModule(ABuff);
    ptIDTable:
      OnIDTable(ABuff);
    ptModulePart1:
      OnModulePart1(ABuff);
    ptModulePart2:
      OnModulePart2(ABuff);
    ptPowerModule:
      OnPowerModule(ABuff);
    ptError:
      OnError(ABuff);
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'WrongProtocol', DACO_TAG + IdBytesToHex(ABuff));
  end;
end;

procedure TMySession.RequestIDTable;
begin
  Request(TRequestParam.Create(TModbus.TCP_METER_ID, TModbus.IDTABLE_ADDRESS, TModbus.WORD_COUNT_IDTABLE));
end;

procedure TMySession.RequestModule(UnitID: Integer; AOffSet, ALen: UInt16);
var
  Addr: UInt16;
begin
  if UnitID <= 0 then
  begin
    // 다음 모듈 없음 - 모든 모듈 데이터 수집 완료
    if Assigned(FOnMeterData) and (FModuleCount > 0) then
      FOnMeterData(Self);
    Exit;
  end;

  Addr := TModbus.GetAddress(UnitID, TDacoM.DEVICE_1000APS) + AOffSet;
  Request(TRequestParam.Create(TModbus.TCP_METER_ID, Addr, ALen));
end;

procedure TMySession.RequestNextModule;
begin
  RequestModule(GetNextID, 0, TModbus.WORD_COUNT_PART1);
end;

procedure TMySession.RequestPower;
begin
  Request(TRequestParam.Create(TModbus.TCP_METER_ID, TModbus.POWER_ADDRESS, TModbus.WORD_COUNT_POWER));
end;

procedure TMySession.RequestSubInfo;
begin
  Request(TRequestParam.Create(TModbus.TCP_METER_ID, TModbus.SYSTEM_ADDRESS, TModbus.WORD_COUNT_SUB));
end;

procedure TMySession.RequestSystemInfo;
begin
  Request(TRequestParam.Create(TModbus.TCP_METER_ID, TModbus.SYSTEM_ADDRESS, TModbus.WORD_COUNT_SYSTEM));
end;

procedure TMySession.Request(AParam: TRequestParam);
begin
  if not Self.Connected then
  begin
    Self.Connect;
    Exit;
  end;

  TTask.Run(
    procedure
    begin
      try
        _Request(AParam);
      except
        on E: Exception do
        begin
          TLogging.Obj.ApplicationMessage(msWarning, 'Request', DACO_TAG + 'Host=%s,E=%s',
            [Self.Host, E.Message]);
          FreeAndNilEx(FTcpClinet);
        end;
      end;
    end);
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
    // TLogging.Obj.ApplicationMessage(msDebug, 'GetNextID1', DACO_TAG+'CurIndex=%d,result=%d',
    // [FCurIndex, FIDList[I].Hi]);

    FCurIndex := I;
    Exit(FIDList[I].Hi);
  end;
  result := -1;
  // TLogging.Obj.ApplicationMessage(msDebug, 'GetNextID2', DACO_TAG + result.ToString);
end;

procedure TMySession.SetModule(AIndex: Integer; const Value: TModuleData);
begin
  if (AIndex < Low(FModule)) or (AIndex > High(FModule)) then
    raise Exception.Create('out of index GetModule, Index=' + AIndex.ToString);

  FModule[AIndex] := Value;
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
  FModule[AIndex].UnitID := AIndex;
end;

procedure TMySession._Request(AParam: TRequestParam);
var
  buff: TIdBytes;
  Header: TMBAPHeader;
  DataLen: Integer;
begin
  buff := TModbus.TcpCommand(AParam);

  FTcpClinet.IOHandler.Write(buff);
  TLogging.Obj.ApplicationMessage(msSystem, 'SEND', DACO_TAG + 'Host=%s,Msg=%s',
    [Self.Host, IdBytesToHex(buff)]);

  SetLength(buff, 0);
  FTcpClinet.IOHandler.ReadBytes(buff, SizeOf(Header));

  TLogging.Obj.ApplicationMessage(msSystem, 'RECV', DACO_TAG + 'Host=%s,Len=%d,Msg=%s',
    [Self.Host, Length(buff), IdBytesToHex(buff)]);

  CopyMemory(@Header, @buff[0], SizeOf(Header));
  RevEveryWord(@Header, SizeOf(TMBAPHeader));

  TLogging.Obj.ApplicationMessage(msSystem, 'HEADER', DACO_TAG + TJson.RecordToJsonString(Header));

  DataLen := Header.Length;
  if TModbus.InvalidDataLen(DataLen) then
  begin
    FTcpClinet.IOHandler.FlushBuffer;
    Exit;
  end;

  SetLength(buff, 0);
  FTcpClinet.IOHandler.ReadBytes(buff, DataLen);
  TLogging.Obj.ApplicationMessage(msSystem, 'RECV', DACO_TAG + 'Host=%s,Len=%d,Msg=%s',
    [Self.Host, Length(buff), IdBytesToHex(buff)]);
  ParsePacket(buff);
end;

procedure TMySession.Connect;
begin
  if Self.Connected then
    Exit;

  TTask.Run(
    procedure
    begin
      try
        if Assigned(FTcpClinet) then
          FreeAndNil(FTcpClinet);

        FTcpClinet := TIdTCPClient.Create(nil);
        FTcpClinet.Host := FConnInfo.StringValue;
        FTcpClinet.Port := FConnInfo.IntegerValue;
        FTcpClinet.ReadTimeout := TDacoM.READ_TIME_OUT;
        FTcpClinet.OnConnected := OnConnected;
        FTcpClinet.OnDisconnected := OnDisconnected;
        FTcpClinet.Connect;
      except
        on E: Exception do
          TLogging.Obj.ApplicationMessage(msWarning, 'Connect', 'Host=%s,E=%s', [Self.Host, E.Message]);
      end;
    end);
end;

function TMySession.Connected: boolean;
begin
  result := Assigned(FTcpClinet) and FTcpClinet.Connected;
end;

{ TDacoObserver }

procedure TDacoObserver.OnMeterData(ASession: TMySession);
begin
  //
end;

procedure TDacoObserver.OnPowerData(ATime: TDateTime; AHost: string; const AData: TPowerData);
begin
  //
end;

end.
