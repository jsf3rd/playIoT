// *******************************************************
//
// DACO-M 1000P Modbus TCP Protocol
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@nate.com
//
// *******************************************************

unit JdcDacoM.Protocol;

interface

uses SysUtils, Classes, JdcGlobal, JdcGlobal.ClassHelper, IdGlobal, Winapi.Windows, Math,
  System.DateUtils, JdcCRC, REST.JSON, System.JSON, JdcDacoM.Common, JdcLogging;

type
  TPowerData = packed record
    VoltLN: T3PhaseEx; // 전압
    VoltLL: T3PhaseEx; // 선간전압
    VoltFundamental: T3PhaseEx; // 기본파 전압
    VoltTHD: T3Phase; // 고주파 왜곡율
    VoltPhasorA: TPhasor; // A상
    VoltPhasorB: TPhasor; // B상
    VoltPhasorC: TPhasor; // C상
    VoltLNUnbalance: Single; // LN 불평형율
    VoltLLUnbalance: Single; // LL 불평형율
    VoltU0Unbalance: Single; // U0 불평형율
    VoltU2Unbalance: Single; // U2 불평형율
    Reserved1: Single;
    Reserved2: Single;
    FrequencyLN: T3Phase; // 주파수
    Temperature: Single; // 온도
    ZeroSequenceComponentVoltage: Single;

    Hramonic: THarmoic; // 고주파 함유율
    Status_RS485: UInt16;
    Status_Modbus: UInt16;
    Status_Voltage_RawData: UInt16;
    Status_Current1_RawData: UInt16;
    Status_Current2_RawData: UInt16;
    Status_Network_Connection: UInt16;
    ProType: UInt16;
    Reserved3: UInt16;
    Reserved4: UInt16;
    Reserved5: UInt16;
    ConnectionMethod: UInt32; // 결선방법

    PhaseDifferenceAngleA: UInt16;
    PhaseDifferenceAngleB: UInt16;
    PhaseDifferenceAngleC: UInt16;
    PowerFactorInfo: UInt16;
    __Serial: UInt32; // 사용안함
    ProductName: UInt32;
    ManufactureName: UInt32;
    HadwareVersion: UInt32;
    ApplicationVersion: UInt32;
    FirmwareVersion: UInt32;
    EthernetMac1_1: UInt32;
    EthernetMac1_2: UInt32;
    EthernetMac2_1: UInt32;
    EthernetMac2_2: UInt32;
    EthernetMac3_1: UInt32;
    EthernetMac3_2: UInt32;
    BootloaderVersion: UInt32;
    KernelVersion: UInt32;
    AppRevisionNumber: UInt32;

    {
      // Event
      AutoIDSet: UInt32;
      EventCurrentPositon: UInt16;
      EventAllPosition: UInt16;
      ModuleID: UInt16;
      EventNumber: UInt16;
      EventName: UInt16;
      EventPhase: UInt16;
      EventSettingValue: Single;
      EventStartEndValue: Single;
      EventDurationDay: UInt16;
      EventDurationSecHigh: UInt16;
      EventDurationSecLow: UInt16;
      TimeYearMonth: UInt16;
      TimeDayHour: UInt16;
      TimeMinSec: UInt16;
    }
  end;

  TPowerModule = packed record
    Header: TMBHeader;
    Data: TPowerData;
  end;

  TDataPart1 = packed record
    VoltLN: T3Phase; // 전압
    FrequencyLN: T3Phase; // 주파수
    Current: T3PhaseEx; // 전류
    ZeroCurrent: Single; //
    CurrentFundmental: T3PhaseEx; // 기본파 전류
    CurrentTHD: T3Phase; // 고주파 왜곡율
    CurrentTDD: T3Phase; // 부하 왜곡율
    CurrentPhsorA: TPhasor; // A상
    CurrentPhsorB: TPhasor; // B상
    CurrentPhsorC: TPhasor; // C상
    CurrentUnbalance: Single;
    CurrentU0Unbalance: Single;
    CurrentU2Unbalance: Single;
    CrestFactor: T3Phase;
    KFactor: T3Phase;
    KW: T3PhaseEx; // 유효전력
    KVAR: T3PhaseEx; // 무효전력
    KVA: T3PhaseEx; // 피상전력
    ZCTCurrent: Single; //
    KWh: TAMOUNT; // 유효전력량
    KVARh: TAMOUNT; // 무효전력량
    KVAh: UInt32; // 피상전력량
  end;

  TModulePart1 = packed record
    Header: TMBHeader;
    Data: TDataPart1;
    function UnitId: Byte;
  end;

  TDataPart2 = packed record
    DemandKW: T3PhaseEx; // 유효전력 디맨드
    DemandKWTotalPrediction: Single; // 유효전력 디맨드 예측값
    DemandCurrent: T3PhaseEx; // 전류 디맨드
    DemandKWCurrentPrediction: Single; // 평균전류 디맨드 예측값
    PowerFactor: T3PhaseEx; // 역률

    Hramonic: THarmoic; // 고주파 함유율
    Reserved1: UInt16;
    Reserved2: UInt16;
    Reserved3: UInt16;
    Reserved4: UInt16;
    Reserved5: UInt16;
    Reserved6: UInt16;
    Reserved7: UInt16;
    Reserved8: UInt16;
    Reserved9: UInt16;
    Reserved10: UInt16;
    __Serial: UInt32; // SN
    ConnectionDirection: UInt16; // 결선방향
    OrderWiring: UInt16; // 결선순서
    Reserved11: UInt16;
    ProductRation: UInt16;
    PFInfoA: UInt16;
    PFInfoB: UInt16;
    PFInfoC: UInt16;

    // 1000E
    // VoltageLL: T3PhaseEx;
  end;

  TModulePart2 = packed record
    Header: TMBHeader;
    Data: TDataPart2;
    function UnitId: Byte;
  end;

  TModuleData = packed record
    UnitId: Integer;
    Part1: TDataPart1;
    Part2: TDataPart2;
    function ToStrings: TStrings;
  end;

  TModuleArray = array [1 .. MAX_UNIT_ID] of TModuleData;

  TIDTable = packed record
    Header: TMBHeader;
    ID: TIDArray40;
  end;

  TAnalogChannel = array [0 .. 15] of UInt16;

  TIOList = packed record
    InputStatus1: UInt16;
    InputStatus2: UInt16;
    OutputStatus1: UInt16;
    OutputStatus2: UInt16;
    AnalogChannel: TAnalogChannel;
  end;

  TIOControl = packed record
    Header: TMBHeader;
    Data: TIOList;
  end;

  TModbus = class
  const
    ZERO_ADDRESS = $2C25; // 11301;
    MODULE_OFFSET = 300;

    MAIN_UNIT_ID = $01;

    IDTABLE_UNITID = $FD; // 253
    IDTABLE_ADDRESS = $66BD; // 26301

    POWER_UNIT_ID = $FE; // 254
    POWER_ADDRESS = $2B5D; // 11101;

    IOCONTROL_UNIT_ID = $BC; // 188
    IOCONTROL_ADDRESS = $68B1; // 26801

    ERROR_CODE = $83; // 131

    MBAP: array [0 .. 5] of Byte = ($00, $00, $00, $00, $00, $06);
    READ_REGISTER = $03;
    WRITE_REGISTER = $06;

    WORD_COUNT_POWER = SizeOf(TPowerData) div 2;
    WORD_COUNT_IOLIST = SizeOf(TIOList) div 2;
    WORD_COUNT_PART1 = SizeOf(TDataPart1) div 2;
    WORD_COUNT_PART2 = SizeOf(TDataPart2) div 2;
    WORD_COUNT_IDTABLE = SizeOf(TIDArray40) div 2;
  public
    // DACO-M 1000(A,P,S) 주소 계산
    class function GetAddress(AID: Byte; ADevice: String): UInt16;

    class function InvalidDataLen(ALen: Integer): Boolean;
    class function TcpCommand(const AParam: TRequestParam): TIdBytes;
    class function SerialCommand(const AParam: TRequestParam): TIdBytes;

    class function GetProtocolType(const ABuff: TIdBytes): TProtocolType;
    class function CheckCRC(const ABuff: TIdBytes): Boolean;
  end;

const
  DACO_TAG = '(DacoM)';

implementation

{ TModulePart1 }

function TModulePart1.UnitId: Byte;
begin
  result := Self.Header.UnitId;
end;

{ TModulePart2 }

function TModulePart2.UnitId: Byte;
begin
  result := Self.Header.UnitId;
end;

{ TModule }

function TModuleData.ToStrings: TStrings;
var
  JsonObject: TJSONObject;
begin
  result := TStringList.Create;
  JsonObject := TJson.RecordToJsonObject(Self.Part1);
  ExtractValues(result, JsonObject);
  JsonObject.Free;

  JsonObject := TJson.RecordToJsonObject(Self.Part2);
  ExtractValues(result, JsonObject);
  JsonObject.Free;
end;

{ TModbusUtil }

class function TModbus.CheckCRC(const ABuff: TIdBytes): Boolean;
var
  buff: TIdBytes;
  MyCRC, ReceivedCRC: UInt16;
begin
  SetLength(buff, Length(ABuff) - 2);
  CopyTIdBytes(ABuff, 0, buff, 0, Length(buff));
  MyCRC := ModbusCRC16(buff);
  ReceivedCRC := BytesToUInt16(ABuff, Length(buff));

  // AProc(msDebug, 'DATA', IdBytesToHex(buff));
  // AProc(msDebug, 'CRC', ToHex(ABuff, 2, Length(buff)));
  if MyCRC <> ReceivedCRC then
    TLogging.Obj.ApplicationMessage(msWarning, 'Wrong CRC', Format('MyCRC=%d,Received=%d',
      [MyCRC, ReceivedCRC]));

  result := MyCRC = ReceivedCRC;
end;

// DACO-M 1000(A,P,S) 주소 계산
class function TModbus.GetAddress(AID: Byte; ADevice: String): UInt16;
var
  Index: Integer;
begin
  // DACO-M 1000E 주소 계산
  if ADevice = TDacoM.DEVICE_1000E then
    result := TModbus.ZERO_ADDRESS + (TModbus.MODULE_OFFSET * (AID - 1)) // 순차 계산
  else if ADevice = TDacoM.DEVICE_1000APS then
  begin
    // 1,4,7...55,58
    // 128,131...182,185
    Index := (AID div 3);
    if AID >= 128 then
      Index := Index - 22;
    result := TModbus.ZERO_ADDRESS + (TModbus.MODULE_OFFSET * Index);
  end
  else
    raise Exception.Create('Unknown Device,' + ADevice);
end;

class function TModbus.GetProtocolType(const ABuff: TIdBytes): TProtocolType;
var
  ByteCount: Integer;
  Received: Integer;
begin
  result := ptUnknown;
  if Length(ABuff) < 2 then
    Exit;

  Received := Length(ABuff);
  ByteCount := ABuff[2];

  if (ABuff[0] = TModbus.IDTABLE_UNITID) and (Received >= SizeOf(TIDTable)) then
    result := ptIDTable
  else if (ABuff[0] = TModbus.POWER_UNIT_ID) and (Received >= SizeOf(TPowerModule)) then
    result := ptPowerModule
  else if (ABuff[0] = TModbus.IOCONTROL_UNIT_ID) and (Received >= SizeOf(TIOControl)) then
    result := ptIOControl
  else if (ByteCount = SizeOf(TDataPart1)) and (Received >= SizeOf(TModulePart1)) then
    result := ptModulePart1
  else if (ByteCount = SizeOf(TDataPart2)) and (Received >= SizeOf(TModulePart2)) then
    result := ptModulePart2
  else if (ByteCount = SizeOf(TIDArray40)) and (Received >= SizeOf(TIDTable)) then
    result := ptCheck // ReadConfig에서 사용
  else if (ABuff[1] = TModbus.ERROR_CODE) and (Received >= SizeOf(TErrorCode)) then
    result := ptError
  else if (ABuff[1] = TModbus.WRITE_REGISTER) and (Received >= SizeOf(TResponse)) then
    result := ptResponse;
end;

class function TModbus.InvalidDataLen(ALen: Integer): Boolean;
begin
  result := (ALen <> SizeOf(TIDTable)) and (ALen <> SizeOf(TModulePart1)) //
    and (ALen <> SizeOf(TModulePart2)) and (ALen <> SizeOf(TPowerModule)) //
    and (ALen <> SizeOf(TErrorCode)) and (ALen <> SizeOf(TIOControl));
end;

class function TModbus.TcpCommand(const AParam: TRequestParam): TIdBytes;
begin
  SetLength(result, Length(MBAP));
  CopyMemory(@result[0], @MBAP[0], Length(MBAP));
  AppendBytes(result, AParam.GetCommand);
end;

class function TModbus.SerialCommand(const AParam: TRequestParam): TIdBytes;
var
  MyCRC: UInt16;
begin
  result := AParam.GetCommand;
  MyCRC := ModbusCRC16(result);
  AppendBytes(result, ToBytes(MyCRC));
end;

end.
