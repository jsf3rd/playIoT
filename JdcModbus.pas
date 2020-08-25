unit JdcModbus;

interface

uses System.Classes, System.SysUtils, IdGlobal, JdcCRC, JdcGlobal, Winapi.Windows;

type
  THeader = record
    ID: Byte;
    FC: Byte;
    Addr: UInt16;
    constructor Create(ID: Byte; FC: Byte; Addr: UInt16);
    function GetCommand: TIdBytes;
  end;

  TSingleRegister = record
    ID: Byte;
    FC: Byte;
    Addr: UInt16;
    Data: UInt16;
    constructor Create(ID: Byte; FC: Byte; Addr: UInt16; Data: Int16);
    function GetCommand: TIdBytes;
  end;

  TMultipleRegister = record
    ID: Byte;
    FC: Byte;
    Addr: UInt16;
    Data: TIdBytes;
    WordCount: UInt16;
    ByteCount: UInt8;
    constructor Create(ID: Byte; FC: Byte; Addr: UInt16; Data: TArray<UInt16>);
    function GetCommand: TIdBytes;
  end;

  TModbus = class
  const
    READ_COIL_STATUS = $01;
    READ_HOLDING_REGISTER = $03;
    READ_INPUT_REGISTER = $4;
    WRITE_SINGLE_COIL = $05;
    WRITE_SINGLE_REGISTER = $06;
    WRITE_MULTIPLE_COILS = $0F;
    WRITE_MULIPLE_REGISTER = $10;

    ERROR_CODE = $80;

    MBAP: array [0 .. 5] of Byte = ($00, $00, $00, $00, $00, $06);
  public
    class function RtuCommand(const AParam: TSingleRegister): TIdBytes;
    class function TcpCommand(const AParam: TSingleRegister): TIdBytes;
  end;

implementation

{ TRequestParam }

constructor TSingleRegister.Create(ID, FC: Byte; Addr: UInt16; Data: Int16);
begin
  Self.ID := ID;
  Self.FC := FC;
  Self.Addr := Addr;
  Self.Data := Data;
end;

function TSingleRegister.GetCommand: TIdBytes;
begin
  SetLength(result, 0);
  AppendByte(result, Self.ID); // unit_id
  AppendByte(result, Self.FC); // FC
  AppendBytes(result, ToBytes(Rev2Bytes(Self.Addr))); // 矫累林家
  AppendBytes(result, ToBytes(Rev2Bytes(Self.Data))); // Read Count / Write Data
end;

{ TModbus }

class function TModbus.RtuCommand(const AParam: TSingleRegister): TIdBytes;
var
  MyCRC: UInt16;
begin
  result := AParam.GetCommand;
  MyCRC := ModbusCRC16(result);
  AppendBytes(result, ToBytes(MyCRC));
end;

{ TMultipleData }

constructor TMultipleRegister.Create(ID, FC: Byte; Addr: UInt16; Data: TArray<UInt16>);
begin
  if Length(Data) > $FF then
    raise Exception.Create('Max data length 255 but ' + Length(Data).ToString);

  Self.ID := ID;
  Self.FC := FC;
  Self.Addr := Addr;
  SetLength(Self.Data, Length(Data) * SizeOf(UInt16));
  CopyMemory(Self.Data, @Data[0], Length(Self.Data));
  Self.WordCount := Length(Data);
  Self.ByteCount := SizeOf(Data);
end;

function TMultipleRegister.GetCommand: TIdBytes;
begin
  SetLength(result, 0);
  AppendByte(result, Self.ID); // unit_id
  AppendByte(result, Self.FC); // FC
  AppendBytes(result, ToBytes(Rev2Bytes(Self.Addr))); // 矫累林家
  AppendBytes(result, ToBytes(Rev2Bytes(Self.WordCount))); // Word Count
  AppendByte(result, Self.ByteCount); // Word Count
  AppendBytes(result, Self.Data); //
end;

class function TModbus.TcpCommand(const AParam: TSingleRegister): TIdBytes;
begin
  SetLength(result, Length(MBAP));
  CopyMemory(@result[0], @MBAP[0], Length(MBAP));
  AppendBytes(result, AParam.GetCommand);
end;

{ THeader }

constructor THeader.Create(ID, FC: Byte; Addr: UInt16);
begin
  Self.ID := ID;
  Self.FC := FC;
  Self.Addr := Addr;

end;

function THeader.GetCommand: TIdBytes;
begin
  SetLength(result, 0);
  AppendByte(result, Self.ID); // unit_id
  AppendByte(result, Self.FC); // FC
  AppendBytes(result, ToBytes(Rev2Bytes(Self.Addr))); // 矫累林家
end;

end.
