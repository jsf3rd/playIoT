unit Q330;

interface

uses Winapi.Windows, System.classes, System.SysUtils, IdGlobal, Q330.CRC32,
  JdcGlobal, System.Types, Data.DBXJSON, System.DateUtils;

type
  TRegisterInfo = record
    SerialNumber: Array [0 .. 7] of Byte;
    ChallengeValue: Array [0 .. 7] of Byte;
    IP: Array [0 .. 3] of Byte;
    UDP: Array [0 .. 1] of Byte;
    Registration: array [0 .. 1] of Byte;
    RandomValue: Array [0 .. 7] of Byte;
    MD5: Array [0 .. 15] of Byte;
  end;

  TQdpHeader = record
    CRC: Array [0 .. 3] of Byte;
    Command: Byte;
    Version: Byte;
    Length: UInt16;
    SEQ: UInt16;
    ACK: UInt16;
  end;

  TDataHeader = record
    Channel: Byte;
    ClockQuality: Byte;
    MinSinceLoss: Word;
    SecondsOffset: DWord;
    USecondsOffset: DWord;
    function GetDateTime(ADataSeq: Integer): TDateTime;
  end;

  TQ330Data = record
    DataHeader: TDataHeader;
    Data: TIdBytes;
  end;

  TQ330 = class
  const
    C1_CACK = $A0; // * Command Acknowledge * /
    C1_RQSRV = $10; // * Request Server Registration * /
    C1_SRVCH = $A1; // * Server Challenge * /
    C1_SRVRSP = $11; // * Server Response * /
    C1_CERR = $A2; // * Command Error * /
    C1_DSRV = $12; // * Delete Server * /

    DT_OPEN = $0B; // Open Data Port
    DT_DATA = $00; // Data Record
    DT_DACK = $0A; // Data Acknowlege
  public
    class function BuilderPacket(AQdpHeader: TQdpHeader; AData: Pointer)
      : TIdBytes;

    class function BuildRawData(ARawData: TIdBytes): TJSONArray;

    class function GetCommandString(ACommand: Byte): String;
  end;

const
  DecompTab: array [0 .. 6, 0 .. 4] of DWord = //
    (( { samps } 4, { postshift } 8, { mask } 255, { hibit } 128, { neg } 256),
    (1, 0, 1073741823, 536870912, 1073741824), //
    (2, 15, 32767, { } 16384, { neg } 32768), //
    (3, 10, 1023, { } 512, { neg } 1024), //
    (5, 6, 63, { } 32, { neg } 64), //
    (6, 5, 31, { } 16, { neg } 32), //
    (7, 4, 15, { } 8, { neg } 16));

  samps = 0;
  postshift = 1;
  mask = 2;
  hibit = 3;
  neg = 4;

implementation

{ TQ330 }

class function TQ330.BuilderPacket(AQdpHeader: TQdpHeader; AData: Pointer)
  : TIdBytes;
var
  Buffer: TIdBytes;
begin
  SetLength(Buffer, SizeOf(TQdpHeader));
  CopyMemory(Buffer, @AQdpHeader, SizeOf(TQdpHeader));

  // Data
  SetLength(Buffer, Length(Buffer) + Rev2Bytes(AQdpHeader.Length));
  CopyMemory(@Buffer[SizeOf(TQdpHeader)], AData, Rev2Bytes(AQdpHeader.Length));

  // CRC 제거
  RemoveBytes(Buffer, SizeOf(AQdpHeader.CRC));

  // CRC 생성
  SetLength(result, 0);
  AppendBytes(result, DWordToBytes(CalcCRC32(Buffer)));

  // CRC + Data
  AppendBytes(result, Buffer);
end;

class function TQ330.BuildRawData(ARawData: TIdBytes): TJSONArray;

  function DecompressData(dnib: Byte; curval, accum: Integer): TArray<Integer>;
  var
    subcode: Byte;
    dcp: array [0 .. 4] of DWord;
    unpacked: array [0 .. 6] of DWord;
    I: Cardinal;
    Work: DWord;
  begin
    subcode := 0;
    if dnib = 2 then
    begin
      subcode := (accum shr 30) and 3;
    end
    else if dnib = 3 then
    begin
      subcode := 4 + (accum shr 30) and 3;
    end;

    CopyMemory(@dcp, @DecompTab[subcode], SizeOf(dcp));

    SetLength(result, dcp[samps]);

    for I := dcp[samps] - 1 downto 0 do
    begin
      Work := accum and dcp[mask];
      if (Work and dcp[hibit]) = dcp[hibit] then
      begin
        Work := Work - dcp[neg];
      end;
      unpacked[I] := Work;
      accum := accum shr dcp[postshift];
    end;

    for I := 0 to dcp[samps] - 1 do
    begin
      curval := curval + unpacked[I];
      result[I] := curval;
    end;

  end;

var
  CurrValue: Integer;
  CompressionMap: TIdBytes;
  CurrMap, dnib: Byte;
  Offset: Word;
  DataBlocks: Integer;
  I, J, MapIndex, bitIndex: Integer;
  accum: Integer;
  Datas: TArray<Integer>;
begin

  RemoveBytes(ARawData, 4); // Channel, Length

  CurrValue := Rev4Bytes(BytesToLongInt(ARawData));
  RemoveBytes(ARawData, 4);
  // InitValue

  Offset := BytesToWord(ARawData);
  RemoveBytes(ARawData, 2); // Offset

  Offset := Rev2Bytes(Offset) - 10;

  SetLength(CompressionMap, 0);
  AppendBytes(CompressionMap, ARawData, 0, Offset);
  RemoveBytes(ARawData, Offset); // CompressionMap

  DataBlocks := trunc(Length(ARawData) / 4);
  MapIndex := 0;
  bitIndex := 0;

  result := TJSONArray.Create;

  for I := 0 to DataBlocks - 1 do
  begin
    CopyMemory(@accum, @ARawData[I * 4], SizeOf(accum));
    accum := Rev4Bytes(accum);

    CurrMap := CompressionMap[MapIndex];
    dnib := CurrMap shr (6 - bitIndex) and 3;

    Datas := DecompressData(dnib, CurrValue, accum);
    CurrValue := Datas[Length(Datas) - 1];

    for J := Low(Datas) to High(Datas) do
    begin
      result.Add(Datas[J]);
    end;

    bitIndex := bitIndex + 2;
    if bitIndex > 6 then
    begin
      bitIndex := 0;
      inc(MapIndex);
    end;
  end;
end;

class function TQ330.GetCommandString(ACommand: Byte): String;
begin
  if ACommand = C1_CACK then
    result := 'Command Acknowledge'
  else if ACommand = C1_RQSRV then
    result := 'Request Server Registration'
  else if ACommand = C1_SRVCH then
    result := 'Server Challenge'
  else if ACommand = C1_SRVRSP then
    result := 'Server Response'
  else if ACommand = C1_CERR then
    result := 'Command Error'
  else if ACommand = C1_DSRV then
    result := 'Delete Server'
  else if ACommand = DT_OPEN then
    result := 'Open Data Port'
  else if ACommand = DT_DATA then
    result := 'Dtat Record'
  else if ACommand = DT_DACK then
    result := 'Data Acknowledge'
  else
    result := 'Unknown Command.';
end;

{ TDataHeader }

function TDataHeader.GetDateTime(ADataSeq: Integer): TDateTime;
var
  Sec: Integer;
begin
  Sec := Rev4Bytes(SecondsOffset);
  Sec := Sec + ADataSeq;
  // Sec := Sec + 2000;

  result := IncSecond(StrToDateTime('2000-01-01 09:00:00'), Sec);
end;

end.
