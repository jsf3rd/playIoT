unit JdcMSeed.Common;

interface

uses System.classes, System.SysUtils, Winapi.Windows, JdcGlobal, IdGlobal,
  System.DateUtils, System.Generics.Collections;

type
  TBlocketteTypeException = class(Exception)
  end;

  TRecord = TPair<TDateTime, Integer>;
  TRawData = TList<TRecord>;
  TMSeedContainer = TDictionary<String, TStream>;

  TEncodingFormat = (efAscii = 0, efInt16 = 1, efInt24 = 2, efInt32 = 3,
    efFloat = 4, efDouble = 5, efStaim1 = 10, efStaim2 = 11);

  TByteOrder = (boLittle, boBig);

  TPeriod = record
    StartDateTime: TDateTime;
    EndDateTime: TDateTime;
    constructor Create(AStart, AEnd: TDateTime);
    function InRange(ADateTime: TDateTime): boolean;
  end;

  TBTime = packed record
    year: Word;
    day: Word;
    hour: Byte;
    min: Byte;
    sec: Byte;
    unused: Byte;
    fract: Word;

    function DateTime: TDateTime;
  end;

  TMSeedHeader = packed record
    sequence_number: array [0 .. 5] of Byte;
    dataquality: Byte;
    reserved: Byte;
    station: array [0 .. 4] of Byte;
    location: array [0 .. 1] of Byte;
    Channel: array [0 .. 2] of Byte;
    network: array [0 .. 1] of Byte;
    start_time: TBTime;
    numsamples: Word;
    samprate_fact: Word;
    samprate_mult: Word;
    act_flags: Byte;
    io_flags: Byte;
    dq_flags: Byte;
    numblockettes: Byte;
    time_correct: Int32;
    data_offset: Word;
    blockette_offset: Word;

    function ChannelCode: String;
    function ToString: String;
    function Interval: Integer;
  end;

  TBlockette1000 = packed record
    blockette_code: Word;
    next_blockette_offset: Word;
    encoding: Byte;
    byteorder: Byte;
    reclen: Byte;
    reserved: Byte;

    function ToString: String;
  end;

  TBlockette1001 = packed record
    blockette_code: Word;
    next_blockette_offset: Word;
    timing_qual: Byte;
    usec: Byte;
    reserved: Byte;
    framecnt: Byte;

    function ToString: String;
  end;

const
  BLOCKETTE_CODE_1000 = $E803;
  BLOCKETTE_CODE_1001 = $E903;

implementation

{ TMSeedHeader }

function TMSeedHeader.ChannelCode: String;
var
  Bytes: TIdBytes;
begin
  SetLength(Bytes, 2);
  CopyMemory(Bytes, @Self.network, 2);
  result := BytesToString(Bytes) + '_';

  SetLength(Bytes, 5);
  CopyMemory(Bytes, @Self.station, 5);
  result := result + BytesToString(Bytes);

  SetLength(Bytes, 2);
  CopyMemory(Bytes, @Self.location, 2);
  result := result + BytesToString(Bytes) + '_';

  SetLength(Bytes, 3);
  CopyMemory(Bytes, @Self.Channel, 3);
  result := result + BytesToString(Bytes);
end;

function TMSeedHeader.Interval: Integer;
begin
  result := Trunc(1000 / Rev2Bytes(samprate_fact));
end;

function TMSeedHeader.ToString: String;
var
  Bytes: TIdBytes;
begin
  SetLength(Bytes, 5);
  CopyMemory(Bytes, @Self.sequence_number, 5);
  result := 'SequenceNumber=' + BytesToString(Bytes);

  SetLength(Bytes, 1);
  CopyMemory(Bytes, @Self.dataquality, 1);
  result := result + ',DataHeaderQualityID=' + BytesToString(Bytes);

  SetLength(Bytes, 5);
  CopyMemory(Bytes, @Self.station, 5);
  result := result + ',StationCode=' + BytesToString(Bytes);

  SetLength(Bytes, 2);
  CopyMemory(Bytes, @Self.location, 2);
  result := result + ',Location=' + BytesToString(Bytes);

  SetLength(Bytes, 3);
  CopyMemory(Bytes, @Self.Channel, 3);
  result := result + ',Channel=' + BytesToString(Bytes);

  SetLength(Bytes, 2);
  CopyMemory(Bytes, @Self.network, 2);
  result := result + ',Network=' + BytesToString(Bytes);

  result := result + ',StartTime=' + FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz',
    Self.start_time.DateTime);

  result := result + ',NumberOfSamples=' + IntToStr(Rev2Bytes(Self.numsamples));
  result := result + ',SampleRate=' + IntToStr(Rev2Bytes(Self.samprate_fact));
  result := result + ',NumberOfBlockettes=' + Self.numblockettes.ToString;
  result := result + ',TimeCorrection=' + Rev4Bytes(Self.time_correct).ToString;
  result := result + ',DataOffset=' + IntToStr(Rev2Bytes(Self.data_offset));
  result := result + ',BlocketteOffset=' +
    IntToStr(Rev2Bytes(Self.blockette_offset));

end;

{ TBTime }

function TBTime.DateTime: TDateTime;
begin
  result := EncodeDate(Rev2Bytes(Self.year), 1, 1);
  result := IncDay(result, Rev2Bytes(Self.day) - 1);
  result := IncHour(result, Self.hour);
  result := IncMinute(result, Self.min);
  result := IncSecond(result, Self.sec);
  result := IncMilliSecond(result, Trunc(Rev2Bytes(Self.fract) / 10));
end;

{ TBlockette1000 }

function TBlockette1000.ToString: String;
begin
  result := 'BlocketteCode=' + IntToStr(Rev2Bytes(Self.blockette_code));
  result := result + ',NextBlocketteOffset=' +
    IntToStr(Rev2Bytes(Self.next_blockette_offset));
  result := result + ',Encoding=' + Self.encoding.ToString;
  result := result + ',ByteOrder=' + Self.byteorder.ToString;
  result := result + ',RecordLength=' + Self.reclen.ToString;
end;

{ TBlockette1001 }

function TBlockette1001.ToString: String;
begin
  result := 'BlocketteCode=' + IntToStr(Rev2Bytes(Self.blockette_code));
  result := result + ',NextBlocketteOffset=' +
    IntToStr(Rev2Bytes(Self.next_blockette_offset));
  result := result + ',TimeQuality=' + Self.timing_qual.ToString;
  result := result + ',Microsec=' + Self.usec.ToString;
  result := result + ',FrameCount=' + Self.framecnt.ToString;
end;

{ TPeriod }

constructor TPeriod.Create(AStart, AEnd: TDateTime);
begin
  Self.StartDateTime := AStart;
  Self.EndDateTime := AEnd;
end;

function TPeriod.InRange(ADateTime: TDateTime): boolean;
begin
  result := (ADateTime >= Self.StartDateTime) and
    (ADateTime < Self.EndDateTime);
end;

end.
