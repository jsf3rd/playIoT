// *******************************************************
//
// playIoT miniseed & steim common file
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@nate.com
//
// *******************************************************

unit JdcMSeed.Common;

interface

uses System.classes, System.SysUtils, Winapi.Windows, JdcGlobal, IdGlobal,
  System.DateUtils, System.Generics.Collections, System.Math;

const
  BLOCKETTE_CODE_1000 = $E803;
  BLOCKETTE_CODE_1001 = $E903;
  BLOCKETTE1001_OFFSET = $3800;

  WORDS_PER_FRAME = 15;
  FRAME_SIZE = (WORDS_PER_FRAME * 4 + 4);
  FRAMES_PER_RECORD = 7;
  RECORD_SIZE = FRAME_SIZE * (FRAMES_PER_RECORD + 1);

type
  TBlocketteTypeException = class(Exception)
    //
  end;

  TPeeks = TArray<Integer>;
  TMSeedContainer = TDictionary<String, TStream>;

  TEncodingFormat = (efAscii = 0, efInt16 = 1, efInt24 = 2, efInt32 = 3,
    efFloat = 4, efDouble = 5, efStaim1 = 10, efStaim2 = 11);

  TSteimType = (stLevel1 = 10, stLevel2 = 11);
  TByteOrder = (boLittle, boBig);

  TPeekValue = record
    DateTime: TDateTime;
    Peek: Integer;

    constructor Create(ADateTime: TDateTime; APeek: Integer);
  end;

  TDataFrame = packed record
    ctrl: UInt32;
    w: array [0 .. WORDS_PER_FRAME - 1] of DWORD;

    procedure Reset;
    constructor Create(AStream: TStream); overload;
  end;

  TDataRecord = Array [0 .. FRAMES_PER_RECORD - 1] of TDataFrame;

  TPeriod = record
    StartDateTime: TDateTime;
    EndDateTime: TDateTime;
    constructor Create(AStart, AEnd: TDateTime);
    function InRange(ADateTime: TDateTime): boolean;
  end;

  TBTime = packed record
  private
    function GetDateTime: TDateTime;
    procedure SetDateTime(const Value: TDateTime);
  public
    year: Word;
    day: Word;
    hour: Byte;
    min: Byte;
    sec: Byte;
    unused: Byte;
    fract: Word;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
  end;

  TMSeedHeader = packed record
    sequence_number: array [0 .. 5] of Byte;
    dataquality: Byte;
    reserved: Byte;
    station: array [0 .. 4] of Byte;
    location: array [0 .. 1] of Byte;
    channel: array [0 .. 2] of Byte;
    network: array [0 .. 1] of Byte;
    start_time: TBTime;
    numsamples: Word;
    samprate_fact: Word;
    samprate_mult: Word;
    act_flags: Byte;
    io_flags: Byte;
    dq_flags: Byte;
    numblockettes: Byte;
    time_correct: Integer;
    data_offset: Word;
    blockette_offset: Word;

    function StationCode: String;
    function ChannelCode: String;
    function ToString: String;
    function Interval: Integer;
    constructor Create(AStream: TStream); overload;
    constructor Create(station, location, channel, network: Ansistring;
      samprate_fact: Word); overload;
  end;

  TBlockette1000 = packed record
    blockette_code: Word;
    next_blockette_offset: Word;
    encoding: Byte;
    byteorder: Byte;
    reclen: Byte;
    reserved: Byte;

    constructor Create(AStream: TStream); overload;
    constructor Create(AType: TSteimType); overload;
    function IsValid: boolean;
    function RecordLength: Integer;
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
    constructor Create(AStream: TStream); overload;
    constructor Create(ATemp: Integer); overload;
    function IsValid: boolean;
    function GetFrameCount: Integer;
  end;

  TFixedHeader = packed record
    Header: TMSeedHeader;
    Blkt1000: TBlockette1000;
    Blkt1001: TBlockette1001;

    constructor Create(AStream: TStream); overload;
    constructor Create(AHeader: TMSeedHeader; AType: TSteimType); overload;
    procedure Validate;
    function IsSteimEncoding: boolean;
    function DataLength: Integer;
    procedure WriteTo(ASteram: TStream);
  private
    procedure Recode4YDP;
  end;

  TCompPacket = record
    frame: Word;
    block: Integer;
    frame_buffer: TDataFrame;

    constructor Create(frame: Word);
    function IsFirstBlock: boolean;
  end;

  TCompSeqType = record
    scan: Integer;
    bc: Cardinal;
    cbits: Integer;
    shift: Integer;
    mask: Integer;
    disc: Integer;
  end;

  TDecompBitType = record
    samps: Integer;
    postshift: Integer;
    mask: Integer;
    hibit: Integer;
    neg: Integer;
  end;

  TRawData = class(TList<TPeekValue>)
  public
    function Peeks: TPeeks;
  end;

  TMSeedCommon = class
  private
    class function CalcSteim2SubCode(dnib: Byte; accum: Integer): Byte;
  public
    class function GetRecordCount(ARecord: TDataRecord;
      AType: TSteimType): Integer;
    class function GetSubCode(steim: TSteimType; dnib: Byte;
      accum: Integer): Byte;

  const
    B1X32 = 0;
    B2X16 = 0;

    B7X4 = 2 shl 2; // * level 2 compression code bits, within each block */
    B6X5 = 1;
    B5X6 = 0;
    B4X8 = 0;
    B3X10 = 3;
    B2X15 = 2;
    B1X30 = 1;

    HUGE = $7FFFFFFF;
    LARGE = $3FFFFFFF;
    BIG = $1FFFFFFF;

    SIGN_MASK = $80000000;
    NEGATIVE = $100000000;

    CompTab: array [0 .. 9, 0 .. 5] of Integer = //
      ( //
      // Steim1
      (4, 1, B4X8, 8, $FF, $7F), // 1Byte
      (2, 2, B2X16, $10, $FFFF, $7FFF), // 2Byte
      (1, 3, B1X32, $20, -1, HUGE), // 4Byte

      // Steim2
      (7, 3, B7X4, 4, $F, 7), // 4bit
      (6, 3, B6X5, 5, $1F, $F), // 5bit
      (5, 3, B5X6, 6, $3F, $1F), // 6bit
      (4, 1, B4X8, 8, $FF, $7F), // 8bit
      (3, 2, B3X10, $A, $3FF, $1FF), // 10bit
      (2, 2, B2X15, $F, $7FFF, $3FFF), // 15bit
      (1, 2, B1X30, $1E, LARGE, BIG) // 30bit
      );

    DecompTab: array [0 .. 9, 0 .. 4] of Integer = //
      ( //
      // Steim1
      (1, 0, -1, 0, 0), // Ref Value
      (4, $8, $FF, $80, $100), // 1Byte
      (2, $10, $FFFF, $8000, $10000), // 2Byte
      (1, 0, -1, 0, 0), // 4Byte

      // Steim2
      (1, $0, LARGE, $20000000, $40000000), // 30bit
      (2, $F, $7FFF, $4000, $8000), // 15bit
      (3, $A, $3FF, $200, $400), // 10bit
      (5, $6, $3F, $20, $40), // 6bit
      (6, $5, $1F, $10, $20), // 5bit
      (7, $4, $F, $8, $10) // 4bit
      );

  end;

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
  CopyMemory(Bytes, @Self.channel, 3);
  result := result + BytesToString(Bytes);

  result := result.Replace(' ', '');
end;

constructor TMSeedHeader.Create(AStream: TStream);
begin
  AStream.Read(Self, SizeOf(Self));
end;

constructor TMSeedHeader.Create(station, location, channel, network: Ansistring;
  samprate_fact: Word);
var
  I: Integer;
begin
  for I := Low(Self.sequence_number) to High(Self.sequence_number) do
    Self.sequence_number[I] := $30;
  Self.dataquality := $44;
  Self.reserved := $20;

  CopyMemory(@Self.station, @station[1], SizeOf(Self.station));
  CopyMemory(@Self.location, @location[1], SizeOf(Self.location));
  CopyMemory(@Self.channel, @channel[1], SizeOf(Self.channel));
  CopyMemory(@Self.network, @network[1], SizeOf(Self.network));

  Self.start_time.SetDateTime(0);
  Self.numsamples := 0;
  Self.samprate_fact := Rev2Bytes(samprate_fact);
  Self.samprate_mult := Rev2Bytes(1);
  Self.act_flags := 0;
  Self.io_flags := 0;
  Self.dq_flags := 0;
  Self.numblockettes := 2;
  Self.time_correct := 0;
  Self.data_offset := Rev2Bytes(64);
  Self.blockette_offset := Rev2Bytes(48);
end;

function TMSeedHeader.Interval: Integer;
begin
  result := Trunc(1000 / Rev2Bytes(samprate_fact));
end;

function TMSeedHeader.StationCode: String;
var
  Bytes: TIdBytes;
begin
  SetLength(Bytes, 5);
  CopyMemory(Bytes, @Self.station, 5);
  result := BytesToString(Bytes);
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
  CopyMemory(Bytes, @Self.channel, 3);
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

function TBTime.GetDateTime: TDateTime;
begin
  result := EncodeDate(Rev2Bytes(Self.year), 1, 1);
  result := IncDay(result, Rev2Bytes(Self.day) - 1);
  result := IncHour(result, Self.hour);
  result := IncMinute(result, Self.min);
  result := IncSecond(result, Self.sec);
  result := IncMilliSecond(result, Trunc(Rev2Bytes(Self.fract) / 10));
end;

procedure TBTime.SetDateTime(const Value: TDateTime);
begin
  Self.year := Rev2Bytes(YearOf(Value));
  Self.day := Rev2Bytes(DayOfTheYear(Value));
  Self.hour := HourOf(Value);
  Self.min := MinuteOf(Value);
  Self.sec := SecondOf(Value);
  Self.unused := 0;
  Self.fract := Rev2Bytes(MilliSecondOf(Value) * 10);
end;

{ TBlockette1000 }

constructor TBlockette1000.Create(AType: TSteimType);
begin
  Self.blockette_code := BLOCKETTE_CODE_1000;
  Self.next_blockette_offset := Rev2Bytes(56);
  Self.encoding := Byte(AType);
  Self.byteorder := 1;
  Self.reclen := 9;
  Self.reserved := 0;
end;

function TBlockette1000.RecordLength: Integer;
begin
  result := Trunc(power(2, Self.reclen));
end;

constructor TBlockette1000.Create(AStream: TStream);
begin
  AStream.Read(Self, SizeOf(Self));

  if Self.blockette_code <> BLOCKETTE_CODE_1000 then
    raise TBlocketteTypeException.Create('Blockette 1000 Code Error, ' +
      IntToStr(Rev2Bytes(Self.blockette_code)));
end;

function TBlockette1000.IsValid: boolean;
begin
  result := Self.blockette_code = BLOCKETTE_CODE_1000;
end;

function TBlockette1000.ToString: String;
begin
  result := 'BlocketteCode=' + IntToStr(Rev2Bytes(Self.blockette_code));
  result := result + ',NextBlocketteOffset=' +
    IntToStr(Rev2Bytes(Self.next_blockette_offset));
  result := result + ',Encoding=' + Self.encoding.ToString;
  result := result + ',ByteOrder=' + Self.byteorder.ToString;
  result := result + ',RecordLength=' + Self.RecordLength.ToString;
end;

{ TBlockette1001 }

constructor TBlockette1001.Create(ATemp: Integer);
begin
  Self.blockette_code := BLOCKETTE_CODE_1001;
  Self.next_blockette_offset := 0;
  Self.timing_qual := 0;
  Self.usec := 0;
  Self.reserved := 0;
  Self.framecnt := FRAMES_PER_RECORD;
end;

function TBlockette1001.GetFrameCount: Integer;
begin
  if IsValid then
    result := framecnt
  else
    result := FRAMES_PER_RECORD;
end;

constructor TBlockette1001.Create(AStream: TStream);
begin
  AStream.Read(Self, SizeOf(Self));
end;

function TBlockette1001.IsValid: boolean;
begin
  result := Self.blockette_code = BLOCKETTE_CODE_1001;
end;

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

{ TMSeedCommon }

class function TMSeedCommon.CalcSteim2SubCode(dnib: Byte; accum: Integer): Byte;
begin
  if dnib = 2 then
  begin
    result := 3 + (accum shr 30) and 3;
  end
  else if dnib = 3 then
  begin
    result := 7 + (accum shr 30) and 3;
  end
  else
    result := dnib;
end;

class function TMSeedCommon.GetSubCode(steim: TSteimType; dnib: Byte;
  accum: Integer): Byte;
begin
  case steim of
    stLevel1:
      begin
        result := dnib;
        if result > 3 then
          raise Exception.Create('Steim SubCode Error. ' + result.ToString);
      end;
    stLevel2:
      begin
        result := TMSeedCommon.CalcSteim2SubCode(dnib, accum);
        if result > 9 then
          raise Exception.Create('Steim SubCode Error. ' + result.ToString);
      end;

  else
    raise Exception.Create('Unknown Steim Type. ' + Integer(steim).ToString);
  end;
end;

class function TMSeedCommon.GetRecordCount(ARecord: TDataRecord;
  AType: TSteimType): Integer;
var
  Flag: DWORD;
  SubCode, dnib: Byte;
  I: Integer;
  dcp: TDecompBitType;
  MyFrame: TDataFrame;
begin
  result := 0;
  for MyFrame in ARecord do
  begin
    Flag := Rev4Bytes(MyFrame.ctrl);
    for I := Low(MyFrame.w) to High(MyFrame.w) do
    begin
      dnib := (Flag shr (28 - I * 2)) and 3;
      SubCode := GetSubCode(AType, dnib, Rev4Bytes(MyFrame.w[I]));

      if SubCode = 0 then
        Continue;

      CopyMemory(@dcp, @DecompTab[SubCode], SizeOf(TDecompBitType));
      result := result + dcp.samps;
    end;
  end;
end;

{ TDataFrame }

constructor TDataFrame.Create(AStream: TStream);
begin
  AStream.Read(Self, SizeOf(Self));
end;

procedure TDataFrame.Reset;
var
  I: Integer;
begin
  Self.ctrl := 0;
  for I := Low(Self.w) to High(Self.w) do
    Self.w[I] := 0;
end;

{ TFixedHeader }

constructor TFixedHeader.Create(AStream: TStream);
begin
  Self.Header := TMSeedHeader.Create(AStream);
  Self.Blkt1000 := TBlockette1000.Create(AStream);
  Self.Blkt1001 := TBlockette1001.Create(AStream);

  // 영등포 구청 전용 코드.
  if Self.Header.StationCode.StartsWith('SLYP') then
    Recode4YDP;
end;

constructor TFixedHeader.Create(AHeader: TMSeedHeader; AType: TSteimType);
begin
  Self.Header := AHeader;
  Self.Blkt1000 := TBlockette1000.Create(AType);
  Self.Blkt1001 := TBlockette1001.Create(0);
end;

function TFixedHeader.DataLength: Integer;
begin
  result := Self.Blkt1000.RecordLength - SizeOf(Self);
end;

function TFixedHeader.IsSteimEncoding: boolean;
var
  Format: TEncodingFormat;
begin
  Format := TEncodingFormat(Blkt1000.encoding);
  result := (Format = TEncodingFormat.efStaim1) or
    (Format = TEncodingFormat.efStaim2);
end;

procedure TFixedHeader.Validate;
begin
  if not((Self.Header.numblockettes = 1) or (Self.Header.numblockettes = 2))
  then
    raise Exception.Create('Blockettes number error ' +
      Self.Header.numblockettes.ToString);

  if not Self.Blkt1000.IsValid then
    raise Exception.Create('Can not find Blockette 1000');
end;

procedure TFixedHeader.WriteTo(ASteram: TStream);
begin
  ASteram.Write(Self, SizeOf(Self));
end;

procedure TFixedHeader.Recode4YDP;
begin
  // Basalt 펌웨어 수정 전 영등포 구청 전용 코드 (ex SLYPG => YPG''''으로 변경)
  if Self.Header.station[0] = Ord('S') then
  begin
    Self.Header.station[0] := Self.Header.station[2];
    Self.Header.station[1] := Self.Header.station[3];
    Self.Header.station[2] := Self.Header.station[4];
    Self.Header.station[3] := $20;
    Self.Header.station[4] := $20;
  end;
  // Basalt 펌웨어 수정 전 영등포 구청 전용 코드
  if Self.Header.channel[1] = Ord('V') then
    Self.Header.channel[1] := Ord('C');
  if Self.Header.channel[1] = Ord('T') then
    Self.Header.channel[1] := Ord('C');
end;

{ TRawData }

function TRawData.Peeks: TPeeks;
var
  I: Integer;
begin
  SetLength(result, Self.count);

  for I := 0 to Self.count - 1 do
    result[I] := Self.Items[I].Peek;
end;

{ TPeekValue }

constructor TPeekValue.Create(ADateTime: TDateTime; APeek: Integer);
begin
  Self.DateTime := ADateTime;
  Self.Peek := APeek;
end;

{ TCompPacket }

constructor TCompPacket.Create(frame: Word);
begin
  Self.frame := frame;
  Self.block := 0;
  Self.frame_buffer.Reset;
end;

function TCompPacket.IsFirstBlock: boolean;
begin
  result := (Self.frame = 0) and (Self.block = 0);
end;

end.
