unit JdcMSeed.Steim;

interface

uses System.SysUtils, System.classes, Winapi.Windows,
  System.Generics.Collections, JdcGlobal, IdGlobal, JdcMSeed.Common;

const
  MAXSAMPPERWORD = 7;

type
  TRefValue = record
    InitValue: Integer;
    LastValue: Integer;
    CurrValue: Integer;

    procedure Reset;
    function Initialized: boolean;
  end;

  TSteimType = (stLevel1 = 10, stLevel2 = 11);

  TCompPacket = record
    last_sample: Integer;
    flag_word: Integer;
    records_written: Integer;
    frame_buffer: TDataFrame;
    frame: word;
    maxframes: word;
    ctabx: Integer;
    block: Integer;
    peek_total: Integer;
    next_in: Integer;
    next_out: Integer;

    time_mark_sample: Integer;
    nex_compressed_sample: Integer;
    blockette_count: Integer;
    blockette_index: Integer;
    last_blockette: Integer;
    diffs: array [0 .. MAXSAMPPERWORD - 1] of Integer;
    sc: array [0 .. MAXSAMPPERWORD] of Integer;
    peeks: TArray<Integer>;
  end;

  TSteim = class abstract
  private
    FSteimType: TSteimType;
    FByteOrder: Byte; // not used
    FRefValue: TRefValue;
  public
    constructor Create(AType: TSteimType; AOrder: Byte);
  end;

  TSteimDecoder = class(TSteim)
  private
    function GetSubCode(dnib: Byte; accum: Integer): Byte;

    function _DecodeData(AFrame: TDataFrame): TArray<Integer>;
    function DecompressSteim(subcode: Byte; accum: Integer): TArray<Integer>;
    procedure SetRefValue(AIndex, AValue: Integer);
  public
    function DecodeData(AFrame: TDataFrame): TArray<Integer>;
  end;

  TSteimEncoder = class(TSteim)
  private
    function _EncodeData(var AValue: TCompPacket): Integer;
  public
    function EncodeData(ARawData: TArray<Integer>): TDataFrame;
  end;

  TDecompBitType = record
    samps: Integer;
    postshift: Integer;
    mask: Integer;
    hibit: Integer;
    neg: Integer;
  end;

  TCompSeqType = record
    scan: Integer;
    bc: Integer;
    cbits: Integer;
    shift: Integer;
    mask: Integer;
    disc: Integer;
  end;

const
  B1X32 = 0;
  B2X16 = 0;

  B7X4 = 2; // * level 2 compression code bits, within each block */
  B6X5 = 1;
  B5X6 = 0;
  B4X8 = 0;
  B3X10 = 3;
  B2X15 = 2;
  B1X30 = 1;

  HUGE = $7FFFFFFF;
  LARGE = $3FFFFFFF;
  BIG = $1FFFFFFF;

  PEEKMASK = $F;
  WORDS_PER_FRAME = 15;
  FRAME_SIZE = (WORDS_PER_FRAME * 4);
  FRAMES_PER_RECORD = 8; //
  LIB_REC_SIZE = (FRAME_SIZE * FRAMES_PER_RECORD);
  // normal miniseed is 512 byte record

  Compseq: array [0 .. 8, 0 .. 5] of Integer = //
    ( //
    // Steim1
    (4, 1, B4X8, 8, $FF, $7F), // 1Byte
    (2, 2, B2X16, $10, $FFFF, $7FFF), // 2Byte
    (1, 3, B1X32, $20, -1, HUGE), // 4Byte

    // Steim2
    (7, 3, B7X4, 4, $F, 7), // 4bit
    (6, 3, B6X5, 5, $1F, $F), // 5bit
    (5, 3, B5X6, 6, $3F, $1F), // 6bit
    (3, 2, B3X10, $A, $3FF, $1FF), // 10bit
    (2, 2, B2X15, $F, $7FFF, $3FFF), // 15bit
    (1, 2, B1X30, $1E, LARGE, BIG) // 30bit
    );

  DecompTab: array [0 .. 9, 0 .. 4] of Integer = //
    ( //
    // Steim1
    (1, $0, -1, 0, 0), // Ref Value
    (4, $8, $FF, $80, $100), // 1Byte
    (2, $F, $FFFF, $8000, $10000), // 2Byte
    (1, $0, -1, 0, 0), // 4Byte

    // Steim2
    (1, $0, $3FFFFFFF, $20000000, $40000000), // 30bit
    (2, $F, $7FFF, $4000, $8000), // 15bit
    (3, $A, $3FF, $200, $400), // 10bit
    (5, $6, $3F, $20, $40), // 6bit
    (6, $5, $1F, $10, $20), // 5bit
    (7, $4, $F, $8, $10) // 4bit
    );

implementation

{ TSteim }

constructor TSteim.Create(AType: TSteimType; AOrder: Byte);
begin
  FSteimType := AType;
  FByteOrder := AOrder; // 0: Little, 1: Big
  FRefValue.Reset;
end;

{ TSteimDecoder }

function TSteimDecoder.DecodeData(AFrame: TDataFrame): TArray<Integer>;
begin
  try
    result := _DecodeData(AFrame);
  except
    on E: Exception do
      raise Exception.Create('Decode Steim Error. ' + E.Message);
  end;
end;

function TSteimDecoder.DecompressSteim(subcode: Byte; accum: Integer)
  : TArray<Integer>;
var
  dcp: TDecompBitType;
  unpacked: array [0 .. 6] of Int32;
  I: Integer;
  Work: Int32;

  curval: Integer;
begin
  curval := FRefValue.CurrValue;

  CopyMemory(@dcp, @DecompTab[subcode], SizeOf(dcp));

  SetLength(result, dcp.samps);

  for I := dcp.samps - 1 downto 0 do
  begin
    Work := accum and dcp.mask;
    if (Work and dcp.hibit) = dcp.hibit then
    begin
      Work := Work - dcp.neg;
    end;
    unpacked[I] := Work;
    accum := accum shr dcp.postshift;
  end;

  if FRefValue.Initialized then
    unpacked[0] := FRefValue.InitValue;

  for I := 0 to dcp.samps - 1 do
  begin
    curval := curval + unpacked[I];
    result[I] := curval;
  end;

end;

function TSteimDecoder.GetSubCode(dnib: Byte; accum: Integer): Byte;
  function CalcSteim2SubCode: Byte;
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

begin
  case FSteimType of
    stLevel1:
      result := dnib;
    stLevel2:
      result := CalcSteim2SubCode;
  else
    raise Exception.Create('Unknown Steim Type. ' + Integer(FSteimType)
      .ToString);
  end;
end;

procedure TSteimDecoder.SetRefValue(AIndex, AValue: Integer);
begin
  if AIndex = 2 then
    FRefValue.InitValue := AValue
  else if AIndex = 4 then
    FRefValue.LastValue := AValue
  else if AIndex = 0 then
    // Header
      ;
end;

function TSteimDecoder._DecodeData(AFrame: TDataFrame): TArray<Integer>;
var
  CompressionMap: TIdBytes;
  CurrMap, dnib: Byte;
  I, J, MapIndex, bitIndex: Integer;
  accum: Integer;
  Datas: TArray<Integer>;
  subcode: Byte;
  ResultData: TList<Integer>;
begin
  CompressionMap := ToBytes(AFrame.ctrl);
  MapIndex := 0;
  bitIndex := 2;

  ResultData := TList<Integer>.Create;
  try
    for I := Low(AFrame.w) to High(AFrame.w) do
    begin
      try
        CopyMemory(@accum, @AFrame.w[I], SizeOf(accum));
        accum := Rev4Bytes(accum);

        CurrMap := CompressionMap[MapIndex];
        dnib := CurrMap shr (6 - bitIndex) and 3;
        subcode := GetSubCode(dnib, accum);

        Datas := DecompressSteim(subcode, accum);

        // Ref Data
        if dnib = 0 then
        begin
          SetRefValue(bitIndex, Datas[0]);
          Continue;
        end;
        FRefValue.CurrValue := Datas[Length(Datas) - 1];

        for J := Low(Datas) to High(Datas) do
        begin
          ResultData.Add(Datas[J]);
        end;
      finally
        bitIndex := (bitIndex + 2) mod 8;
        if bitIndex = 0 then
          inc(MapIndex);
      end;
    end;
    result := ResultData.ToArray;
  finally
    ResultData.Free;
  end;
end;

{ TRefValue }

function TRefValue.Initialized: boolean;
begin
  result := (InitValue > Integer.MinValue) and (LastValue > Integer.MinValue)
    and (CurrValue = Integer.MinValue);
end;

procedure TRefValue.Reset;
begin
  InitValue := Integer.MinValue;
  LastValue := Integer.MinValue;
  CurrValue := Integer.MinValue;
end;

{ TSteimEncoder }

function TSteimEncoder._EncodeData(var AValue: TCompPacket): Integer;
var
  ctablo: Integer;
  hiscan: Integer;
  ctabw: Integer;

  sp: TCompSeqType;

  done: boolean;
  t_scan: Integer;
  t_shift: Integer;
  t_mask: Integer;
  t_disc: Integer;
  ctabfit: Integer;
  block_code: Integer;
  accum: Integer;

  value: Integer;

  I: Integer;
begin

  if ((AValue.frame mod 7) = 0) and (AValue.next_out = 0) then
  begin
    AValue.frame_buffer.w[0] := AValue.peeks[AValue.next_out];
    AValue.frame_buffer.w[1] := 0;
    AValue.block := 2;
    AValue.flag_word := 0;
    AValue.ctabx := 0;
  end;

  AValue.sc[0] := AValue.last_sample;

  //
  ctablo := 0;
  hiscan := 0;
  done := false;
  ctabfit := -1;
  ctabw := AValue.ctabx;

  repeat
    CopyMemory(@sp, @Compseq[AValue.ctabx], SizeOf(TCompSeqType));
    t_scan := sp.scan;
    t_disc := sp.disc;

    while hiscan < t_scan do
    begin
      value := AValue.peeks[AValue.next_out + hiscan];
      AValue.sc[hiscan + 1] := value;
      AValue.diffs[hiscan] := value - AValue.sc[hiscan];
      inc(hiscan);
    end;

    for I := 0 to t_scan - 1 do
    begin
      if abs(AValue.diffs[I]) > t_disc then
      begin
        if ctabfit < 0 then
        begin
          if AValue.ctabx >= 2 then
          begin
            raise Exception.Create('Overflow.');
            done := true;
          end
          else
            inc(AValue.ctabx);
        end
        else
        begin
          AValue.ctabx := ctabfit;
          done := true;
        end;
        break;
      end
      else if (I = t_scan - 1) then
      begin
        if AValue.ctabx > ctabw then
          done := true
        else
        begin
          if AValue.ctabx > ctablo then
          begin
            ctabfit := AValue.ctabx;
            dec(AValue.ctabx);
          end
          else
            done := true;
        end;
        break;
      end;
    end;
  until (done);

  CopyMemory(@sp, @Compseq[AValue.ctabx], SizeOf(TCompSeqType));
  t_scan := sp.scan;
  t_shift := sp.shift;
  t_mask := sp.mask;
  AValue.peek_total := AValue.peek_total - t_scan;
  AValue.next_out := AValue.next_out + t_scan;
  block_code := sp.bc;
  accum := sp.cbits;

  for I := 0 to t_scan - 1 do
  begin
    accum := (accum shl t_shift) or (t_mask and AValue.diffs[I]);
  end;

  AValue.frame_buffer.w[AValue.block] := accum;
  AValue.flag_word := (AValue.flag_word shl 2) + block_code;
  AValue.last_sample := AValue.sc[t_scan];
  inc(AValue.block);

  if AValue.block >= WORDS_PER_FRAME then
  begin
    AValue.frame_buffer.ctrl := AValue.flag_word;
    //
    inc(AValue.frame);
    AValue.block := 0;
    AValue.flag_word := 0;
    //
  end;

  result := t_scan;
end;

function TSteimEncoder.EncodeData(ARawData: TArray<Integer>): TDataFrame;
var
  ACompPacket: TCompPacket;
begin
  ACompPacket.peeks := ARawData;
  ACompPacket.next_out := 0;
  ACompPacket.last_sample := 0;
  ACompPacket.frame := 0;

  while ACompPacket.next_out < Length(ARawData) do
  begin
    _EncodeData(ACompPacket);

  end;
end;

end.
