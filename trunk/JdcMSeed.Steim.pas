unit JdcMSeed.Steim;

interface

uses System.SysUtils, System.classes, Winapi.Windows,
  System.Generics.Collections, JdcGlobal, IdGlobal, JdcMSeed.Common,
  System.Math;

type
  TRefValue = record
    InitValue: Integer;
    LastValue: Integer;
    CurrValue: Integer;

    procedure Reset;
    function Initialized: boolean;
  end;

  TCompPacket = record
    last_sample: Integer;
    frame_buffer: TDataFrame;
    frame: word;
    block: Integer;
    peeks: TArray<TRecord>;
  end;

  TSteim = class abstract
  private
    FSteimType: TSteimType;
    FByteOrder: TByteOrder; // not used
    FRefValue: TRefValue;
  public
    constructor Create(AType: TSteimType; AOrder: TByteOrder);
  end;

  TSteimDecoder = class(TSteim)
  private
    function _DecodeData(AFrame: TDataFrame): TArray<Integer>;
    function DecompressSteim(subcode: Byte; accum: Integer): TArray<Integer>;
    procedure SetRefValue(AIndex, AValue: Integer);
  public
    function DecodeData(AFrame: TDataFrame): TArray<Integer>;
  end;

  TSteimEncoder = class(TSteim)
  private
    function _EncodeData(var AValue: TCompPacket;
      const AIndex: Integer): Integer;
  public
    function EncodeData(ARawData: TArray<TRecord>; AIndex: Integer): TBlockette;
  end;

implementation

{ TSteim }

constructor TSteim.Create(AType: TSteimType; AOrder: TByteOrder);
begin
  FSteimType := AType;
  FByteOrder := AOrder;
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
        subcode := TMSeedCommon.GetSubCode(FSteimType, dnib, accum);

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

function TSteimEncoder._EncodeData(var AValue: TCompPacket;
  const AIndex: Integer): Integer;
var
  hiscan: Integer;
  ctabx: Integer;
  sp: TCompSeqType;

  done: boolean;
  t_scan: Integer;
  block_code: Cardinal;
  accum: Integer;

  value: Integer;
  I: Integer;

  diffs: array [0 .. MAXSAMPPERWORD - 1] of Integer;
  sc: array [0 .. MAXSAMPPERWORD] of Integer;
begin

  if AValue.block = 0 then
  begin
    AValue.frame_buffer.ctrl := 0;

    if AValue.frame = 0 then
    begin
      AValue.frame_buffer.w[0] := AValue.peeks[AIndex].value;
      AValue.frame_buffer.w[1] := 0;
      AValue.block := 2;
    end;
  end;

  sc[0] := AValue.last_sample;
  ctabx := (Integer(FSteimType) and 1) * 3;

  hiscan := 0;
  done := false;

  repeat
    CopyMemory(@sp, @Compseq[ctabx], SizeOf(TCompSeqType));
    t_scan := sp.scan;

    while (hiscan < t_scan) and (AIndex + hiscan < Length(AValue.peeks)) do
    begin
      value := AValue.peeks[AIndex + hiscan].value;
      sc[hiscan + 1] := value;
      diffs[hiscan] := value - sc[hiscan];
      inc(hiscan);
    end;

    if hiscan = 0 then
    begin
      t_scan := hiscan;
      break;
    end;

    if hiscan < t_scan then
    begin
      inc(ctabx);
      Continue;
    end;

    for I := 0 to t_scan - 1 do
    begin
      if abs(diffs[I]) > sp.disc then
      begin
        if (FSteimType = stLevel1) and (ctabx > 2) then
          raise Exception.Create('Overflow Steim1. ' + diffs[I].ToString)
        else if (FSteimType = stLevel2) and (ctabx > 9) then
          raise Exception.Create('Steim2 Overflow. ' + diffs[I].ToString)
        else
          inc(ctabx);
        break;
      end
      else if (I = t_scan - 1) then
        done := true;
    end;
  until (done);

  if t_scan > 0 then
  begin
    accum := sp.cbits;
    block_code := sp.bc;
  end
  else
  begin
    accum := 0;
    block_code := 0;
  end;

  for I := 0 to t_scan - 1 do
  begin
    accum := (accum shl sp.shift) or (sp.mask and diffs[I]);
  end;

  AValue.frame_buffer.w[AValue.block] := accum;
  AValue.frame_buffer.ctrl := (AValue.frame_buffer.ctrl shl 2) + block_code;
  AValue.last_sample := sc[t_scan];
  result := t_scan;
end;

function TSteimEncoder.EncodeData(ARawData: TArray<TRecord>; AIndex: Integer)
  : TBlockette;
var
  ACompPacket: TCompPacket;
  Index: Integer;
  I: Integer;
begin
  Index := AIndex;

  ACompPacket.peeks := ARawData;
  if AIndex = 0 then
    ACompPacket.last_sample := 0
  else
    ACompPacket.last_sample := ARawData[AIndex - 1].value;

  ACompPacket.frame := 0;
  while ACompPacket.frame < FRAMES_PER_RECORD do
  begin
    ACompPacket.block := 0;
    ACompPacket.frame_buffer.Reset;

    while ACompPacket.block < WORDS_PER_FRAME do
    begin
      Index := Index + _EncodeData(ACompPacket, Index);
      inc(ACompPacket.block);
    end;
    result[ACompPacket.frame].ctrl := Rev4Bytes(ACompPacket.frame_buffer.ctrl);

    for I := 0 to WORDS_PER_FRAME do
      result[ACompPacket.frame].w[I] :=
        Rev4Bytes(ACompPacket.frame_buffer.w[I]);

    inc(ACompPacket.frame);
  end;

  result[0].w[1] := Rev4Bytes(ACompPacket.last_sample);
end;

end.
