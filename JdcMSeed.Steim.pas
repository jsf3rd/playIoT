// *******************************************************
//
// playIoT steim compression/decompression at levels 1,2
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
// Reference,
// http://quake.geo.berkeley.edu/qug/software/steimdt1.tar
//
// *******************************************************

unit JdcMSeed.Steim;

interface

uses System.SysUtils, System.classes, Winapi.Windows,
  System.Generics.Collections, JdcGlobal, IdGlobal, JdcMSeed.Common,
  System.Math;

const
  MAXSAMPPERWORD = 7; // Steim2.

type
  TDiffsArray = array [0 .. MAXSAMPPERWORD - 1] of Integer;
  TScanArray = array [0 .. MAXSAMPPERWORD] of Integer;

  TRefValue = record
    CurrValue: Integer;
    LastValue: Integer;
    procedure Reset;
  end;

  TSteimDecoder = class
  private
    FSteimType: TSteimType;
    FByteOrder: TByteOrder; // not used

    IsFirstDiff: boolean;
    FRefValue: TRefValue;
    FPeeks: TList<Integer>;
    FDataRecord: TDataRecord;
    procedure _DecodeData(AIndex: Integer);
    function DecompressSteim(subcode: Byte; accum: Integer): TPeeks;
    procedure SetRefValue(AIndex, AValue: Integer);
  public
    function DecodeData(ADataRecord: TDataRecord): TPeeks;
    constructor Create(AType: TSteimType; AOrder: TByteOrder);
    destructor Destroy; override;
  end;

  TSteimEncoder = class
  private
    FSteimType: TSteimType;
    FFrameCount: Integer;
    FByteOrder: TByteOrder; // not used

    FPeeks: TPeeks;
    FPeekIndex: Integer;
    FLastSample: Integer;
    function _EncodeData: TDataRecord;
    procedure CompressSteim(var CompPacket: TCompPacket);
    function GetCompressSeq(diffs: TDiffsArray; hiscan: Integer): TCompSeqType;
    function BuildDataFrame(AIndex: Integer): TDataFrame;

  public
    function EncodeData(APeeks: TPeeks; AIndex: Integer): TDataRecord;
    constructor Create(AType: TSteimType; AOrder: TByteOrder; ACount: Integer);
  end;

implementation

{ TSteimDecoder }

constructor TSteimDecoder.Create(AType: TSteimType; AOrder: TByteOrder);
begin
  FSteimType := AType;
  FByteOrder := AOrder; // Only Big Endian.

  if FByteOrder = boLittle then
    raise Exception.Create
      ('Decoder "BigEndian" Expected but "LittleEndian" found');

  FRefValue.Reset;
  IsFirstDiff := false;
end;

function TSteimDecoder.DecodeData(ADataRecord: TDataRecord): TPeeks;
var
  I: Integer;
begin
  SetLength(FDataRecord, Length(ADataRecord));
  FDataRecord := ADataRecord;

  FPeeks := TList<Integer>.Create;
  try
    try
      for I := Low(ADataRecord) to High(ADataRecord) do
      begin
        _DecodeData(I);
      end;
    except
      on E: Exception do
        raise Exception.Create('Decode steim error. ' + E.Message);
    end;

    if FRefValue.LastValue <> FPeeks.Items[FPeeks.Count - 1] then
      raise Exception.Create('Data Integrity Error. ' +
        FRefValue.LastValue.ToString + ' <> ' + FPeeks.Items[FPeeks.Count - 1]
        .ToString);

    result := FPeeks.ToArray;
  finally
    FreeAndNil(FPeeks);
  end;
end;

procedure TSteimDecoder.SetRefValue(AIndex, AValue: Integer);
begin
  if AIndex = 2 then
    FRefValue.CurrValue := AValue
  else if AIndex = 4 then
  begin
    FRefValue.LastValue := AValue;
    IsFirstDiff := true;
  end
  else
    // Header, Null
      ;
end;

function TSteimDecoder.DecompressSteim(subcode: Byte; accum: Integer): TPeeks;
var
  I: Integer;
  Work: Integer;
  dcp: TDecompBitType;
  unpacked: TDiffsArray;
begin
  CopyMemory(@dcp, @TMSeedCommon.DecompTab[subcode], SizeOf(dcp));

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

  if IsFirstDiff then
  begin
    IsFirstDiff := false;
    unpacked[0] := 0;
  end;

  for I := 0 to dcp.samps - 1 do
  begin
    FRefValue.CurrValue := FRefValue.CurrValue + unpacked[I];
    result[I] := FRefValue.CurrValue;
  end;
end;

destructor TSteimDecoder.Destroy;
begin
  if Assigned(FPeeks) then
    FreeAndNil(FPeeks);

  inherited;
end;

procedure TSteimDecoder._DecodeData(AIndex: Integer);
var
  Peeks: TPeeks;
  subcode: Byte;
  accum: Integer;

  I, J: Integer;
  CurrMap, dnib: Byte;
  CompressionMap: TIdBytes;
  MapIndex, bitIndex: Integer;
begin
  CompressionMap := ToBytes(FDataRecord[AIndex].ctrl);
  MapIndex := 0;
  bitIndex := 2;

  for I := Low(FDataRecord[AIndex].w) to High(FDataRecord[AIndex].w) do
  begin
    try
      CopyMemory(@accum, @FDataRecord[AIndex].w[I], SizeOf(accum));
      accum := Rev4Bytes(accum);

      CurrMap := CompressionMap[MapIndex];
      dnib := CurrMap shr (6 - bitIndex) and 3;
      subcode := TMSeedCommon.GetSubCode(FSteimType, dnib, accum);

      if dnib = 0 then
      begin
        if (AIndex = 0) and (I < 2) then
          SetRefValue(bitIndex, accum);

        Continue;
      end;
      Peeks := DecompressSteim(subcode, accum);

      for J := Low(Peeks) to High(Peeks) do
      begin
        FPeeks.Add(Peeks[J]);
      end;
    finally
      bitIndex := (bitIndex + 2) mod 8;
      if bitIndex = 0 then
        inc(MapIndex);
    end;
  end;
end;

{ TRefValue }
procedure TRefValue.Reset;
begin
  CurrValue := 0;
  LastValue := 0;
end;

{ TSteimEncoder }

function TSteimEncoder.GetCompressSeq(diffs: TDiffsArray; hiscan: Integer)
  : TCompSeqType;
var
  I: Integer;
  done: boolean;
  ctabx: Integer;
begin
  ctabx := (Integer(FSteimType) and 1) * 3;
  done := false;
  repeat
    CopyMemory(@result, @TMSeedCommon.CompTab[ctabx], SizeOf(TCompSeqType));

    if hiscan < result.scan then
    begin
      inc(ctabx);
      Continue;
    end;

    for I := 0 to result.scan - 1 do
    begin
      if abs(diffs[I]) > result.disc then
      begin
        if (FSteimType = stLevel1) and (ctabx > 2) then
          raise Exception.Create('Overflow steim1. ' + diffs[I].ToString)
        else if (FSteimType = stLevel2) and (ctabx > 9) then
          raise Exception.Create('Overflow steim2. ' + diffs[I].ToString)
        else
          inc(ctabx);
        break;
      end
      else if (I = result.scan - 1) then
        done := true;
    end;
  until (done);

end;

procedure TSteimEncoder.CompressSteim(var CompPacket: TCompPacket);
var
  I: Integer;

  accum: Integer;
  Value: Integer;
  hiscan: Integer;

  sp: TCompSeqType;
  sc: TScanArray;
  diffs: TDiffsArray;
begin
  sc[0] := FLastSample;

  if CompPacket.IsFirstBlock then
  begin
    CompPacket.frame_buffer.w[0] := FPeeks[FPeekIndex];
    CompPacket.frame_buffer.w[1] := 0;
    CompPacket.block := 2;
  end;

  hiscan := 0;
  while (hiscan < MAXSAMPPERWORD) do
  begin
    if FPeekIndex + hiscan >= Length(FPeeks) then
      break;

    Value := FPeeks[FPeekIndex + hiscan];
    sc[hiscan + 1] := Value;
    diffs[hiscan] := Value - sc[hiscan];
    inc(hiscan);
  end;

  sp.scan := 0;
  sp.bc := 0;
  accum := 0;
  if hiscan > 0 then
  begin
    sp := GetCompressSeq(diffs, hiscan);
    accum := sp.cbits;

    for I := 0 to sp.scan - 1 do
    begin
      accum := (accum shl sp.shift) or (sp.mask and diffs[I]);
    end;
  end;

  CompPacket.frame_buffer.w[CompPacket.block] := accum;
  CompPacket.frame_buffer.ctrl := (CompPacket.frame_buffer.ctrl shl 2) + sp.bc;

  FLastSample := sc[sp.scan];
  FPeekIndex := FPeekIndex + sp.scan;
end;

constructor TSteimEncoder.Create(AType: TSteimType; AOrder: TByteOrder;
  ACount: Integer);
begin
  FSteimType := AType;
  FFrameCount := ACount;
  FByteOrder := AOrder; // Only Big Endian.

  if FByteOrder = boLittle then
    raise Exception.Create
      ('Encoder "BigEndian" Expected but "LittleEndian" found');
end;

function TSteimEncoder.EncodeData(APeeks: TPeeks; AIndex: Integer): TDataRecord;
begin
  FPeeks := APeeks;
  FPeekIndex := AIndex;

  if FPeekIndex = 0 then
    FLastSample := FPeeks[FPeekIndex]
  else
    FLastSample := FPeeks[FPeekIndex - 1];

  try
    result := _EncodeData;
  except
    on E: Exception do
      raise Exception.Create('Encode steim error. ' + E.Message);
  end;
end;

function TSteimEncoder._EncodeData: TDataRecord;
var
  I: Integer;
  FrameIndex: Integer;
  DataFrame: TDataFrame;
begin
  SetLength(result, FFrameCount);
  for FrameIndex := Low(result) to High(result) do
  begin
    DataFrame := BuildDataFrame(FrameIndex);

    result[FrameIndex].ctrl := Rev4Bytes(DataFrame.ctrl);
    for I := 0 to WORDS_PER_FRAME - 1 do
      result[FrameIndex].w[I] := Rev4Bytes(DataFrame.w[I]);
  end;
  result[0].w[1] := Rev4Bytes(FLastSample);
end;

function TSteimEncoder.BuildDataFrame(AIndex: Integer): TDataFrame;
var
  CompPacket: TCompPacket;
begin
  CompPacket := TCompPacket.Create(AIndex);
  while CompPacket.block < WORDS_PER_FRAME do
  begin
    CompressSteim(CompPacket);
    inc(CompPacket.block);
  end;
  result := CompPacket.frame_buffer;
end;

end.
