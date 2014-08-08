// *******************************************************
//
// Judico miniSeed file parser
//
// Copyright(c) 2014 ENBGroup.
//
// jsf3rd@enbgroup.co.kr
//
// *******************************************************

unit JdcMSeed;

interface

uses System.classes, System.SysUtils, Winapi.Windows, JdcGlobal,
  System.DateUtils, System.Generics.Collections, Math,
  JdcMSeed.Steim, JdcMSeed.Common, System.IOUtils;

type
  TMSeedFile = class
  private
    FDateTime: TDateTime;
    FContainer: TMSeedContainer;
  private
    procedure _AddFile(AFile: String);
    procedure SaveEachChannel(AStream: TStream);

    function _ExtractRawData(AStream: TStream; APeriod: TPeriod): TRawData;
    function ExtractRawData(ACode: String; APeriod: TPeriod): TRawData;

    procedure _ExtractToASCii(APath, ACode: String; APeriod: TPeriod);
    procedure SaveToASCiiFile(APath: String; RawData: TRawData);
    procedure SaveToBinaryFile(APath, ACode: String);

    procedure _ExtractToMSeed(APath, ACode: String; AType: TSteimType;
      APeriod: TPeriod);
    procedure SaveToMSeed(APath: String; ARawData: TRawData;
      AFixedHeader: TFixedHeader);
  public
    constructor Create(AFile: String = '');
    destructor Destroy; override;

    procedure AddFile(AFile: String);

    procedure Clear;
    procedure RevemoveChannel(AChannel: String);

    function GetChannelList: TArray<String>;

    procedure AsciiToMSeed(ASource: String; AHeader: TMSeedHeader;
      AType: TSteimType);

    procedure ExtractToASCii(APath, ACode: String); overload;
    procedure ExtractToASCii(APath, ACode: String;
      ABegin, AEnd: TDateTime); overload;

    procedure ExtractToMSeed(APath, ACode: String; AType: TSteimType); overload;
    procedure ExtractToMSeed(APath, ACode: String; AType: TSteimType;
      ABegin, AEnd: TDateTime); overload;
  end;

implementation

{ TMSeedFile }

procedure TMSeedFile.Clear;
var
  MyElem: TPair<String, TStream>;
begin
  for MyElem in FContainer do
  begin
    MyElem.Value.Free;
  end;
  FContainer.Clear;
end;

constructor TMSeedFile.Create(AFile: String = '');
begin
  FContainer := TMSeedContainer.Create;
  AddFile(AFile);
end;

destructor TMSeedFile.Destroy;
var
  MyElem: TPair<String, TStream>;
begin
  for MyElem in FContainer do
  begin
    MyElem.Value.Free;
  end;
  FreeAndNil(FContainer);

  inherited;
end;

procedure TMSeedFile.SaveEachChannel(AStream: TStream);
var
  tmp: TBytes;

  Channel: String;
  Buffer: TStream;
  FixedHeader: TFixedHeader;
begin
  FixedHeader := TFixedHeader.Create(AStream);

  Channel := FixedHeader.Header.ChannelCode;
  if not FContainer.ContainsKey(Channel) then
    FContainer.Add(Channel, TBytesStream.Create);

  Buffer := FContainer.Items[Channel];
  FixedHeader.WriteTo(Buffer);

  SetLength(tmp, FixedHeader.DataLength);
  AStream.Read(tmp, Length(tmp));
  Buffer.Write(tmp, Length(tmp));
end;

procedure TMSeedFile._AddFile(AFile: String);
var
  Stream: TStream;
begin
  Stream := TFile.OpenRead(AFile);
  try
    Stream.Position := 0;
    while Stream.Position < Stream.Size do
    begin
      SaveEachChannel(Stream);
    end;
  finally
    Stream.Free;
  end;
end;

function SteimDecoderFactory(AParam: TBlockette1000): TSteimDecoder;
var
  Format: TEncodingFormat;
begin
  Format := TEncodingFormat(AParam.encoding);
  if (Format <> efStaim1) and (Format <> efStaim2) then
    raise Exception.Create('This encoding format is not surpported. ' +
      AParam.encoding.ToString);

  result := TSteimDecoder.Create(TSteimType(Format),
    TByteOrder(AParam.byteorder));
end;

function TMSeedFile._ExtractRawData(AStream: TStream; APeriod: TPeriod)
  : TRawData;
var
  I: Integer;
  Peeks: TPeeks;
  MyElem: Integer;
  FixedHeader: TFixedHeader;
  SteimDecoder: TSteimDecoder;
  DataRecord: TArray<TDataFrame>;
begin
  result := TRawData.Create;

  AStream.Position := 0;
  while AStream.Position < AStream.Size do
  begin
    FixedHeader := TFixedHeader.Create(AStream);
    if not FixedHeader.IsValid then
    begin
      AStream.Position := AStream.Position + FixedHeader.DataLength;
      Continue;
    end;

    SetLength(DataRecord, FixedHeader.Blkt1001.framecnt);
    for I := Low(DataRecord) to High(DataRecord) do
    begin
      DataRecord[I] := TDataFrame.Create(AStream);
    end;

    SteimDecoder := SteimDecoderFactory(FixedHeader.Blkt1000);
    try
      Peeks := SteimDecoder.DecodeData(DataRecord);
      FDateTime := FixedHeader.Header.start_time.DateTime;
      for MyElem in Peeks do
      begin
        if APeriod.InRange(FDateTime) then
          result.Add(TPeekValue.Create(FDateTime, MyElem));

        FDateTime := IncMilliSecond(FDateTime, FixedHeader.Header.Interval);
      end;
    finally
      SteimDecoder.Free;
    end;
  end;
end;

procedure TMSeedFile._ExtractToASCii(APath, ACode: String; APeriod: TPeriod);
var
  RawData: TRawData;
begin
  RawData := ExtractRawData(ACode, APeriod);
  try
    if RawData.Count > 0 then
      SaveToASCiiFile(APath, RawData)
    else
      SaveToBinaryFile(APath, ACode);
  finally
    RawData.Free;
  end;
end;

procedure TMSeedFile.SaveToASCiiFile(APath: String; RawData: TRawData);
var
  I: Integer;
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    for I := 0 to RawData.Count - 1 do
      StringList.Add(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz',
        RawData.Items[I].DateTime) + ' ' + RawData.Items[I].Peek.ToString);
    StringList.SaveToFile(APath);
  finally
    StringList.Free;
  end;
end;

procedure TMSeedFile.SaveToBinaryFile(APath, ACode: String);
var
  Stream: TStream;
begin
  Stream := FContainer.Items[ACode];
  if Stream.Size = 0 then
    Exit;

  Stream.Position := 0;
  TBytesStream(Stream).SaveToFile(APath);
end;

procedure TMSeedFile.SaveToMSeed(APath: String; ARawData: TRawData;
  AFixedHeader: TFixedHeader);
var
  Index: Integer;
  Stream: TStream;
  SampleNum: Integer;
  SteimType: TSteimType;
  DataRecord: TDataRecord;
  SteimEncoder: TSteimEncoder;
begin
  SteimType := TSteimType(AFixedHeader.Blkt1000.encoding);

  Stream := TFile.Create(APath);
  try
    SteimEncoder := TSteimEncoder.Create(SteimType, boBig);
    try
      Index := 0;
      while Index < ARawData.Count do
      begin
        AFixedHeader.Header.start_time.DateTime :=
          ARawData.Items[Index].DateTime;

        DataRecord := SteimEncoder.EncodeData(ARawData.Peeks, Index);
        SampleNum := TMSeedCommon.GetRecordCount(DataRecord, SteimType);
        Index := Index + SampleNum;

        AFixedHeader.Header.numsamples := Rev2Bytes(SampleNum);
        AFixedHeader.WriteTo(Stream);
        Stream.Write(@DataRecord, SizeOf(TDataRecord));
      end;
    finally
      SteimEncoder.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TMSeedFile.AsciiToMSeed(ASource: String; AHeader: TMSeedHeader;
  AType: TSteimType);

  function ReadFile(AFile: String): TRawData;
  var
    tmp: String;
    MyRecord: String;
    DateTime: TDateTime;
    RawList: TStringList;
  begin
    result := TRawData.Create;

    RawList := TStringList.Create;
    try
      RawList.LoadFromFile(AFile);
      result.Capacity := RawList.Count;

      for MyRecord in RawList do
      begin
        tmp := MyRecord.Split([' '])[0] + ' ' + MyRecord.Split([' '])[1];
        DateTime := StrToDateTime(tmp);

        tmp := MyRecord.Split([' '])[2];
        result.Add(TPeekValue.Create(DateTime, tmp.ToInteger));
      end;
    finally
      RawList.Free;
    end;
  end;

var
  RawData: TRawData;
  FileName: String;
begin
  RawData := ReadFile(ASource);
  try
    FileName := ChangeFileExt(ASource, '.mseed');
    SaveToMSeed(FileName, RawData, TFixedHeader.Create(AHeader, AType));
  finally
    RawData.Free;
  end;
end;

procedure TMSeedFile._ExtractToMSeed(APath, ACode: String; AType: TSteimType;
  APeriod: TPeriod);
var
  Stream: TStream;
  RawData: TRawData;
  FixedHeader: TFixedHeader;
begin
  Stream := FContainer.Items[ACode];
  Stream.Position := 0;
  FixedHeader := TFixedHeader.Create(TMSeedHeader.Create(Stream), AType);

  if not FixedHeader.IsValid then
  begin
    SaveToBinaryFile(APath, ACode);
    Exit;
  end;

  RawData := ExtractRawData(ACode, APeriod);
  try
    SaveToMSeed(APath, RawData, FixedHeader)
  finally
    RawData.Free;
  end;
end;

procedure TMSeedFile.AddFile(AFile: String);
begin
  if AFile = '' then
    Exit;

  if not TFile.Exists(AFile) then
    raise Exception.Create('File not exist. ' + AFile);

  try
    _AddFile(AFile);
  except
    on E: Exception do
      raise Exception.Create('Can not add file. ' + E.Message);
  end;
end;

function TMSeedFile.ExtractRawData(ACode: String; APeriod: TPeriod): TRawData;
begin
  if not FContainer.ContainsKey(ACode) then
    raise Exception.Create('Channel code error. ' + ACode);

  try
    result := _ExtractRawData(FContainer.Items[ACode], APeriod);
  except
    on E: Exception do
      raise Exception.Create('Extract rawData error.' + E.Message);
  end;
end;

procedure TMSeedFile.ExtractToASCii(APath, ACode: String;
  ABegin, AEnd: TDateTime);
begin
  try
    _ExtractToASCii(APath, ACode, TPeriod.Create(ABegin, AEnd));
  except
    on E: Exception do
      raise Exception.Create('Extract to ascii error. ' + ACode + ', ' +
        E.Message);
  end;
end;

procedure TMSeedFile.ExtractToMSeed(APath, ACode: String; AType: TSteimType);
begin
  ExtractToMSeed(APath, ACode, AType, MinDateTime, MaxDateTime);
end;

procedure TMSeedFile.ExtractToASCii(APath, ACode: String);
begin
  ExtractToASCii(APath, ACode, MinDateTime, MaxDateTime);
end;

procedure TMSeedFile.ExtractToMSeed(APath, ACode: String; AType: TSteimType;
  ABegin, AEnd: TDateTime);
begin
  try
    _ExtractToMSeed(APath, ACode, AType, TPeriod.Create(ABegin, AEnd));
  except
    on E: Exception do
      raise Exception.Create('Extract to mseed error. ' + ACode + ', ' +
        E.Message);
  end;
end;

function TMSeedFile.GetChannelList: TArray<String>;
begin
  result := FContainer.Keys.ToArray;
end;

procedure TMSeedFile.RevemoveChannel(AChannel: String);
begin
  FContainer.Items[AChannel].Free;
  FContainer.Remove(AChannel);
end;

end.
