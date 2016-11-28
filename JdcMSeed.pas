// *******************************************************
//
// playIoT miniSeed file parser
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
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
    procedure _AddStream(var Stream: TStream);
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

    function _GetStartTime(AStream: TStream): TDateTime;
  public
    constructor Create(AFile: String = '');
    destructor Destroy; override;

    procedure AddFile(AFile: String);
    procedure AddStream(AStream: TStream);

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

    function GetStartTime(ACode: string): TDateTime;
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
    _AddStream(Stream);
  finally
    Stream.Free;
  end;
end;

function SteimDecoderFactory(AParam: TBlockette1000): TSteimDecoder;
var
  Format: TEncodingFormat;
begin
  Format := TEncodingFormat(AParam.encoding);
  if (Format <> efSteim1) and (Format <> efSteim2) then
    raise Exception.Create('This encoding format is not surpported. ' +
      AParam.encoding.ToString);

  result := TSteimDecoder.Create(TSteimType(Format),
    TByteOrder(AParam.byteorder));
end;

function TMSeedFile._ExtractRawData(AStream: TStream; APeriod: TPeriod)
  : TRawData;
var
  Peeks: TPeeks;
  MyElem: Integer;
  FixedHeader: TFixedHeader;
  SteimDecoder: TSteimDecoder;
  DataRecord: TDataRecord;
  I: Integer;
begin
  result := TRawData.Create;

  AStream.Position := 0;
  while AStream.Position < AStream.Size do
  begin
    FixedHeader := TFixedHeader.Create(AStream);
    FixedHeader.Validate;

    if not FixedHeader.IsSteimEncoding then
      raise Exception.Create('not a steim format.');

    SetLength(DataRecord, FixedHeader.FrameCount);
    for I := Low(DataRecord) to High(DataRecord) do
      AStream.Read(DataRecord[I], SizeOf(TDataFrame));

    SteimDecoder := SteimDecoderFactory(FixedHeader.Blkt1000);
    try
      Peeks := SteimDecoder.DecodeData(DataRecord);
      result.Capacity := result.Capacity + Length(Peeks);

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
  Stream: TStreamWriter;
begin
  TFile.Create(APath).Free;
  Stream := TFile.AppendText(APath);
  try
    for I := 0 to RawData.Count - 1 do
      Stream.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz',
        RawData.Items[I].DateTime) + ',' + RawData.Items[I].Peek.ToString);
  finally
    FreeAndNil(Stream);
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
  TBytesStream(Stream).SaveToFile(ChangeFileExt(APath, ''));
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

  Peeks: TPeeks;
  I: Integer;
begin
  SteimType := TSteimType(AFixedHeader.Blkt1000.encoding);

  Stream := TFile.Create(APath);
  try
    SteimEncoder := TSteimEncoder.Create(SteimType,
      TByteOrder(AFixedHeader.Blkt1000.byteorder), AFixedHeader.FrameCount);
    try
      Index := 0;
      Peeks := ARawData.Peeks;
      while Index < ARawData.Count do
      begin
        AFixedHeader.Header.start_time.DateTime :=
          ARawData.Items[Index].DateTime;

        DataRecord := SteimEncoder.EncodeData(Peeks, Index);
        SampleNum := TMSeedCommon.GetRecordCount(DataRecord, SteimType);
        Index := Index + SampleNum;

        AFixedHeader.Header.numsamples := Rev2Bytes(SampleNum);
        AFixedHeader.WriteTo(Stream);

        for I := Low(DataRecord) to High(DataRecord) do
          Stream.Write(DataRecord[I], SizeOf(TDataFrame));
      end;
    finally
      SteimEncoder.Free;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TMSeedFile.AddStream(AStream: TStream);
begin
  if not Assigned(AStream) then
    Exit;

  try
    _AddStream(AStream);
  except
    on E: Exception do
      raise Exception.Create('Can not add stream. ' + E.Message);
  end;
end;

procedure TMSeedFile.AsciiToMSeed(ASource: String; AHeader: TMSeedHeader;
  AType: TSteimType);

  function ReadFile(AFile: String): TRawData;
  var
    MyRecord: String;
    DateTime: TDateTime;
    Stream: TStreamReader;
    Splited: TArray<String>;
  begin
    result := TRawData.Create;

    Stream := TFile.OpenText(AFile);
    try
      while not Stream.EndOfStream do
      begin
        MyRecord := Stream.ReadLine;
        if MyRecord = '' then
          break;

        Splited := MyRecord.Split([',']);
        DateTime := StrToDateTime(Splited[0]);
        result.Add(TPeekValue.Create(DateTime, Splited[1].ToInteger));
      end;
    finally
      Stream.Free;
    end;
  end;

var
  RawData: TRawData;
  FileName: String;
begin
  RawData := ReadFile(ASource);
  try
    FileName := ExtractFilePath(ASource) + AHeader.ChannelCode + '.mseed';
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

  FixedHeader := TFixedHeader.Create(Stream);
  if not FixedHeader.IsSteimEncoding then
  begin
    SaveToBinaryFile(APath, ACode);
    Exit;
  end;

  RawData := ExtractRawData(ACode, APeriod);
  try
    FixedHeader.Blkt1000.encoding := Byte(AType);
    FixedHeader.Blkt1000.byteorder := Byte(boBig);
    SaveToMSeed(APath, RawData, FixedHeader)
  finally
    RawData.Free;
  end;
end;

function TMSeedFile._GetStartTime(AStream: TStream): TDateTime;
var
  FixedHeader: TFixedHeader;
begin
  AStream.Position := 0;
  FixedHeader := TFixedHeader.Create(AStream);
  FixedHeader.Validate;
  result := FixedHeader.Header.start_time.DateTime;
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
      raise Exception.Create('Extract rawData error. ' + E.Message);
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

procedure TMSeedFile._AddStream(var Stream: TStream);
begin
  Stream.Position := 0;
  while Stream.Position < Stream.Size do
  begin
    SaveEachChannel(Stream);
  end;
end;

function TMSeedFile.GetChannelList: TArray<String>;
begin
  result := FContainer.Keys.ToArray;
end;

function TMSeedFile.GetStartTime(ACode: string): TDateTime;
begin
  if not FContainer.ContainsKey(ACode) then
    raise Exception.Create('Channel code error. ' + ACode);

  try
    result := _GetStartTime(FContainer.Items[ACode]);
  except
    on E: Exception do
      raise Exception.Create('GetStartTime error. ' + E.Message);
  end;
end;

procedure TMSeedFile.RevemoveChannel(AChannel: String);
begin
  FContainer.Items[AChannel].Free;
  FContainer.Remove(AChannel);
end;

end.
