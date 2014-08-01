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

    function ReadHeader(AStream: TStream): TMSeedHeader;
    function ReadBlockette1000(AStream: TStream): TBlockette1000;
    function ReadBlockette1001(AStream: TStream): TBlockette1001;
    function ReadDataFrame(AStream: TStream): TDataFrame;
  private
    procedure _AddFile(AFile: String);
    procedure SaveEachChannel(AStream: TStream);

    function _ExtractRawData(AStream: TStream; APeriod: TPeriod): TRawData;
    function ExtractRawData(ACode: String; APeriod: TPeriod): TRawData;

    procedure _ExtractToASCii(AFolder, ACode: String; APeriod: TPeriod);
    procedure SaveToASCiiFile(AFolder, ACode: String; RawData: TRawData);

    procedure _ExtractToMSeed(AFolder, ACode: String; APeriod: TPeriod);
  public
    constructor Create(AFile: String = '');
    destructor Destroy; override;

    procedure AddFile(AFile: String);

    function GetChannelList: TArray<String>;

    procedure ExtractToASCii(AFolder, ACode: String); overload;
    procedure ExtractToASCii(AFolder, ACode: String;
      ABegin, AEnd: TDateTime); overload;

    procedure ExtractToMSeed(AFolder, ACode: String); overload;
    procedure ExtractToMSeed(AFolder, ACode: String;
      ABegin, AEnd: TDateTime); overload;
  end;

implementation

{ TMSeedFile }

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

function TMSeedFile.ReadHeader(AStream: TStream): TMSeedHeader;
begin
  AStream.Read(result, SizeOf(result));
end;

function TMSeedFile.ReadBlockette1000(AStream: TStream): TBlockette1000;
begin
  AStream.Read(result, SizeOf(result));

  if result.blockette_code <> BLOCKETTE_CODE_1000 then
    raise TBlocketteTypeException.Create('Blockette 1000 Code Error, ' +
      IntToStr(Rev2Bytes(result.blockette_code)));
end;

function TMSeedFile.ReadBlockette1001(AStream: TStream): TBlockette1001;
begin
  AStream.Read(result, SizeOf(result));

  if result.blockette_code <> BLOCKETTE_CODE_1001 then
    raise TBlocketteTypeException.Create('Blockette 1001 Code Error, ' +
      IntToStr(Rev2Bytes(result.blockette_code)));
end;

function TMSeedFile.ReadDataFrame(AStream: TStream): TDataFrame;
begin
  AStream.Read(result, SizeOf(result));
end;

procedure TMSeedFile.SaveEachChannel(AStream: TStream);
var
  Header: TMSeedHeader;
  Blkt1000: TBlockette1000;
  Blkt1001: TBlockette1001;

  Buffer: TStream;
  DataFrame: TDataFrame;

  I: Integer;
  Channel: String;
begin
  Header := ReadHeader(AStream);
  Channel := Header.ChannelCode;

  if not FContainer.ContainsKey(Channel) then
    FContainer.Add(Channel, TBytesStream.Create);

  Blkt1000 := ReadBlockette1000(AStream);
  try
    Blkt1001 := ReadBlockette1001(AStream);
  except
    on E: TBlocketteTypeException do
    begin
      AStream.Position := AStream.Position +
        Trunc(power(2, Blkt1000.reclen) - SizeOf(Header) - SizeOf(Blkt1000) -
        SizeOf(Blkt1001));
      exit;
    end;
  end;

  Buffer := FContainer.Items[Channel];
  Buffer.Write(Header, SizeOf(Header));
  Buffer.Write(Blkt1000, SizeOf(Blkt1000));
  Buffer.Write(Blkt1001, SizeOf(Blkt1001));

  for I := 0 to Blkt1001.framecnt - 1 do
  begin
    DataFrame := ReadDataFrame(AStream);
    Buffer.Write(DataFrame, SizeOf(DataFrame));
  end;

end;

procedure TMSeedFile._AddFile(AFile: String);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFile, fmOpenRead);
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
    raise Exception.Create('This format is not surpported. ' +
      AParam.encoding.ToString);

  result := TSteimDecoder.Create(TSteimType(Format), AParam.byteorder);
end;

function TMSeedFile._ExtractRawData(AStream: TStream; APeriod: TPeriod)
  : TRawData;
var
  Header: TMSeedHeader;
  Blkt1000: TBlockette1000;
  Blkt1001: TBlockette1001;

  I: Integer;
  DataFrame: TDataFrame;
  RawData: TArray<Integer>;
  SteimDecoder: TSteimDecoder;
  MyElem: Integer;
begin
  result := TRawData.Create;

  AStream.Position := 0;
  while AStream.Position < AStream.Size do
  begin
    Header := ReadHeader(AStream);
    Blkt1000 := ReadBlockette1000(AStream);
    Blkt1001 := ReadBlockette1001(AStream);

    FDateTime := Header.start_time.DateTime;

    SteimDecoder := SteimDecoderFactory(Blkt1000);
    try
      for I := 0 to Blkt1001.framecnt - 1 do
      begin
        DataFrame := ReadDataFrame(AStream);
        RawData := SteimDecoder.DecodeData(DataFrame);
        for MyElem in RawData do
        begin
          if APeriod.InRange(FDateTime) then
            result.Add(TRecord.Create(FDateTime, MyElem));

          FDateTime := IncMilliSecond(FDateTime, Header.Interval);
        end;
      end;
    finally
      SteimDecoder.Free;
    end;
  end;
end;

procedure TMSeedFile._ExtractToASCii(AFolder, ACode: String; APeriod: TPeriod);
var
  RawData: TRawData;
begin
  RawData := ExtractRawData(ACode, APeriod);
  try
    SaveToASCiiFile(AFolder, ACode, RawData);
  finally
    RawData.Free;
  end;
end;

procedure TMSeedFile.SaveToASCiiFile(AFolder, ACode: String; RawData: TRawData);
var
  StringList: TStringList;
  MyElem: TRecord;
begin
  StringList := TStringList.Create;
  try
    for MyElem in RawData do
      StringList.Add(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', MyElem.Key) + ' '
        + MyElem.Value.ToString);
    StringList.SaveToFile(AFolder + '\' + ACode + '_' +
      FormatDateTime('YYYYMMDD', now) + '.txt');
  finally
    StringList.Free;
  end;
end;

procedure TMSeedFile._ExtractToMSeed(AFolder, ACode: String; APeriod: TPeriod);
var
  RawData: TRawData;
begin

  RawData := ExtractRawData(ACode, APeriod);
  try
    //

  finally
    RawData.Free;
  end;
end;

procedure TMSeedFile.AddFile(AFile: String);
begin
  if not TFile.Exists(AFile) then
    raise Exception.Create('File not exist. ' + AFile);

  try
    _AddFile(AFile);
  except
    on E: Exception do
      raise Exception.Create('Can not Add File. ' + E.Message);
  end;
end;

function TMSeedFile.ExtractRawData(ACode: String; APeriod: TPeriod): TRawData;
begin
  if not FContainer.ContainsKey(ACode) then
    raise Exception.Create('Channel Code Error. ' + ACode);

  try
    result := _ExtractRawData(FContainer.Items[ACode], APeriod);
  except
    on E: Exception do
      raise Exception.Create('Extract RawData Error.' + E.Message);
  end;
end;

procedure TMSeedFile.ExtractToASCii(AFolder, ACode: String;
  ABegin, AEnd: TDateTime);
begin
  try
    _ExtractToASCii(AFolder, ACode, TPeriod.Create(ABegin, AEnd));
  except
    on E: Exception do
      raise Exception.Create('Extract To ASCii Error. ' + ACode + ', ' +
        E.Message);
  end;
end;

procedure TMSeedFile.ExtractToMSeed(AFolder, ACode: String);
begin
  ExtractToMSeed(AFolder, ACode, MinDateTime, MaxDateTime);
end;

procedure TMSeedFile.ExtractToASCii(AFolder, ACode: String);
begin
  ExtractToASCii(AFolder, ACode, MinDateTime, MaxDateTime);
end;

procedure TMSeedFile.ExtractToMSeed(AFolder, ACode: String;
  ABegin, AEnd: TDateTime);
begin
  try
    _ExtractToMSeed(AFolder, ACode, TPeriod.Create(ABegin, AEnd));
  except
    on E: Exception do
      raise Exception.Create('Extract To MSeed Error. ' + ACode + ', ' +
        E.Message);
  end;
end;

function TMSeedFile.GetChannelList: TArray<String>;
begin
  result := FContainer.Keys.ToArray;
end;

end.
