unit RawDataFormat;

interface

uses System.Classes, System.SysUtils, IdGlobal, WinApi.Windows,
  System.Threading, JdcGlobal, System.IOUtils, Math, System.DateUtils;

type
  TData = TArray<Double>;
  TDataList = TArray<TData>;

  TDataInfo = record
    DataLength: Cardinal;
    LastSequence: Cardinal;
  end;

  TLoggerInfo = record
    LoggerType: string;
    LoggerNumber: string;
    SampleCount: Integer;
    ChannelCount: Integer;
  public
    constructor Create(ALoggerType, ALoggerNumber: string);
    function CreateRawDataMap: TDataInfo;
    function Name: String;
  end;

  TRawDataHeader = packed record
    Version: Byte;
    LoggerType: array [0 .. 3] of AnsiChar;
    LoggerNumber: array [0 .. 1] of AnsiChar;
    ChannelCount: Byte;
    SampelCount: Byte;
    TimeStamp: TDateTime;
    DataLength: Cardinal;
  public
    constructor Create(ALoggerInfo: TLoggerInfo; ADateTime: TDateTime);
    function ToString: String;
    function LoggerTypeToString: string;
    function LoggerNumberToString: string;
    function LoggerName: String;
  end;

  TRawDataEncoder = class(TObject)
  private
    FHeader: TRawDataHeader;

    FDataStream: TStream;

    procedure SetDateTime(Avalue: TDateTime);
    function GetDateTime: TDateTime;

    function DataToStream(const AData: TDataList): TStream;
  public
    constructor Create(const ALoggerInfo: TLoggerInfo;
      const ADateTime: TDateTime; const AData: TDataList);
    destructor Destroy; override;

    function PacketStream: TStream;
    function DataList: TDataList;

    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property Header: TRawDataHeader read FHeader write FHeader;
  end;

  TRawDataDecoder = class
  private
    FHeader: TRawDataHeader;

    FDataStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    property Header: TRawDataHeader read FHeader write FHeader;

    function GetData: TDataList;
  end;

implementation

uses JdcGlobal.ClassHelper;

function StreamToData(AHeader: TRawDataHeader; AStream: TStream): TDataList;
var
  _Double: Double;
  I, J: Integer;
begin
  AStream.Position := 0;
  SetLength(Result, AHeader.ChannelCount);
  for I := Low(Result) to High(Result) do
    SetLength(Result[I], AHeader.SampelCount);

  for I := Low(Result) to High(Result) do
  begin
    for J := Low(Result[I]) to High(Result[I]) do
    begin
      AStream.ReadData(_Double, SizeOf(_Double));
      Result[I][J] := RoundTo(_Double, -5);
    end;
  end;

end;

{ TRawDataEncoder }

constructor TRawDataEncoder.Create(const ALoggerInfo: TLoggerInfo;
  const ADateTime: TDateTime; const AData: TDataList);
begin
  FHeader := TRawDataHeader.Create(ALoggerInfo, ADateTime);
  FDataStream := DataToStream(AData);
end;

destructor TRawDataEncoder.Destroy;
begin
  FreeAndNil(FDataStream);

  inherited;
end;

function TRawDataEncoder.GetDateTime: TDateTime;
begin
  Result := FHeader.TimeStamp;
end;

function TRawDataEncoder.PacketStream: TStream;
begin
  Result := TMemoryStream.Create;

  if FHeader.DataLength <> FDataStream.Size then
    raise Exception.Create('DataLength Error.' + FDataStream.Size.ToString);

  Result.Write(@FHeader, SizeOf(TRawDataHeader));

  FDataStream.Position := 0;
  Result.CopyFrom(FDataStream, FDataStream.Size);
end;

procedure TRawDataEncoder.SetDateTime(Avalue: TDateTime);
begin
  FHeader.TimeStamp := Avalue;
end;

function TRawDataEncoder.DataList: TDataList;
begin
  Result := StreamToData(FHeader, FDataStream);
end;

function TRawDataEncoder.DataToStream(const AData: TDataList): TStream;
var
  Buffer: TStream;
  I, J: Integer;
begin
  Buffer := TMemoryStream.Create;

  for I := Low(AData) to High(AData) do
  begin
    for J := Low(AData[I]) to High(AData[I]) do
    begin
      Buffer.WriteData(AData[I][J]);
    end;
  end;

  Result := Buffer;
end;

{ TDataFormatDecoder }

constructor TRawDataDecoder.Create(AStream: TStream);
begin
  AStream.Read(FHeader, SizeOf(FHeader));
  FDataStream := TMemoryStream.Create;
  FDataStream.CopyFrom(AStream, FHeader.DataLength);
end;

destructor TRawDataDecoder.Destroy;
begin
  FreeAndNil(FDataStream);

  inherited;
end;

function TRawDataDecoder.GetData: TDataList;
var
  I: Integer;
  J: Integer;
begin
  SetLength(Result, FHeader.ChannelCount);

  FDataStream.Position := 0;
  for I := 0 to FHeader.ChannelCount - 1 do
  begin
    SetLength(Result[I], FHeader.SampelCount);
    for J := 0 to FHeader.SampelCount - 1 do
    begin
      FDataStream.Read(Result[I][J], SizeOf(Double));
    end;
  end;

end;

{ TRawDataFormatHeader }

constructor TRawDataHeader.Create(ALoggerInfo: TLoggerInfo;
  ADateTime: TDateTime);
begin
  Self.Version := 1;
  CopyMemory(@Self.LoggerType, ToBytes(ALoggerInfo.LoggerType,
    IndyTextEncoding_UTF8), SizeOf(Self.LoggerType));
  CopyMemory(@Self.LoggerNumber, ToBytes(ALoggerInfo.LoggerNumber,
    IndyTextEncoding_UTF8), SizeOf(Self.LoggerNumber));
  Self.ChannelCount := ALoggerInfo.ChannelCount;
  Self.SampelCount := ALoggerInfo.SampleCount;
  Self.TimeStamp := ADateTime;
  Self.DataLength := ALoggerInfo.SampleCount * ALoggerInfo.ChannelCount *
    SizeOf(Double);
end;

function TRawDataHeader.LoggerTypeToString: string;
var
  MyChar: AnsiChar;
begin
  Result := '';
  for MyChar in Self.LoggerType do
    Result := Result + WideString(MyChar);
end;

function TRawDataHeader.LoggerName: String;
begin
  Result := LoggerTypeToString + LoggerNumberToString;
end;

function TRawDataHeader.LoggerNumberToString: string;
var
  MyChar: AnsiChar;
begin
  Result := '';
  for MyChar in Self.LoggerNumber do
    Result := Result + WideString(MyChar);
end;

function TRawDataHeader.ToString: String;
begin
  Result := LoggerName + ', ' + Self.TimeStamp.FormatWithoutMSec;
end;

{ TLoggerInfo }

constructor TLoggerInfo.Create(ALoggerType, ALoggerNumber: string);
begin
  Self.LoggerType := ALoggerType;
  Self.LoggerNumber := ALoggerNumber;
  Self.SampleCount := 100;

  Randomize;
  Self.ChannelCount := 100;
end;

function TLoggerInfo.CreateRawDataMap: TDataInfo;
begin
  Result.DataLength := SizeOf(TRawDataHeader) + Self.SampleCount *
    Self.ChannelCount * SizeOf(Double);
  Result.LastSequence := 0;
end;

function TLoggerInfo.Name: String;
begin
  Result := Self.LoggerType + Self.LoggerNumber;
end;

end.
