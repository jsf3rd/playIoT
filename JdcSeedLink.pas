unit JdcSeedLink;

interface

uses System.Classes, System.SysUtils, IdTCPClient, JdcMSeed.Common,
  System.DateUtils, System.Generics.Collections, WinApi.Windows,
  IdExceptionCore, JdcGlobal;

type
  TOnReceiveDataEvent = procedure(ACode: string; AStartTime: TDateTime;
    const AData: TPeeks) of object;
  TOnReceiveMSeedEvent = procedure(ACode: string; AStartTime: TDateTime;
    AStream: TStream) of object;

  TSeedLinkChannel = record
    NetworkCode: string;
    StationCode: string;
    ChannelCode: string;
  end;

  TJdcSeedLink = class
  private
    FIdTcpClient: TIdTCPClient;
    FThread: TThread;

    FOnReceiveDataEvent: TOnReceiveDataEvent;
    FOnLogEvent: TOnLogEvent;
    FOnReceiveMSeed: TOnReceiveMSeedEvent;
    FOnErrorEvent: TOnErrorEvent;

    FTimeOutCount: Integer;

    function GetCodeKey(AHeader: TMSeedHeader): string;

    procedure SendCommand(Value: string);
    procedure RecvString;
    procedure RecvData;
    procedure ProcessNewData(Stream: TMemoryStream);
  public
    constructor Create(ATcpClient: TIdTCPClient);
    destructor Destroy; override;

    procedure SendCAT;
    procedure SendHello;
    procedure AddStation(Channel: TSeedLinkChannel);
    procedure SendEnd;

    function MSeedToRawData(FixedHeader: TFixedHeader; AStream: TStream)
      : TList<Integer>;

    property OnReceiveData: TOnReceiveDataEvent read FOnReceiveDataEvent
      write FOnReceiveDataEvent;
    property OnReceiveMSeed: TOnReceiveMSeedEvent read FOnReceiveMSeed
      write FOnReceiveMSeed;
    property OnLog: TOnLogEvent read FOnLogEvent write FOnLogEvent;
    property OnError: TOnErrorEvent read FOnErrorEvent write FOnErrorEvent;
  end;

implementation

uses IdGlobal, JdcMSeed.Steim;

const
  COMMAND_CAT = 'CAT';
  COMMAND_HELLO = 'HELLO';
  COMMAND_STATION = 'STATION';
  COMMAND_SELECT = 'SELECT';
  COMMAND_DATA = 'DATA';
  COMMAND_END = 'END';
  COMMAND_BYE = 'BYE';

  { TSeedLink }

procedure TJdcSeedLink.AddStation(Channel: TSeedLinkChannel);
begin
  SendCommand(COMMAND_STATION + '  ' + Channel.StationCode + ' ' +
    Channel.NetworkCode);
  RecvString;
  SendCommand(COMMAND_SELECT + ' ' + Channel.ChannelCode);
  RecvString;
  SendCommand(COMMAND_DATA);
  RecvString;
end;

constructor TJdcSeedLink.Create(ATcpClient: TIdTCPClient);
begin
  FIdTcpClient := ATcpClient;
  FThread := nil;
  FTimeOutCount := 0;
end;

destructor TJdcSeedLink.Destroy;
begin
  try
    SendCommand(COMMAND_BYE);
  except
    on E: Exception do
      raise Exception.Create(Self.ClassName + ' Destroy Error,' + E.Message);
  end;

  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;

  inherited;
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

function TJdcSeedLink.MSeedToRawData(FixedHeader: TFixedHeader;
  AStream: TStream): TList<Integer>;
var
  Peeks: TPeeks;
  MyElem: Integer;
  SteimDecoder: TSteimDecoder;
  DataRecord: TArray<TDataFrame>;
begin
  result := TList<Integer>.Create;

  while AStream.Position < AStream.Size do
  begin
    SetLength(DataRecord, FixedHeader.Blkt1001.GetFrameCount);
    AStream.Read(DataRecord[0], Length(DataRecord) * SizeOf(TDataFrame));

    SteimDecoder := SteimDecoderFactory(FixedHeader.Blkt1000);
    try
      Peeks := SteimDecoder.DecodeData(DataRecord);
      result.Capacity := result.Capacity + Length(Peeks);

      for MyElem in Peeks do
      begin
        result.Add(MyElem);
      end;
    finally
      SteimDecoder.Free;
    end;
  end;

end;

procedure TJdcSeedLink.ProcessNewData(Stream: TMemoryStream);
var
  FixedHeader: TFixedHeader;
  RawData: TList<Integer>;
begin
  try
    Stream.Position := 0;
    FixedHeader := TFixedHeader.Create(Stream);
    FixedHeader.Validate;

    if Assigned(OnReceiveData) then
    begin
      RawData := MSeedToRawData(FixedHeader, Stream);
      try
        OnReceiveData(GetCodeKey(FixedHeader.Header),
          FixedHeader.Header.start_time.DateTime, RawData.ToArray);
      finally
        FreeAndNil(RawData);
      end;
    end;

    if Assigned(OnReceiveMSeed) then
    begin
      OnReceiveMSeed(GetCodeKey(FixedHeader.Header),
        FixedHeader.Header.start_time.DateTime, Stream);
    end;

  except
    on E: Exception do
    begin
      OnLog(Self, 'Error on ProcessNewData, ' + E.Message);
      raise E;
    end;
  end;
end;

function TJdcSeedLink.GetCodeKey(AHeader: TMSeedHeader): string;
var
  Bytes: TIdBytes;
begin
  SetLength(Bytes, 2);
  CopyMemory(Bytes, @AHeader.network, 2);
  result := BytesToString(Bytes);

  SetLength(Bytes, 5);
  CopyMemory(Bytes, @AHeader.station, 5);
  result := result + BytesToString(Bytes) + '_';

  SetLength(Bytes, 3);
  CopyMemory(Bytes, @AHeader.Channel, 3);
  result := result + BytesToString(Bytes);

  result := result.Replace(' ', '');
end;

procedure TJdcSeedLink.RecvData;
var
  buffer: TIdBytes;
  tmp: string;
  Stream: TMemoryStream;

  SL: TIdBytes;
  Index: Integer;
begin
  SetLength(buffer, 0);
  FIdTcpClient.IOHandler.ReadBytes(buffer, 520);

  tmp := BytesToString(buffer, 0, 8);
  if not tmp.Contains('SL') then
  begin
    SL := ToBytes('SL');
    Index := IdBytesPos(SL, buffer);

    if Index < 0 then
      Exit;

    OnLog(Self, 'SEEDLink Log - ' + BytesToString(buffer, Index, 8));

    RemoveBytes(buffer, Index);
    FIdTcpClient.IOHandler.ReadBytes(buffer, Index);
    Exit;
  end;

  Stream := TMemoryStream.Create;
  try
    WriteTIdBytesToStream(Stream, buffer, 512, 8);
    ProcessNewData(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TJdcSeedLink.RecvString;
var
  Msg: string;
begin
  if not FIdTcpClient.Connected then
    Exit;

  while True do
  begin
    try
      Msg := FIdTcpClient.IOHandler.ReadLn;
      if Msg.IsEmpty then
        Break
      else
        OnLog(Self, 'RECV - ' + Msg);

      if Msg.Equals('OK') then
        Break;
    except
      on E: EIdReadTimeout do
        Break;

      on E: Exception do
      begin
        OnError(Self, 'Error on RecvString', E.Message);
        Break;
      end;
    end;

  end;
end;

procedure TJdcSeedLink.SendCAT;
begin
  SendCommand(COMMAND_CAT);
  RecvString;
end;

procedure TJdcSeedLink.SendCommand(Value: string);
begin
  OnLog(Self, 'SEND - ' + Value);
  FIdTcpClient.IOHandler.WriteLn(Value);
end;

procedure TJdcSeedLink.SendEnd;
begin
  // RecvString;
  SendCommand(COMMAND_END);

  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while not TThread.CurrentThread.CheckTerminated do
      begin
        try
          Sleep(1);
          RecvData;
          FTimeOutCount := 0;
        except
          on E: EIdReadTimeout do
          begin
            Inc(FTimeOutCount);

            if FTimeOutCount > 1 then
              OnError(Self, 'EReadTimeOut on RecvData', E.Message);
          end;
          on E: Exception do
            OnError(Self, 'Error on RecvData', E.Message);
        end;
      end;
      Sleep(1000);
    end);
  FThread.FreeOnTerminate := false;
  FThread.Start;
end;

procedure TJdcSeedLink.SendHello;
begin
  SendCommand(COMMAND_HELLO);
  RecvString;
end;

end.
