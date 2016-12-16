// *******************************************************
//
// playIoT SeedLink Client
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
// *******************************************************

unit JdcSeedLink;

interface

uses System.Classes, System.SysUtils, IdTCPClient, JdcMSeed.Common,
  System.DateUtils, System.Generics.Collections, WinApi.Windows,
  IdExceptionCore, JdcGlobal;

type
  TOnReceiveDataEvent = procedure(ACode: string; AFixedHeader: TFixedHeader;
    const AData: TPeeks) of object;
  TOnReceiveMSeedEvent = procedure(ACode: string; AFixedHeader: TFixedHeader;
    AStream: TStream) of object;

  TSeedLinkChannel = record
    NetworkCode: string;
    StationCode: string;
    ChannelCode: string;
  end;

  TSeedLinkInfo = record
    ConnInfo: TConnInfo;
    ChannelInfo: TSeedLinkChannel;
  end;

  TJdcSeedLink = class
  private
    FIdTcpClient: TIdTCPClient;
    FThread: TThread;

    FOnReceiveDataEvent: TOnReceiveDataEvent;
    FOnLogEvent: TLogProc;
    FOnReceiveMSeed: TOnReceiveMSeedEvent;

    FTimeOutCount: Integer;

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

    property OnReceiveData: TOnReceiveDataEvent read FOnReceiveDataEvent
      write FOnReceiveDataEvent;
    property OnReceiveMSeed: TOnReceiveMSeedEvent read FOnReceiveMSeed
      write FOnReceiveMSeed;
    property OnLog: TLogProc read FOnLogEvent write FOnLogEvent;
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

procedure TJdcSeedLink.ProcessNewData(Stream: TMemoryStream);

  function GetCodeKey(AHeader: TMSeedHeader): string;
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

var
  FixedHeader: TFixedHeader;
  RawData: TPeeks;
  CodeKey: string;
begin
  try
    Stream.Position := 0;
    FixedHeader := TFixedHeader.Create(Stream);
    FixedHeader.Validate;

    CodeKey := GetCodeKey(FixedHeader.Header);

    if Assigned(OnReceiveData) then
    begin
      RawData := TMSeedCommon.MSeedToRawData(FixedHeader, Stream);
      OnReceiveData(CodeKey, FixedHeader, RawData);
    end;

    if Assigned(OnReceiveMSeed) then
    begin
      OnReceiveMSeed(CodeKey, FixedHeader, Stream);
    end;

  except
    on E: Exception do
    begin
      OnLog(mtError, 'ProcessNewData', 'E=' + E.Message);
      raise E;
    end;
  end;
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

    OnLog(mtDebug, 'SEEDLink', BytesToString(buffer, Index, 8));

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
        OnLog(mtDebug, 'RECV', Msg);

      if Msg.Equals('OK') then
        Break;
    except
      on E: EIdReadTimeout do
        Break;

      on E: Exception do
      begin
        OnLog(mtError, 'RecvString', 'E=' + E.Message);
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
  OnLog(mtDebug, 'SEND', Value);
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
              OnLog(mtError, 'ReadTimeOut', 'E=' + E.Message);
          end;
          on E: Exception do
            OnLog(mtError, 'RecvData', 'E=' + E.Message);
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
