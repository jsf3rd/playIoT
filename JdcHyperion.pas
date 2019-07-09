// *******************************************************
//
// Micron Optics x55 Protocol Hyperion Library
//
// Copyright(c) 2019 playIoT.
//
// jsf3rd@playiot.biz
//
// http://www.micronoptics.com/product/optical-sensing-instrument-si255/#tab-software
//
// *******************************************************

unit JdcHyperion;

interface

uses System.Classes, System.SysUtils, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, JdcGlobal, JdcHyperion.Common, IdGlobal, Winapi.windows, System.JSON, REST.JSON,
  JdcGlobal.ClassHelper, IdUDPBase, IdUDPServer, IdSocketHandle;

type
  TOnUdpPeaks = procedure(AValue: TArray<Double>) of object;

  THyperion = class
  private
    FComm: TIdTcpClient;
    FPeakStreamComm: TIdTcpClient;
    FUdpPeakServer: TIdUDPServer;
    FConnInfo: TConnInfo;

    FBuffer: TIdBytes;
    FData: TArray<Double>;
    FNumPeaks: Integer;

    FLastResponse: TResponse;
    FOnLog: TLogProc;

    FOnUdpPeaks: TOnUdpPeaks;
    procedure CreateComm;
    procedure CreatePeakStreamComm;
    procedure CreateUdpPeakServer;

    function read_response(AComm: TIdTcpClient; printLog: boolean = true): TResponse;
    procedure write_command(ACmd: string; AParam: string; requestOptions: UInt8);
    function execute_command(ACmd: string; AParam: string = '';
      requestOptions: UInt8 = REQUEST_OPT_NONE): TResponse;

    procedure set_peak_stream_divider(streamingDivider: Integer);

    function GetPeaks(printLog: boolean): TPeaks;

    procedure OnUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes;
      ABinding: TIdSocketHandle);

  public
    constructor Create(AConnInfo: TConnInfo; AOnLog: TLogProc);
    destructor Destroy; override;

    function get_serial_number: string;
    function get_peaks: TPeaks;

    procedure enable_peak_streaming(streamingDivider: Integer = 1);
    function stream_peaks: TPeaks;
    procedure disable_peak_streaming;

    function enable_udp_peak_datagrams(AValue: TConnInfo;
      streamingDivider: Integer = 1): TPeaks;
    procedure disable_udp_peak_datagrams;

    function get_laser_scan_speed: Integer;
    function get_available_laser_scan_speeds: TArray<Integer>;
    procedure set_laser_scan_speed(sacnSpeed: Integer);

    function Connected: boolean;

    property OnLog: TLogProc read FOnLog write FOnLog;
    property OnUdpPeaks: TOnUdpPeaks read FOnUdpPeaks write FOnUdpPeaks;
  end;

implementation

{ THyperion }

function THyperion.Connected: boolean;
begin
  Result := FComm.Connected;
end;

constructor THyperion.Create(AConnInfo: TConnInfo; AOnLog: TLogProc);
begin
  FConnInfo := AConnInfo;
  FPeakStreamComm := nil;
  FUdpPeakServer := nil;
  FOnLog := AOnLog;
  FNumPeaks := 0;

  CreateComm;
  try
    FComm.Connect;
    OnLog(msInfo, 'Hyperion', 'Comm.Connected');
  except
    on E: Exception do
      OnLog(msError, 'Comm', Format('%s,E=%s', [AConnInfo.ToString, E.Message]));
  end;
end;

procedure THyperion.CreateComm;
begin
  FComm := TIdTcpClient.Create(nil);
  FComm.Host := FConnInfo.StringValue;
  FComm.Port := FConnInfo.IntegerValue;
  FComm.ConnectTimeout := 3000;
  FComm.ReadTimeout := 3000;
end;

procedure THyperion.CreatePeakStreamComm;
begin
  FPeakStreamComm := TIdTcpClient.Create(nil);
  FPeakStreamComm.Host := FConnInfo.StringValue;
  FPeakStreamComm.Port := H_PEAK_STREAM_PORT;
  FComm.ConnectTimeout := 3000;
  FComm.ReadTimeout := 3000;
end;

procedure THyperion.CreateUdpPeakServer;
begin
  FUdpPeakServer := TIdUDPServer.Create(nil);
  FUdpPeakServer.DefaultPort := H_PEAK_STREAM_PORT;
  FUdpPeakServer.OnUDPRead := OnUDPRead;
end;

destructor THyperion.Destroy;
begin
  if Assigned(FPeakStreamComm) then
    disable_peak_streaming;

  // if Assigned(FUdpPeakComm) then

  if FComm.Connected then
  begin
    FComm.Disconnect;
    OnLog(msInfo, 'Hyperion', 'Comm.Disconnected');
    FreeAndNil(FComm);
  end;

  inherited;
end;

procedure THyperion.disable_peak_streaming;
begin
  FLastResponse := execute_command('#DisablePeakDataStreaming');

  FPeakStreamComm.Disconnect;
  FreeAndNil(FPeakStreamComm);
  OnLog(msInfo, 'Hyperion', 'PeakStreamComm.Disconnected')
end;

procedure THyperion.disable_udp_peak_datagrams;
begin
  if FComm.Tag = 0 then
    FLastResponse := execute_command('#DisableUdpPeakDatagrams');

  FUdpPeakServer.Active := false;
  FreeAndNil(FUdpPeakServer);
  FNumPeaks := 0;
  OnLog(msInfo, 'Hyperion', 'UdpPeakServer.Close')
end;

procedure THyperion.enable_peak_streaming(streamingDivider: Integer);
begin
  set_peak_stream_divider(streamingDivider);
  FLastResponse := execute_command('#EnablePeakDataStreaming');

  if not Assigned(FPeakStreamComm) then
    CreatePeakStreamComm;

  try
    FPeakStreamComm.Connect;
    OnLog(msInfo, 'Hyperion', 'PeakStreamComm.Connected');
  except
    on E: Exception do
      OnLog(msError, 'PeakStreamComm', Format('%s,E=%s', [FConnInfo.ToString, E.Message]));
  end;
end;

function THyperion.enable_udp_peak_datagrams(AValue: TConnInfo;
  streamingDivider: Integer): TPeaks;
begin
  set_peak_stream_divider(streamingDivider);

  Result := get_peaks;
  FNumPeaks := Result.numPeaks;
  FComm.Tag := 0;

  FLastResponse := execute_command('#EnableUdpPeakDatagrams',
    Format('%s %d', [AValue.StringValue, AValue.IntegerValue]));

  if not Assigned(FUdpPeakServer) then
    CreateUdpPeakServer;

  SetLength(FBuffer, 0);
  try
    FUdpPeakServer.Active := true;
    OnLog(msInfo, 'Hyperion', 'UdpPeakServer.Open');
  except
    on E: Exception do
      OnLog(msError, 'UdpPeakServer', Format('Server=%s,E=%s', [AValue.ToString, E.Message]));
  end;
end;

function THyperion.execute_command(ACmd, AParam: string; requestOptions: UInt8): TResponse;
begin
  write_command(ACmd, AParam, requestOptions);
  Result := read_response(FComm);
end;

function THyperion.GetPeaks(printLog: boolean): TPeaks;
var
  Header: TPeaksHeader;
  Size: UInt32;
  buff: TIdBytes;
  I: Integer;

  JSONObject: TJSONObject;
begin
  Size := FLastResponse.contentLength;
  if Size < SizeOf(TPeaksHeader) then
  begin
    OnLog(msError, 'Peaks', Format('Wrong Size,expected %d but %d',
      [SizeOf(TPeaksHeader), Size]));
    Exit;
  end;

  CopyMemory(@Header, @FLastResponse.content[0], SizeOf(TPeaksHeader));
  if printLog then
    OnLog(msDebug, 'PeaksHeader', TJson.RecordToJsonString(Header));

  Size := Size - SizeOf(TPeaksHeader);
  SetLength(buff, Size);
  CopyTIdBytes(FLastResponse.content, SizeOf(TPeaksHeader), buff, 0, Size);
  Result := TPeaks.Create(Header, buff);

  if printLog then
  begin
    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('DateTime', Result.timeStamp.ToISO8601);
    for I := Low(Result.startInds) to High(Result.startInds) do
    begin
      JSONObject.AddPair(Format('CH%0.2d-SN01', [I]), Result.values[Result.startInds[I]]);
    end;
    OnLog(msDebug, 'Peaks', JSONObject.ToString);
    JSONObject.Free;
  end;
end;

function THyperion.get_available_laser_scan_speeds: TArray<Integer>;
var
  num: Integer;
begin
  FLastResponse := execute_command('#GetAvailableLaserScanSpeeds');
  num := FLastResponse.contentLength div SizeOf(Integer);
  SetLength(Result, num);
  CopyMemory(@Result[0], @FLastResponse.content[0], FLastResponse.contentLength);
end;

function THyperion.get_laser_scan_speed: Integer;
begin
  FLastResponse := execute_command('#GetLaserScanSpeed');
  Result := BytesToInt32(FLastResponse.content);
  OnLog(msDebug, 'ScanSpeed', Result.ToString);
end;

function THyperion.get_peaks: TPeaks;
begin
  FLastResponse := execute_command('#GetPeaks', '', REQUEST_OPT_SUPPRESS_MSG);
  Result := GetPeaks(true);
end;

function THyperion.get_serial_number: string;
begin
  FLastResponse := execute_command('#GetSerialNumber');
  Result := BytesToString(FLastResponse.content, IndyTextEncoding_UTF8);
  OnLog(msDebug, 'SerialNumber', Result);
end;

procedure THyperion.OnUDPRead(AThread: TIdUDPListenerThread; const AData: TIdBytes;
  ABinding: TIdSocketHandle);
var
  rlt: TArray<Double>;
  Count, Size: Integer;
  I: Integer;
begin
  AppendBytes(FBuffer, AData);

  if Length(FBuffer) < (FNumPeaks * SizeOf(Double) / 2) then
    Exit;

  Count := Length(FBuffer) div SizeOf(Double);
  Size := Count * SizeOf(Double);

  SetLength(rlt, Count);
  CopyMemory(@rlt[0], @FBuffer[0], Size);

  for I := Low(rlt) to High(rlt) do
  begin
    if rlt[I] = 0 then
    begin
      if Length(FData) > 0 then
      begin
        if Assigned(OnUdpPeaks) then
          OnUdpPeaks(FData);
      end
      else
        Continue;
    end
    else
      FData := concat(FData, [rlt[I]]);
  end;

  RemoveBytes(FBuffer, Size);
end;

function THyperion.read_response(AComm: TIdTcpClient; printLog: boolean): TResponse;
var
  Header: TReadHeader;
  buff: TIdBytes;
begin
  try
    SetLength(buff, 0);
    AComm.IOHandler.ReadBytes(buff, SizeOf(TReadHeader));
    if printLog then
      OnLog(msDebug, 'RECV', IdBytesToHex(buff));

    CopyMemory(@Header, @buff[0], SizeOf(TReadHeader));
    if printLog then
      OnLog(msDebug, 'ReadHeader', TJson.RecordToJsonString(Header));

    Result.contentLength := Header.contentLength;
    Result.messageLength := Header.messageLength;

    if Header.messageLength > 0 then
    begin
      Result.msg := AComm.IOHandler.ReadString(Header.messageLength, IndyTextEncoding_UTF8);
      OnLog(msDebug, 'Message', Result.msg);
    end
    else
      Result.msg := '';

    SetLength(Result.content, 0);
    AComm.IOHandler.ReadBytes(Result.content, Header.contentLength);
    if printLog then
      OnLog(msDebug, 'Content', IdBytesToHex(Result.content));

    if Header.status <> H_SUCCESS then
      OnLog(msWarning, 'readResponse', Format('Status=%d,Msg=%s',
        [Header.status, Result.msg]));
  except
    on E: Exception do
      OnLog(msError, 'readResponse', Format('Port=%d,E=%s', [AComm.BoundPort, E.Message]));
  end;
end;

procedure THyperion.set_laser_scan_speed(sacnSpeed: Integer);
begin

end;

procedure THyperion.set_peak_stream_divider(streamingDivider: Integer);
begin
  FLastResponse := execute_command('#SetPeakDataStreamingDivider', streamingDivider.ToString);
end;

function THyperion.stream_peaks: TPeaks;
begin
  FLastResponse := read_response(FPeakStreamComm, false);
  Result := GetPeaks(false);
end;

procedure THyperion.write_command(ACmd, AParam: string; requestOptions: UInt8);
var
  Header: TWriteHeader;
  cmd, param: TIdBytes;
  buff: TIdBytes;
begin
  try
    cmd := ToBytes(ACmd, IndyTextEncoding_UTF8);
    param := ToBytes(AParam, IndyTextEncoding_UTF8);

    Header.requestOption := requestOptions;
    Header.reserved := 0;
    Header.commandSize := Length(cmd);
    Header.argSize := Length(param);

    SetLength(buff, SizeOf(TWriteHeader));
    CopyMemory(@buff[0], @Header, SizeOf(TWriteHeader));
    FComm.IOHandler.Write(buff);
    OnLog(msDebug, 'SEND', IdBytesToHex(buff));
    OnLog(msDebug, 'WriteHeader', TJson.RecordToJsonString(Header));

    FComm.IOHandler.Write(cmd);
    OnLog(msDebug, 'SEND', IdBytesToHex(cmd));
    OnLog(msDebug, 'Command', ACmd);

    FComm.IOHandler.Write(param);
    if AParam <> '' then
    begin
      OnLog(msDebug, 'SEND', IdBytesToHex(param));
      OnLog(msDebug, 'Param', AParam);
    end;
  except
    on E: Exception do
      OnLog(msError, 'writeCommand', Format('ACmd=%s,AParam=%s,E=%s',
        [ACmd, AParam, E.Message]));
  end;
end;

end.
