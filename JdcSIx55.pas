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

unit JdcSIx55;

interface

uses System.Classes, System.SysUtils, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, JdcGlobal, JdcSIx55.Common, IdGlobal, Winapi.windows, System.JSON, REST.JSON,
  JdcGlobal.ClassHelper;

type
  Hyperion = class
  private
    FComm: TIdTcpClient;
    FPeakStreamComm: TIdTcpClient;
    FHost: string;

    FLastResponse: TResponse;
    FOnLog: TLogProc;
    procedure CreateComm;
    procedure CreatePeakStreamComm;

    function read_response(AComm: TIdTcpClient): TResponse;
    procedure write_command(ACmd: string; AParam: string; requestOptions: UInt8);
    function execute_command(ACmd: string; AParam: string = '';
      requestOptions: UInt8 = REQUEST_OPT_NONE): TResponse;

    procedure set_peak_stream_divider(streamingDivider: Integer);

    function GetPeaks(printLog: boolean): TPeaks;
  public
    constructor Create(AHost: string);

    function get_serial_number: string;
    function get_peaks: TPeaks;

    procedure enable_peak_streaming(streamingDivider: Integer = 1);
    function stream_peaks: TPeaks;
    procedure disable_peak_streaming;

    function get_laser_scan_speed: Integer;
    function get_available_laser_scan_speeds: TArray<Integer>;
    procedure set_laser_scan_speed(sacnSpeed: Integer);

    property OnLog: TLogProc read FOnLog write FOnLog;
  end;

implementation

{ Hyperion }

constructor Hyperion.Create(AHost: string);
begin
  FHost := AHost;
  FPeakStreamComm := nil;

  CreateComm;
  try
    FComm.Connect;
    OnLog(msDebug, 'Hyperion', 'Comm.Connected');
  except
    on E: Exception do
      OnLog(msError, 'Comm', Format('Host=%s,E=%s', [AHost, E.Message]));
  end;
end;

procedure Hyperion.CreateComm;
begin
  FComm := TIdTcpClient.Create(nil);
  FComm.Host := FHost;
  FComm.Port := H_CMD_PORT;
end;

procedure Hyperion.CreatePeakStreamComm;
begin
  FPeakStreamComm := TIdTcpClient.Create(nil);
  FPeakStreamComm.Host := FHost;
  FPeakStreamComm.Port := H_PEAK_STREAM_PORT;
end;

procedure Hyperion.disable_peak_streaming;
begin
  FLastResponse := execute_command('#DisablePeakDataStreaming');

  FPeakStreamComm.Disconnect;
  FreeAndNil(FPeakStreamComm);
  OnLog(msDebug, 'Hyperion', 'PeakStreamComm.Disconnected')
end;

procedure Hyperion.enable_peak_streaming(streamingDivider: Integer);
begin
  set_peak_stream_divider(streamingDivider);
  FLastResponse := execute_command('#EnablePeakDataStreaming');

  if not Assigned(FPeakStreamComm) then
    CreatePeakStreamComm;

  try
    FPeakStreamComm.Connect;
    OnLog(msDebug, 'Hyperion', 'PeakStreamComm.Connected');
  except
    on E: Exception do
      OnLog(msError, 'PeakStreamComm', Format('Host=%s,E=%s', [FHost, E.Message]));
  end;

end;

function Hyperion.execute_command(ACmd, AParam: string; requestOptions: UInt8): TResponse;
begin
  write_command(ACmd, AParam, requestOptions);
  result := read_response(FComm);
end;

function Hyperion.GetPeaks(printLog: boolean): TPeaks;
var
  Header: TPeaksHeader;
  Size: UInt8;
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
  result := TPeaks.Create(Header, buff);

  if printLog then
  begin
    JSONObject := TJSONObject.Create;
    JSONObject.AddPair('DateTime', result.timeStamp.ToISO8601);
    for I := Low(result.startInds) to High(result.startInds) do
    begin
      JSONObject.AddPair(Format('CH%0.2d-SN01', [I]), result.peaks[result.startInds[I]]);
    end;
    OnLog(msDebug, 'Peaks', JSONObject.ToString);
    JSONObject.Free;
  end;
end;

function Hyperion.get_available_laser_scan_speeds: TArray<Integer>;
var
  num: Integer;
begin
  FLastResponse := execute_command('#GetAvailableLaserScanSpeeds');
  num := FLastResponse.contentLength div SizeOf(Integer);
  SetLength(result, num);
  CopyMemory(@result[0], @FLastResponse.content[0], FLastResponse.contentLength);
end;

function Hyperion.get_laser_scan_speed: Integer;
begin
  FLastResponse := execute_command('#GetLaserScanSpeed');
  result := BytesToInt32(FLastResponse.content);
  OnLog(msDebug, 'ScanSpeed', result.ToString);
end;

function Hyperion.get_peaks: TPeaks;
begin
  FLastResponse := execute_command('#GetPeaks', '', REQUEST_OPT_SUPPRESS_MSG);
  result := GetPeaks(True);
end;

function Hyperion.get_serial_number: string;
begin
  FLastResponse := execute_command('#GetSerialNumber');
  result := BytesToString(FLastResponse.content, IndyTextEncoding_UTF8);
  OnLog(msDebug, 'SerialNumber', result);
end;

function Hyperion.read_response(AComm: TIdTcpClient): TResponse;
var
  Header: TReadHeader;
  buff: TIdBytes;
begin
  try
    SetLength(buff, 0);
    AComm.IOHandler.ReadBytes(buff, SizeOf(TReadHeader));
    OnLog(msDebug, 'RECV', IdBytesToHex(buff));

    CopyMemory(@Header, @buff[0], SizeOf(TReadHeader));
    OnLog(msDebug, 'ReadHeader', TJson.RecordToJsonString(Header));

    result.contentLength := Header.contentLength;
    result.messageLength := Header.messageLength;

    if Header.messageLength > 0 then
    begin
      // IndyTextEncoding_OSDefault
      result.msg := AComm.IOHandler.ReadString(Header.messageLength, IndyTextEncoding_UTF8);
      OnLog(msDebug, 'Message', result.msg);
    end
    else
      result.msg := '';

    SetLength(result.content, 0);
    AComm.IOHandler.ReadBytes(result.content, Header.contentLength);
    OnLog(msDebug, 'Content', 'Count=' + Header.contentLength.ToString);

    if Header.status <> H_SUCCESS then
      OnLog(msWarning, 'readResponse', Format('Status=%d,Msg=%s',
        [Header.status, result.msg]));
  except
    on E: Exception do
      OnLog(msError, 'readResponse', Format('Port=%d,E=%s', [AComm.BoundPort, E.Message]));
  end;
end;

procedure Hyperion.set_laser_scan_speed(sacnSpeed: Integer);
begin

end;

procedure Hyperion.set_peak_stream_divider(streamingDivider: Integer);
begin
  FLastResponse := execute_command('#SetPeakDataStreamingDivider', streamingDivider.ToString);
end;

function Hyperion.stream_peaks: TPeaks;
begin
  FLastResponse := read_response(FPeakStreamComm);
  result := GetPeaks(False);
end;

procedure Hyperion.write_command(ACmd, AParam: string; requestOptions: UInt8);
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
