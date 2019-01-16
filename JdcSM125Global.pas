unit JdcSM125Global;

interface

uses
  Classes, SysUtils, Winapi.Windows, IdGlobal;

const
  COMMAND_GET_DATA = '#GET_DATA';
  COMMAND_PD_GET_PEAKS_AND_LEVELS = '#GET_PEAKS_AND_LEVELS';

  COMMAND_GET_PEAK_THRESHOLD_CH = '#GET_PEAK_THRESHOLD_CH';
  COMMAND_GET_REL_PEAK_THRESHOLD_CH = '#GET_REL_PEAK_THRESHOLD_CH';
  COMMAND_GET_PEAK_WIDTH_CH = '#GET_PEAK_WIDTH_CH';
  COMMAND_GET_PEAK_WIDTH_LEVEL_CH = '#GET_PEAK_WIDTH_LEVEL_CH';

  IDX_GET_DATA = 0;
  IDX_PEAKS_DETECT = 22;
  IDX_GET_PEAKS_AND_LEVELS = 54;

  CHANNEL_NUM = 4;

  SM125_COMM_DLL = 'sm125_Comm_DLL.dll';

type
  TSM125_Connect = function(sIPAddr: PAnsiChar): Integer; stdcall;
  TSM125_Disconnect = function: Integer stdcall;
  TSendCommand = function(const ACommand: PAnsiChar): Integer; stdcall;
  TDataRead = function(var vData: byte; var vSize: Integer): Integer; stdcall;
  TSendCommandPD = function(const ACommand: PAnsiChar): Integer; stdcall;
  TDataReadPD = function(var vData: byte; var vSize: Integer): Integer; stdcall;


  TSensorCount = array [1 .. CHANNEL_NUM] of UInt16;

  TPeaksHeader = packed record
    Timestamp: UInt32;
    MicroSec: UInt32;
    Serial: UInt32;
    SensorCount: TSensorCount;
    TerminalFlag: UInt16;
    MUXState: WORD;
    Reserved: UInt64;
  end;

  TPeaksDatas = array [1 .. CHANNEL_NUM] of array of Int32;
  TPeaksLevels = array [1 .. CHANNEL_NUM] of array of Int16;

  TPeaksData = packed record
    Data: TPeaksDatas;
    Levels: TPeaksLevels;
  end;

  TSM125Global = class
  private
    FErrorMsg: TStringList;
    FCommandList: TStringList;

    constructor Create;
  public
    class function Obj: TSM125Global;
    destructor Destroy; override;

    function GetCommandList: String;

    property ErrorMsg: TStringList read FErrorMsg;
    property CommandList: TStringList read FCommandList;
  end;

procedure ReadBytes(var AData: TIdBytes; ADest: Pointer; ALength: Integer);
procedure ReadDatas(var AData: TIdBytes; var APeaksData: TPeaksData;
  ASensorCount: TSensorCount);
procedure ReadLevels(var AData: TIdBytes; var APeaksData: TPeaksData;
  ASensorCount: TSensorCount);

var
  FObj: TSM125Global;

implementation

{ TSM125Global }

procedure ReadBytes(var AData: TIdBytes; ADest: Pointer; ALength: Integer);
begin
  CopyMemory(ADest, AData, ALength);
  RemoveBytes(AData, ALength, 0);
end;

procedure ReadLevels(var AData: TIdBytes; var APeaksData: TPeaksData;
  ASensorCount: TSensorCount);
var
  Levels: Smallint;
  i: Integer;
  j: Integer;
begin
  for i := Low(ASensorCount) to high(ASensorCount) do
  begin
    SetLength(APeaksData.Levels[i], ASensorCount[i]);

    if ASensorCount[i] = 0 then
      Continue;

    for j := 0 to ASensorCount[i] - 1 do
    begin
      ReadBytes(AData, @Levels, Sizeof(Levels));
      APeaksData.Levels[i][j] := Levels;
    end;
  end;
end;

procedure ReadDatas(var AData: TIdBytes; var APeaksData: TPeaksData;
  ASensorCount: TSensorCount);
var
  j: Integer;
  Data: Integer;
  i: Integer;
begin
  for i := Low(ASensorCount) to high(ASensorCount) do
  begin
    SetLength(APeaksData.Data[i], ASensorCount[i]);

    if ASensorCount[i] = 0 then
      Continue;

    for j := 0 to ASensorCount[i] - 1 do
    begin
      ReadBytes(AData, @Data, Sizeof(Data));
      APeaksData.Data[i][j] := Data;
    end;
  end;
end;

constructor TSM125Global.Create;
begin
  FErrorMsg := TStringList.Create;

  FErrorMsg.Values['-9998'] := 'Receive timeout on socket not set.';
  FErrorMsg.Values['-9997'] := 'Send timeout on socket not set.';
  FErrorMsg.Values['-9999'] := 'Connection has been closed.';
  FErrorMsg.Values['-9996'] := 'Number of bytes returned is too large, >131170 bytes.';
  FErrorMsg.Values['-9991'] := 'Number of bytes returned is <0.';
  FErrorMsg.Values['-9990'] := 'Number of bytes returned = 0.';
  FErrorMsg.Values['-9994'] := 'Unable to allocate memory.';
  FErrorMsg.Values['-9995'] := 'Unable to allocate memory.';
  FErrorMsg.Values['-9900'] := 'Connection has been closed.';
  FErrorMsg.Values['10038)'] := 'sm125 already disconnected. Socket operation on non-socket.';
  FErrorMsg.Values['10013'] := 'Permission denied.';
  FErrorMsg.Values['10048'] := 'Address already in use.';
  FErrorMsg.Values['10049'] := 'Cannot assign requested address.';
  FErrorMsg.Values['10047'] := 'Address family not supported by protocol family.';
  FErrorMsg.Values['10037'] := 'Operation already in progress.';
  FErrorMsg.Values['10053'] := 'Software caused connection abort.';
  FErrorMsg.Values['10061'] := 'Connection refused.';
  FErrorMsg.Values['10054'] := 'Connection reset by peer.';
  FErrorMsg.Values['10039'] := 'Destination address required.';
  FErrorMsg.Values['10014'] := 'Bad address.';
  FErrorMsg.Values['10064'] := 'Host is down.';
  FErrorMsg.Values['10065'] := 'No route to host.';
  FErrorMsg.Values['10036'] := 'Operation now in progress.';
  FErrorMsg.Values['10004'] := 'Interrupted function call.';
  FErrorMsg.Values['10022'] := 'Invalid argument.';
  FErrorMsg.Values['10056'] := 'Socket is already connected.';
  FErrorMsg.Values['10024'] := 'Too many open files.';
  FErrorMsg.Values['10040'] := 'Message too long.';
  FErrorMsg.Values['10050'] := 'Network is down.';
  FErrorMsg.Values['10052'] := 'Network dropped connection on reset.';
  FErrorMsg.Values['10051'] := 'Network is unreachable.';
  FErrorMsg.Values['10055'] := 'No buffer space available.';
  FErrorMsg.Values['10042'] := 'Bad protocol option. An unknown,';
  FErrorMsg.Values['10057'] := 'Socket is not connected.';
  FErrorMsg.Values['10038'] := 'Socket operation on non-socket.';
  FErrorMsg.Values['10045'] := 'Operation not supported.';
  FErrorMsg.Values['10046'] := 'Protocol family not supported.';
  FErrorMsg.Values['10067'] := 'Too many processes.';
  FErrorMsg.Values['10043'] := 'Protocol not supported.';
  FErrorMsg.Values['10041'] := 'Protocol wrong type for socket.';
  FErrorMsg.Values['10058'] := 'Cannot send after socket shutdown.';
  FErrorMsg.Values['10044'] := 'Socket type not supported.';
  FErrorMsg.Values['10060'] := 'Connection timed out.';
  FErrorMsg.Values['10109'] := 'Class type not found.';
  FErrorMsg.Values['10035'] := 'Resource temporarily unavailable.';
  FErrorMsg.Values['11001'] := 'Host not found.';
  FErrorMsg.Values['10093'] := 'Successful WSAStartup not yet performed. Please Wait.';
  FErrorMsg.Values['11004'] := 'Valid name, no data record of requested type.';
  FErrorMsg.Values['11003'] := 'This is a non-recoverable error.';
  FErrorMsg.Values['10091'] := 'Network subsystem is unavailable.';
  FErrorMsg.Values['11002'] := 'Non-authoritative host not found.';
  FErrorMsg.Values['10092'] := 'WINSOCK.DLL version out of range.';
  FErrorMsg.Values['10094'] := 'Graceful shutdown in progress.';

  FCommandList := TStringList.Create;

  FCommandList.Add('#GET_DATA'); // 0

  FCommandList.Add('#IDN?');
  FCommandList.Add('#GET_SYSTEM_IMAGE_ID');
  FCommandList.Add('#GET_PRODUCT_SN');
  FCommandList.Add('#SET_IP_ADDRESS');
  FCommandList.Add('#SET_IP_NETMASK');
  FCommandList.Add('#GET_IP_ADDRESS');
  FCommandList.Add('#GET_IP_NETMASK');
  FCommandList.Add('#SET_WLAN_IP_ADDRESS');
  FCommandList.Add('#SET_WLAN_IP_NETMASK');
  FCommandList.Add('#GET_WLAN_IP_ADDRESS');
  FCommandList.Add('#GET_WLAN_IP_NETMASK');
  FCommandList.Add('#GET_DUT1_STATE');
  FCommandList.Add('#GET_DUT2_STATE');
  FCommandList.Add('#GET_DUT3_STATE');
  FCommandList.Add('#GET_DUT4_STATE');
  FCommandList.Add('#SET_DUT1_STATE');
  FCommandList.Add('#SET_DUT2_STATE');
  FCommandList.Add('#SET_DUT3_STATE');
  FCommandList.Add('#SET_DUT4_STATE');

  FCommandList.Add('#GET_SYSTEM_DATE');
  FCommandList.Add('#SET_SYSTEM_DATE');

  FCommandList.Add('#SET_PEAK_THRESHOLD_CH1'); // 22
  FCommandList.Add('#SET_PEAK_THRESHOLD_CH2');
  FCommandList.Add('#SET_PEAK_THRESHOLD_CH3');
  FCommandList.Add('#SET_PEAK_THRESHOLD_CH4');
  FCommandList.Add('#GET_PEAK_THRESHOLD_CH1');
  FCommandList.Add('#GET_PEAK_THRESHOLD_CH2');
  FCommandList.Add('#GET_PEAK_THRESHOLD_CH3');
  FCommandList.Add('#GET_PEAK_THRESHOLD_CH4');
  FCommandList.Add('#SET_REL_PEAK_THRESHOLD_CH1');
  FCommandList.Add('#SET_REL_PEAK_THRESHOLD_CH2');
  FCommandList.Add('#SET_REL_PEAK_THRESHOLD_CH3');
  FCommandList.Add('#SET_REL_PEAK_THRESHOLD_CH4');
  FCommandList.Add('#GET_REL_PEAK_THRESHOLD_CH1');
  FCommandList.Add('#GET_REL_PEAK_THRESHOLD_CH2');
  FCommandList.Add('#GET_REL_PEAK_THRESHOLD_CH3');
  FCommandList.Add('#GET_REL_PEAK_THRESHOLD_CH4');
  FCommandList.Add('#SET_PEAK_WIDTH_CH1');
  FCommandList.Add('#SET_PEAK_WIDTH_CH2');
  FCommandList.Add('#SET_PEAK_WIDTH_CH3');
  FCommandList.Add('#SET_PEAK_WIDTH_CH4');
  FCommandList.Add('#GET_PEAK_WIDTH_CH1');
  FCommandList.Add('#GET_PEAK_WIDTH_CH2');
  FCommandList.Add('#GET_PEAK_WIDTH_CH3');
  FCommandList.Add('#GET_PEAK_WIDTH_CH4');
  FCommandList.Add('#SET_PEAK_WIDTH_LEVEL_CH1');
  FCommandList.Add('#SET_PEAK_WIDTH_LEVEL_CH2');
  FCommandList.Add('#SET_PEAK_WIDTH_LEVEL_CH3');
  FCommandList.Add('#SET_PEAK_WIDTH_LEVEL_CH4');
  FCommandList.Add('#GET_PEAK_WIDTH_LEVEL_CH1');
  FCommandList.Add('#GET_PEAK_WIDTH_LEVEL_CH2');
  FCommandList.Add('#GET_PEAK_WIDTH_LEVEL_CH3');
  FCommandList.Add('#GET_PEAK_WIDTH_LEVEL_CH4');

  FCommandList.Add('#GET_PEAKS_AND_LEVELS'); // 54

end;

destructor TSM125Global.Destroy;
begin
  FreeAndNil(FErrorMsg);

  FreeAndNil(FCommandList);
  inherited;
end;

function TSM125Global.GetCommandList: String;
begin
  result := FCommandList.Text;
end;

class function TSM125Global.Obj: TSM125Global;
begin
  if FObj = nil then
    FObj := TSM125Global.Create;
  result := FObj;
end;

end.
