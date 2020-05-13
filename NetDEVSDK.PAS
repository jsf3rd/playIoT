unit NetDEVSDK;

interface

uses System.Classes, System.SysUtils, Winapi.Windows, System.IOUtils;

const
  NETDEVSDK_DLL = 'NetDEVSDK.DLL';
  DEFAULT_CHANNEL = 1;
  NETDEV_LEN_64 = 63;

  ERROR_CODE: array [0 .. 157, 0 .. 1] of string = //
    (('0', 'Succeeded'), //
    ('-1', 'Failed'), //
    ('1', 'Common error'), //
    ('2', 'Common error returned by device'), //
    ('3', 'Failed to call system function. See errno'), //
    ('4', 'Null pointer'), //
    ('5', 'Invalid parameter'), //
    ('6', 'Invalid module ID'), //
    ('7', 'Failed to allocate memory'), //
    ('8', 'Not supported by device'), //
    ('9', 'listen  Failed to create socketlisten'), //
    ('10', 'Failed to initialize lock'), //
    ('11', 'Failed to initialize semaphore'), //
    ('12', 'Error occurred during SDK resource allocation'), //
    ('13', 'SDK not initialized'), //
    ('14', 'SDK already initialized'), //
    ('15', 'Data not all sent'), //
    ('16', 'More data required'), //
    ('17', 'Failed to create connection'), //
    ('18', 'Failed to send request message'), //
    ('19', 'Message timeout'), //
    ('20', 'Failed to decode response message'), //
    ('21', 'Socket failed to receive message'), //
    ('22', 'Maximum number reached. The assigned numbers of registration connections and preview connections reached the maximum supported by SDK'),
    ('24', 'Failed to obtain local port number'), //
    ('25', 'Failed to create thread'), //
    ('26', 'Buffer is too small for receiving device data'), //
    ('27', 'Failed to obtain the IP or MACaddress of the local PC'), //
    ('28', 'Resource code not exist'), //
    ('31', 'Incorrect message content'), //
    ('32', 'Failed to obtain capabilities'), //
    ('33', 'User not subscribed to alarms'), //
    ('34', 'User authentication failed'), //
    ('35', 'Failed to bind alarms'), //
    ('36', 'Not enough permission. In Windows, it is normally because the operator is not an administrator.'),
    ('37', 'Manufacturers that are not supported'), //
    ('38', 'Function not supported'), //
    ('39', 'File transmission failed'), //
    ('40', 'Json common error'), //
    ('41', 'No result'), //
    ('42', 'Device type that are not supported'), //
    ('101', 'Incorrect password'), //
    ('102', 'Number of login users reachedthe upper limit'), //
    ('103', 'User not online'), //
    ('104', 'User not online'), //
    ('105', 'User has no rights'), //
    ('106', 'Reached the upper limitno moreusers can be added'), //
    ('107', 'User already exists'), //
    ('108', 'Password changed'), //
    ('109', 'Remote user with weak password'), //
    ('250', 'Playback ended'), //
    ('251', 'Playback controlling module not exist'), //
    ('252', 'Beyond playback capability'), //
    ('253', 'Recording file controlling module not exist'), //
    ('254', 'No recording'), //
    ('255', 'Cannot get the URL for playback'), //
    ('300', 'Failed to set preset'), //
    ('301', 'Failed to query preset'), //
    ('302', 'Failed to query route'), //
    ('303', 'Failed to start route recording'), //
    ('304', 'Failed to end route recording'), //
    ('305', 'Failed to query patrol route'), //
    ('306', 'Failed to set patrol route'), //
    ('307', 'PTZ operation failed'), //
    ('308', 'Preset is being used in patrol route and cannot be deleted'), //
    ('309', 'Discontinuous presets'), //
    ('310', 'Route is in use and cannotbe deleted'), //
    ('311', 'Serial modes do not match'), //
    ('312', 'Route does not exist'), //
    ('313', 'Route points are full'), //
    ('500', 'Device stream full'), //
    ('501', 'Device stream closed'), //
    ('502', 'Device stream does not exist'), //
    ('503', 'Failed to read file (directory) status'), //
    ('504', 'File does not exist'), //
    ('505', 'Failed to create directory'), //
    ('506', 'Subscription is full for current user'), //
    ('507', 'Only admin can upgrade'), //
    ('508', 'Upgrade not started'), //
    ('509', 'Upgrade in process'), //
    ('510', 'Insufficient memory for upgrade'), //
    ('511', 'Error occurred while opening the mirror file during upgrade'), //
    ('512', 'Error occurred while upgrading FLASH'), //
    ('513', 'Cannot load multiple upgrade processes at the same time'), //
    ('514', 'Upgrade timeout'), //
    ('515', 'Invalid configuration file'), //
    ('516', 'Storage resource not allocated'), //
    ('1000', 'Basic decoding error code'), //
    ('1001', 'Basic decoding error code'), //
    ('1002', 'Invalid input parameter'), //
    ('1003', 'Not enough system memory'), //
    ('1004', 'Failed to create SOCKET'), //
    ('1005', 'Failed to receive'), //
    ('1006', 'None received'), //
    ('1007', 'Currently not supported'), //
    ('1008', 'Failed to create the thread'), //
    ('1009', 'Failed to load the dynamiclibrary'), //
    ('1010', 'Failed to get the dynamiclibrary'), //
    ('1011', 'Failed to send'), //
    ('1012', 'No permission to create the file'), //
    ('1013', 'Failed to find the file toread'), //
    ('1014', 'Close log'), //
    ('1257', 'Failed to initialize the player'), //
    ('1258', 'Failed to allocate playingchannel resources'), //
    ('1259', 'Failed to get playing channel resources'), //
    ('1260', 'Cache queue full'), //
    ('1261', 'Cache queue empty'), //
    ('1262', 'Failed to open the file'), //
    ('1263', 'The file is read'), //
    ('1264', 'Disk space full'), //
    ('1265', 'Failed to read'), //
    ('1266', 'The microphone does not exist'), //
    ('1267', 'TS packing not finished'), //
    ('1268', 'Recording saved'), //
    ('1269', 'Resolution changed'), //
    ('1270', 'Video Record'), //
    ('1513', 'Failed to start media streaming'), //
    ('1514', 'Failed to close media streaming'), //
    ('1515', 'Failed to receive data dueto network error'), //
    ('1516', 'Failed to handle media data'), //
    ('1517', 'Playing not started in playing channel'), //
    ('1518', 'Failed to enter media stream data'), //
    ('1519', 'Input data cache full'), //
    ('1520', 'Failed to set media streamdata callback function'), //
    ('1521', 'Error occurred when running voice service'), //
    ('1522', 'Failed to start voice service'), //
    ('1523', 'Failed to close voice service'), //
    ('1524', 'Unknown media stream'), //
    ('1525', 'Packet loss'), //
    ('1526', 'More packets are needed for the packing'), //
    ('1527', 'Failed to create the decoder'), //
    ('1528', 'Failed to decode'), //
    ('1529', 'Not enough data received'), //
    ('1530', 'Display resources full'), //
    ('1531', 'Display resources do not exist'), //
    ('1532', 'Failed to create the resources'), //
    ('1533', 'Audio resources do not exist'), //
    ('1534', 'Decoder requires more data'), //
    ('1535', 'Failed to create encoder'), //
    ('1536', 'Capture resources do not exist'), //
    ('1537', 'Recording already opened'), //
    ('1538', 'Decoding in progress, please wait'), //
    ('1539', 'Too much data, still needpacking'), //
    ('2000', 'Live video service alreadyestablished'), //
    ('2001', 'Media stream not ready'), //
    ('2002', 'Display resource is busy for live video service'), //
    ('2003', 'Control module for live video not exist'), //
    ('2004', 'Live stream resource full'), //
    ('2100', 'Format of captured image not supported'), //
    ('2101', 'Insufficient disk space'), //
    ('2102', 'No decoded image for capture'), //
    ('2103', 'Single capture failed'), //
    ('2200', 'Two-way audio already exists'), //
    ('2201', 'Two-way audio service doesnot exist'), //
    ('2202', 'Invalid two-way audio resource code'), //
    ('2203', 'Audio resource is being used by two-way audio'), //
    ('2204', 'Two-way audio failed'), //
    ('2205', 'No more audio service allowed'));

type
  TNETDEVAlarmInfo = record
    tAlarmTime: Int64;
    dwChannelID: Int32;
    wIndex: UINT16;
    pszName: PAnsiChar;
    dwTotalBandWidth: Int32;
    dwUnusedBandwidth: Int32;
    dwTotalStreamNum: Int32;
    dwFreeStreamNum: Int32;
    byRes: Array [0 .. 10 - 1] of BYTE;
  end;

  TNETDEV_ExceptionCallBack_PF = procedure(plUserID: IntPtr; dwType: Int32; plExpHandle: IntPtr;
    plUserData: IntPtr)stdcall;

  TNetDEVLiveStreamIndex = (NETDEV_LIVE_STREAM_INDEX_MAIN, NETDEV_LIVE_STREAM_INDEX_AUX,
    NETDEV_LIVE_STREAM_INDEX_THIRD, NETDEV_LIVE_STREAM_INDEX_INVALID);
  TNetDEVProtocol = (NETDEV_TRANSPROTOCOL_RTPUDP, NETDEV_TRANSPROTOCOL_RTPTCP);
  TNetDEVPictureFluency = (NETDEV_PICTURE_REAL, NETDEV_PICTURE_FLUENCY);

  TNetDEVPictureFormat = (NETDEV_PICTURE_BMP, NETDEV_PICTURE_JPG, NETDEV_PICTURE_INVALID);

  TNETDEVException = (NETDEV_EXCEPTION_REPORT_VOD_END = 300, NETDEV_EXCEPTION_REPORT_VOD_ABEND = 301,
    NETDEV_EXCEPTION_REPORT_BACKUP_END = 302, NETDEV_EXCEPTION_REPORT_BACKUP_DISC_OUT = 303,
    NETDEV_EXCEPTION_REPORT_BACKUP_DISC_FULL = 304, NETDEV_EXCEPTION_REPORT_BACKUP_ABEND = 305,
    NETDEV_EXCEPTION_EXCHANGE = $8000, NETDEV_EXCEPTION_REPORT_INVALID = $FFFF);

  TOnException = procedure(AType: TNETDEVException) of object;

  TNetDEVDeviceInfo = record
    dwDevType: Int32;
    wAlarmInPortNum: Int16;
    wAlarmOutPortNum: Int16;
    dwChannelNum: Int32;
    byRes: array [0 .. 47] of BYTE;
  end;

  TPreviewInfo = record
    dwChannelID: Int32;
    dwStreamType: Int32;
    dwLinkMode: Int32;
    hPlayWnd: IntPtr;
    dwFluency: Int32;
    byRes: array [0 .. 259] of BYTE;

    constructor Create(AHandle: HWND; AType: TNetDEVLiveStreamIndex = NETDEV_LIVE_STREAM_INDEX_MAIN;
      AChannel: Int32 = DEFAULT_CHANNEL; AMode: TNetDEVProtocol = NETDEV_TRANSPROTOCOL_RTPTCP;
      AFluncy: TNetDEVPictureFluency = NETDEV_PICTURE_REAL);
  end;

  TNETDEVVideoStreamInfo = record
    enStreamType: Int32;
    bEnableFlag: Int32;
    dwHeight: Int32;
    dwWidth: Int32;
    dwFrameRate: Int32;
    dwBitRate: Int32;
    enCodeType: Int32;
    enQuality: Int32;
    dwGop: Int32;
    byRes: Array [0 .. 31] of BYTE;
  end;

  TNETDEVDeviceBasicInfo = record
    szDevModel: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szSerialNum: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szFirmwareVersion: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szMacAddress: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    szDeviceName: Array [0 .. NETDEV_LEN_64] of AnsiChar;
    byRes: Array [0 .. 447] of BYTE;
  end;

  TSourceDataCallBack = procedure(lpRealHandle: IntPtr; var pucBuffer: BYTE; dwBufSize: Int32;
    dwMediaDataType: Int32; lpUserParam: IntPtr)stdcall;
  TExceptionCallBack = procedure(plUserID: IntPtr; dwType: Int32; var stAlarmInfo: TNETDEVAlarmInfo;
    plExpHandle, plUserData: IntPtr)stdcall;

  TNETDEV_Init = function: Boolean stdcall;
  TNETDEV_Login = function(pszDevIP: PAnsiChar; wDevPort: Int16; pszUserName: PAnsiChar;
    pszPassword: PAnsiChar; var pstDevInfo: TNetDEVDeviceInfo): IntPtr stdcall;
  TNETDEV_Logout = function(lpUserID: IntPtr): Boolean stdcall;
  TNETDEV_GetDevConfig = function(lpUserID: IntPtr; dwChannelID: Int32; dwCommand: Int32;
    var lpOutBuffer: TNETDEVVideoStreamInfo; dwOutBufferSize: Int32; var pdwBytesReturned: Int32)
    : Boolean stdcall;
  TNETDEV_RealPlay = function(lpUserID: IntPtr; var pstPreviewInfo: TPreviewInfo;
    cbDataCallBack: TSourceDataCallBack; lpUserData: IntPtr): IntPtr stdcall;
  TNETDEV_StopRealPlay = function(lpRealHandle: IntPtr): Boolean stdcall;
  TNETDEV_CapturePicture = function(lpRealHandle: IntPtr; szFileName: PAnsiChar; dwCaptureMode: Int32)
    : Boolean stdcall;
  TNETDEV_GetLastError = function(): Int32 stdcall;
  TNETDEV_SetExceptionCallBack = function(cbExceptionCallBack: TNETDEV_ExceptionCallBack_PF;
    lpUserData: IntPtr): Boolean stdcall;
  TNETDEV_Cleanup = function(): Boolean stdcall;
  NETDEV_SetLogPath = function(pszLogPath: PAnsiChar): Boolean stdcall;

  TNETDEV_SetPlayDataCallBack = function(lpRealHandle: IntPtr; cbPlayDataCallBack: IntPtr; bContinue: Int32;
    lpUserData: IntPtr): Boolean stdcall;

  TCCTVInfo = record
    Enable: Boolean;
    IP: String;
    Port: Integer;
    ID: String;
    Password: String;
    function Equals(AInfo: TCCTVInfo): Boolean;
  end;

  TNetDEV = class
  private
    FDLLHandle: THandle;

    FInited: Boolean;

    FUserID: IntPtr;
    FRealHandle: IntPtr;

    FNETDEV_StopRealPlay: TNETDEV_StopRealPlay;
    FNETDEV_Init: TNETDEV_Init;
    FNETDEV_SetExceptionCallBack: TNETDEV_SetExceptionCallBack;
    FNETDEV_CapturePicture: TNETDEV_CapturePicture;
    FNETDEV_RealPlay: TNETDEV_RealPlay;
    FNETDEV_SetPlayDataCallBack: TNETDEV_SetPlayDataCallBack;
    FNETDEV_Logout: TNETDEV_Logout;
    FNETDEV_GetDevConfig: TNETDEV_GetDevConfig;
    FNETDEV_Login: TNETDEV_Login;
    FNETDEV_Cleanup: TNETDEV_Cleanup;
    FNETDEV_SetLogPath: NETDEV_SetLogPath;
    FNETDEV_GetLastError: TNETDEV_GetLastError;
    FCCTVInfo: TCCTVInfo;
    FTag: Cardinal;
    FLastCapture: Cardinal;

    procedure FreeDLL;
    procedure LoadDLL(APath: string);
    procedure SetOnException(const Value: TOnException);
  public
    constructor Create(const ALibPath: string; const ALogPath: string);
    destructor Destroy; override;

    function GetLastError: string;

    function Played: Boolean;

    function Init: Boolean;
    function Cleanup: Boolean;
    function Login: Boolean; overload;
    function Login(const pszDevIP: String; const wDevPort: Int16; const pszUserName: String;
      const pszPassword: String): Boolean; overload;
    function Logout: Boolean;
    function GetStreamInfo(const dwChannelID: Int32; var lpOutBuffer: TNETDEVVideoStreamInfo;
      const dwOutBufferSize: Int32; var pdwBytesReturned: Int32): Boolean;
    function RealPlay(APreviewInfo: TPreviewInfo): Boolean;
    function StopRealPlay: Boolean;
    function CapturePicture(const szFileName: String;
      const dwCaptureMode: TNetDEVPictureFormat = NETDEV_PICTURE_JPG): Boolean;
    function SetLogPath(const szPath: string): Boolean;

    property OnException: TOnException write SetOnException;
    property CCTVInfo: TCCTVInfo read FCCTVInfo write FCCTVInfo;
    property Tag: Cardinal read FTag write FTag;
    property LastCapture: Cardinal read FLastCapture write FLastCapture;
  end;

implementation

var
  MyOnException: TOnException;

procedure ExceptionCallBack(plUserID: IntPtr; dwType: Int32; plExpHandle, plUserData: IntPtr)stdcall;
begin
  if Assigned(MyOnException) then
    MyOnException(TNETDEVException(dwType));
end;

{ TNetDEV }

function TNetDEV.CapturePicture(const szFileName: String; const dwCaptureMode: TNetDEVPictureFormat): Boolean;
var
  Name: AnsiString;
begin
  if FRealHandle = 0 then
    raise Exception.Create('StopRealPlay');

  FLastCapture := GetTickCount;
  Name := AnsiString(szFileName);
  result := FNETDEV_CapturePicture(FRealHandle, PAnsiChar(Name), Int32(dwCaptureMode));
end;

function TNetDEV.Cleanup: Boolean;
begin
  if not FInited then
    Exit(False);

  result := FNETDEV_Cleanup;
  FInited := not result;
end;

constructor TNetDEV.Create(const ALibPath: string; const ALogPath: string);
begin
  if not TDirectory.Exists(ALibPath) then
    raise Exception.Create('path not exist. [' + ALibPath + ']');

  MyOnException := nil;
  FInited := False;
  FDLLHandle := 0;
  FUserID := 0;
  FRealHandle := 0;

  FTag := 0;
  FLastCapture := 0;

  LoadDLL(ALibPath);

  SetLogPath(ALogPath);
  Init;
end;

destructor TNetDEV.Destroy;
begin
  try
    StopRealPlay;
    Logout;
    Cleanup;
  except
    on E: Exception do
  end;

  FreeDLL;

  inherited;
end;

procedure TNetDEV.FreeDLL;
begin
  if FDLLHandle <> 0 then
  begin
    try
      FDLLHandle := 0;
    except
      on E: Exception do
    end;
  end;
end;

function TNetDEV.GetStreamInfo(const dwChannelID: Int32; var lpOutBuffer: TNETDEVVideoStreamInfo;
  const dwOutBufferSize: Int32; var pdwBytesReturned: Int32): Boolean;
const
  NETDEV_GET_STREAMCFG = 120;
begin
  if FUserID = 0 then
    Exit(False);

  result := FNETDEV_GetDevConfig(FUserID, dwChannelID, NETDEV_GET_STREAMCFG, lpOutBuffer, dwOutBufferSize,
    pdwBytesReturned);
end;

function TNetDEV.Init: Boolean;
begin
  result := FNETDEV_Init;
  FInited := result;
end;

function TNetDEV.GetLastError: String;
var
  I: Integer;
  Code: Integer;
begin
  if FDLLHandle = 0 then
    Exit('Unload DLL');

  Code := FNETDEV_GetLastError;
  for I := Low(ERROR_CODE) to High(ERROR_CODE) do
  begin
    if ERROR_CODE[I, 0].ToInteger = Code then
      Exit(ERROR_CODE[I, 1]);
  end;
  result := 'Unknown Error';
end;

procedure TNetDEV.LoadDLL(APath: string);
begin
  SetCurrentDir(APath);
  FDLLHandle := LoadLibrary(NETDEVSDK_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  @FNETDEV_Init := GetProcAddress(FDLLHandle, 'NETDEV_Init');
  @FNETDEV_Login := GetProcAddress(FDLLHandle, 'NETDEV_Login');
  @FNETDEV_Logout := GetProcAddress(FDLLHandle, 'NETDEV_Logout');
  @FNETDEV_RealPlay := GetProcAddress(FDLLHandle, 'NETDEV_RealPlay');
  @FNETDEV_GetDevConfig := GetProcAddress(FDLLHandle, 'NETDEV_GetDevConfig');

  @FNETDEV_StopRealPlay := GetProcAddress(FDLLHandle, 'NETDEV_StopRealPlay');
  @FNETDEV_CapturePicture := GetProcAddress(FDLLHandle, 'NETDEV_CapturePicture');
  @FNETDEV_GetLastError := GetProcAddress(FDLLHandle, 'NETDEV_GetLastError');
  @FNETDEV_SetExceptionCallBack := GetProcAddress(FDLLHandle, 'NETDEV_SetExceptionCallBack');
  @FNETDEV_SetPlayDataCallBack := GetProcAddress(FDLLHandle, 'NETDEV_SetPlayDataCallBack');
  @FNETDEV_Cleanup := GetProcAddress(FDLLHandle, 'NETDEV_Cleanup');
  @FNETDEV_SetLogPath := GetProcAddress(FDLLHandle, 'NETDEV_SetLogPath');
  SetCurrentDir(TPath.GetDocumentsPath);
end;

function TNetDEV.Login(const pszDevIP: String; const wDevPort: Int16;
  const pszUserName, pszPassword: String): Boolean;
var
  pstDevInfo: TNetDEVDeviceInfo;
  IP, ID, Pwd: AnsiString;
begin
  if FUserID > 0 then
    Exit(True);

  IP := AnsiString(pszDevIP);
  ID := AnsiString(pszUserName);
  Pwd := AnsiString(pszPassword);
  FUserID := FNETDEV_Login(PAnsiChar(IP), wDevPort, PAnsiChar(ID), PAnsiChar(Pwd), pstDevInfo);
  result := FUserID > 0;
end;

function TNetDEV.Login: Boolean;
begin
  result := Login(FCCTVInfo.IP, FCCTVInfo.Port, FCCTVInfo.ID, FCCTVInfo.Password);
end;

function TNetDEV.Logout: Boolean;
begin
  if FUserID = 0 then
    Exit(False);

  result := FNETDEV_Logout(FUserID);
  if result then
    FUserID := 0;
end;

function TNetDEV.Played: Boolean;
begin
  result := FRealHandle > 0;
end;

function TNetDEV.RealPlay(APreviewInfo: TPreviewInfo): Boolean;
begin
  if FUserID = 0 then
    raise Exception.Create('Logout');

  if FRealHandle > 0 then
    StopRealPlay;

  FRealHandle := FNETDEV_RealPlay(FUserID, APreviewInfo, nil, 0);
  result := FRealHandle > 0;

  if result then
    FNETDEV_SetExceptionCallBack(ExceptionCallBack, 0);
end;

function TNetDEV.SetLogPath(const szPath: string): Boolean;
var
  Path: AnsiString;
begin
  Path := AnsiString(szPath);
  result := FNETDEV_SetLogPath(PAnsiChar(Path));
end;

procedure TNetDEV.SetOnException(const Value: TOnException);
begin
  MyOnException := Value;
end;

function TNetDEV.StopRealPlay: Boolean;
begin
  if FRealHandle = 0 then
    Exit(False);

  result := FNETDEV_StopRealPlay(FRealHandle);
  if result then
  begin
    FRealHandle := 0;
    FNETDEV_SetExceptionCallBack(nil, 0)
  end;

end;

{ TPreviewInfo }

constructor TPreviewInfo.Create(AHandle: HWND; AType: TNetDEVLiveStreamIndex; AChannel: Int32;
  AMode: TNetDEVProtocol; AFluncy: TNetDEVPictureFluency);
begin
  Self.dwChannelID := AChannel;
  Self.dwStreamType := Int32(AType);
  Self.dwLinkMode := Int32(AMode);
  Self.hPlayWnd := AHandle;
  Self.dwFluency := Int32(AFluncy);
end;

{ TCCTVInfo }

function TCCTVInfo.Equals(AInfo: TCCTVInfo): Boolean;
begin
  result := (Self.IP = AInfo.IP) and (Self.Port = AInfo.Port) and (Self.ID = AInfo.ID) and
    (Self.Password = AInfo.Password)
end;

end.
