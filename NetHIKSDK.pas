unit NetHIKSDK;

interface

uses Winapi.Windows, System.SysUtils, JdcNetSDK, Vcl.Controls, JdcLogging, JdcGlobal, System.IOUtils;

const
  NETHIKSDK_DLL = 'HCNetSDK.dll';
  SERIALNO_LEN = 48;
  STREAM_ID_LEN = 32;
  NET_DVR_DEV_ADDRESS_MAX_LEN = 129;
  NET_DVR_LOGIN_USERNAME_MAX_LEN = 64;
  NET_DVR_LOGIN_PASSWD_MAX_LEN = 64;

  PICTURE_QUALITY_BEST = 0;
  PICTURE_QUALITY_BETTER = 1;
  PICTURE_QUALITY_AVERAGE = 2;

  PICTURE_SIZE_CIF = 0;
  PICTURE_SIZE_QCIF = 1;
  PICTURE_SIZE_D1 = 2;
  PICTURE_SIZE_UXGA = 3; // 1600x1200
  PICTURE_SIZE_SVGA = 4; // 800x600
  PICTURE_SIZE_HD720P = 5; // 1280x720
  PICTURE_SIZE_VGA = 6;
  PICTURE_SIZE_XVGA = 7;
  PICTURE_SIZE_HD900P = 8;
  PICTURE_SIZE_HD1080 = 9;
  PICTURE_SIZE_2560_1920 = 10;
  PICTURE_SIZE_1600_304 = 11;
  PICTURE_SIZE_2048_1536 = 12;
  PICTURE_SIZE_2448_2048 = 13;
  PICTURE_SIZE_2448_1200 = 14;
  PICTURE_SIZE_2448_800 = 15;
  PICTURE_SIZE_XGA = 16; // 1024*768
  PICTURE_SIZE_SXGA = 17; // 1280*1024
  PICTURE_SIZE_WD1 = 18; // 960*576/960*480
  PICTURE_SIZE_1080I = 19; // 1920*1080

type
  NET_DVR_USER_LOGIN_INFO = record
    sDeviceAddress: array [0 .. NET_DVR_DEV_ADDRESS_MAX_LEN - 1] of AnsiChar;
    byUseTransport: Byte;
    wPort: WORD;
    sUserName: array [0 .. NET_DVR_LOGIN_USERNAME_MAX_LEN - 1] of AnsiChar;
    sPassword: array [0 .. NET_DVR_LOGIN_PASSWD_MAX_LEN - 1] of AnsiChar;
    cbLoginResult: Pointer;
    pUser: Pointer;
    bUseAsynLogin: Boolean;
    byProxyType: Byte;
    byUseUTCTime: Byte;
    byLoginMode: Byte;
    // 0-Private 1-ISAPI 2-adapt
    byHttps: Byte; // 0-tcp,1-tls 2-adapt
    iProxyID: LONG;
    byVerifyMode: Byte;
    byRes3: array [0 .. 118] of Byte;
  end;

  TSourceDataCallBack = procedure(lRealHandle: Int32; dwDataType: DWORD; var pBuffer: Byte; dwBufSize: DWORD;
    pUser: IntPtr)stdcall;

  NET_DVR_DEVICEINFO_V30 = record
    sSerialNumber: array [0 .. SERIALNO_LEN - 1] of Byte;
    byAlarmInPortNum: Byte;
    byAlarmOutPortNum: Byte;
    byDiskNum: Byte;
    byDVRType: Byte;
    byChanNum: Byte;
    byStartChan: Byte;
    byAudioChanNum: Byte;
    byIPChanNum: Byte;
    byZeroChanNum: Byte;
    byMainProto: Byte;
    bySubProto: Byte;
    bySupport: Byte;
    bySupport1: Byte;
    bySupport2: Byte;
    wDevType: WORD;
    bySupport3: Byte;
    byMultiStreamProto: Byte;
    byStartDChan: Byte;
    byStartDTalkChan: Byte;
    byHighDChanNum: Byte;
    bySupport4: Byte;
    byLanguageType: Byte;

    byVoiceInChanNum: Byte;
    byStartVoiceInChanNo: Byte;
    bySupport5: Byte;
    bySupport6: Byte;
    byMirrorChanNum: Byte;
    wStartMirrorChanNo: Byte;
    bySupport7: Byte;
    byRes2: Byte;
  end;

  NET_DVR_DEVICEINFO_V40 = record
    struDeviceV30: NET_DVR_DEVICEINFO_V30;

    bySupportLock: Byte;
    // the device support lock function,this byte assigned by SDK.when bySupportLock is 1,dwSurplusLockTime and byRetryLoginTime is valid
    byRetryLoginTime: Byte; // retry login times
    byPasswordLevel: Byte; // PasswordLevel,0-invalid,1-default password,2-valid password,3-risk password,
    // 4- the administrator creates an ordinary user to set the password for him/her, and the ordinary user shall be prompted to "please modify the initial login password" after correctly logging into the device. In the case of no modification, the user will be reminded every time he/she logs in;
    // 5- when the password of an ordinary user is modified by the administrator, the ordinary user needs to be prompted "please reset the login password" after correctly logging into the device again. If the password is not modified, the user will be reminded of each login;
    // 6- the administrator creates an installer/operator user and sets password for him/her,  then need prompt "please change initial password" after the user login in the device. If the password is not modified, it can't operate other actions except for changing password
    byProxyType: Byte; // Proxy Type,0-not use proxy, 1-use socks5 proxy, 2-use EHome proxy
    dwSurplusLockTime: DWORD; // surplus locked time
    byCharEncodeType: Byte;
    // character encode type,1-GB2312,2-GBK,3-BIG5,4-Shift_JIS,5-EUC-KR,6-UTF-8,7-21:ISO8859-1---15,22-Hebrew
    bySupportDev5: Byte;
    // Support v50 version of the device parameters, device name and device type name length is extended to 64 bytes
    bySupport: Byte; // capability set extension, bit and result: 0- no support, 1- support
    // bySupport & 0x1:0 - Reserved
    // bySupport & 0x2:0 - does not support changes to report 1- support change escalation
    byLoginMode: Byte; // loginmodel 0-Private 1-ISAPI
    dwOEMCode: DWORD;
    iResidualValidity: Integer;
    byResidualValidity: Byte;
    bySingleStartDTalkChan: Byte;
    bySingleDTalkChanNums: Byte;
    byPassWordResetLevel: Byte;
    bySupportStreamEncrypt: Byte;
    byMarketType: Byte;
    byTLSCap: Byte;
    byRes2: array [0 .. 236] of Byte;
  end;

  NET_DVR_PREVIEWINFO = record
    lChannel: Int32;

    // Stream type 0-main stream,1-sub stream,2-third stream,3-forth stream,4-fifth stream,5-sixth stream,7-seventh stream,8-eighth stream,9-ninth stream,10-tenth stream
    dwStreamType: UINT;
    dwLinkMode: UINT;
    // Protocol type: 0-TCP, 1-UDP, 2-Muticast, 3-RTP,4-RTP/RTSP, 5-RSTP/HTTP
    hPlayWnd: IntPtr;
    bBlocked: BOOL; // If data stream requesting process is blocked or not: 0-no, 1-yes
    bPassbackRecord: BOOL; // 0- not enable  ,1 enable
    byPreviewMode: Byte; // Preview mode 0-normal preview,2-delay preview
    byStreamID: array [0 .. STREAM_ID_LEN - 1] of Byte;
    byProtoType: Byte; // 0-private,1-RTSP,2-SRTP
    byRes1: array [0 .. 1] of Byte;
    dwDisplayBufNum: UINT;
    // soft player display buffer size(number of frames), range:1-50, default:1
    byRes: array [0 .. 215] of Byte;
  end;

  NET_DVR_JPEGPARA = packed record
    { 0-CIF,           1-QCIF,           2-D1,         3-UXGA(1600x1200), 4-SVGA(800x600),5-HD720p(1280x720),
      6-VGA,           7-XVGA,           8-HD900p,     9-HD1080,     10-2560*1920,
      11-1600*304,     12-2048*1536,     13-2448*2048,  14-2448*1200, 15-2448*800,
      16-XGA(1024*768), 17-SXGA(1280*1024),18-WD1(960*576/960*480),      19-1080i }
    wPicSize: WORD;
    wPicQuality: WORD; // 0 -  the best,  1 -  better,  2 -  average;
  end;

  DRAWFUN = procedure(lRealHandle: Longint; hDc: IntPtr; dwUser: UINT); stdcall;

type
  TNET_DVR_Login_V30 = function(sDVRIP: PAnsiChar; wDVRPort: Int32; sUserName: PAnsiChar;
    sPassword: PAnsiChar; var lpDeviceInfo: NET_DVR_DEVICEINFO_V30): Integer; stdcall;
  TNET_DVR_Login_V40 = function(pLoginInfo: NET_DVR_USER_LOGIN_INFO; var lpDeviceInfo: NET_DVR_DEVICEINFO_V40)
    : Integer; stdcall;
  TNET_DVR_Logout = function(iUserID: Longint): BOOL; stdcall;
  TNET_DVR_GetLastError = function(): Integer; stdcall;
  TNET_DVR_GetErrorMsg = function(var ErrorNo: Longint): PAnsiChar; stdcall;
  TNET_DVR_Init = function(): Boolean; stdcall;
  TNET_DVR_Cleanup = function(): Boolean; stdcall;
  TNET_DVR_RealPlay_V40 = function(iUserID: Longint; var lpPreviewInfo: NET_DVR_PREVIEWINFO;
    fRealDataCallBack_V30: TSourceDataCallBack; pUser: IntPtr): Integer; stdcall;
  TNET_DVR_StopRealPlay = function(iRealHandle: Longint): BOOL; stdcall;
  TNET_DVR_RigisterDrawFun = function(lRealHandle: Longint; fDrawFun: DRAWFUN; dwUser: UINT): BOOL; stdcall;
  TNET_DVR_GetPlayBackPlayerIndex = function(lRealHandle: Longint): Integer; stdcall;
  TNET_DVR_SetCapturePictureMode = function(dwCaptureMode: DWORD): BOOL; stdcall;
  TNET_DVR_CapturePicture = function(lRealHandle: Longint; sPicFileName: PAnsiChar): BOOL; stdcall;
  TNET_DVR_CaptureJPEGPicture = function(lUserID: Longint; lChannel: Longint;
    var lpJpegPara: NET_DVR_JPEGPARA; sPicFileName: PAnsiChar): BOOL; stdcall;
  TNET_DVR_MakeKeyFrame = function(lUserID: Longint; lChannel: DWORD): BOOL; stdcall;
  TNET_DVR_SaveRealData = function(lRealHandle: Longint; sFileName: PAnsiChar): BOOL; stdcall;
  TNET_DVR_StopSaveRealData = function(lRealHandle: Longint): BOOL; stdcall;

  TPlayerNetHIK = class(TPlayerAbstract)
  protected
    function _StopRealPlay: Boolean; override;
    function _Logout: Boolean; override;
    function _RealPlay: IntPtr; override;
  public
    function Login: Boolean; override;
    function Played: Boolean; override;
    function Logined: Boolean; override;
    function CapturePicture(const szFileName: String): Boolean; override;
    function GetLastError: String; override;
  end;

  TNetHIK = class(TNetAbstract)
  protected
    procedure LoadDLL(const APath: string); override;
    function InitLib: Boolean; override;
    function CreatePlayer(const AID: string): TPlayerAbstract; override;
  public
    function GetLastError: string; override;
    function Cleanup: Boolean; override;
  end;

implementation

uses IdGlobal;

var
  FNET_DVR_Login_V30: TNET_DVR_Login_V30;
  FNET_DVR_Login_V40: TNET_DVR_Login_V40;
  FNET_DVR_Logout: TNET_DVR_Logout;
  FNET_DVR_GetLastError: TNET_DVR_GetLastError;
  FNET_DVR_GetErrorMsg: TNET_DVR_GetErrorMsg;
  FNET_DVR_Init: TNET_DVR_Init;
  FNET_DVR_Cleanup: TNET_DVR_Cleanup;
  FNET_DVR_RealPlay_V40: TNET_DVR_RealPlay_V40;
  FNET_DVR_StopRealPlay: TNET_DVR_StopRealPlay;
  FNET_DVR_RigisterDrawFun: TNET_DVR_RigisterDrawFun;
  FNET_DVR_GetPlayBackPlayerIndex: TNET_DVR_GetPlayBackPlayerIndex;
  FNET_DVR_SetCapturePictureMode: TNET_DVR_SetCapturePictureMode;
  FNET_DVR_CapturePicture: TNET_DVR_CapturePicture;
  FNET_DVR_CaptureJPEGPicture: TNET_DVR_CaptureJPEGPicture;
  FNET_DVR_MakeKeyFrame: TNET_DVR_MakeKeyFrame;
  FNET_DVR_SaveRealData: TNET_DVR_SaveRealData;
  FNET_DVR_StopSaveRealData: TNET_DVR_StopSaveRealData;

procedure StrToCharArray(ADest: Pointer; const AValue: AnsiString);
begin
  CopyMemory(ADest, @AValue[1], Length(AValue));
end;

procedure OnSourceDataCallBack(lRealHandle: Int32; dwDataType: DWORD; var pBuffer: Byte; dwBufSize: DWORD;
  pUser: IntPtr)stdcall;
var
  Player: TPlayerNetHIK;
begin
  Player := Pointer(pUser);
  Player.Tick := GetTickCount;
end;

{ TPlayerNetHIK }

function TPlayerNetHIK.CapturePicture(const szFileName: String): Boolean;
var
  Name: AnsiString;
  param: NET_DVR_JPEGPARA;
begin
  param.wPicSize := 0;
  param.wPicQuality := PICTURE_QUALITY_AVERAGE;

  Name := AnsiString(szFileName);
  result := FNET_DVR_CaptureJPEGPicture(FUserID, FChannel, param, PAnsiChar(Name));
end;

function TPlayerNetHIK.GetLastError: String;
var
  Code: Int32;
begin
  if not Assigned(FNET_DVR_GetLastError) then
    Exit('Unload DLL');

  Code := FNET_DVR_GetLastError;
  result := String(FNET_DVR_GetErrorMsg(Code));
end;

function TPlayerNetHIK.Login: Boolean;
var
  LLoginInfo: NET_DVR_USER_LOGIN_INFO;
  LDeviceInfoV40: NET_DVR_DEVICEINFO_V40;
  ChCount: IntPtr;
begin
  FLoggingIn := True;
  try
    TLogging.Obj.ApplicationMessage(msDebug, 'NH_TryLogin', 'ID=%s,TryCount=%d', [FID, FRetryCount]);

    ZeroMemory(@LDeviceInfoV40, SizeOf(LDeviceInfoV40));
    ZeroMemory(@LLoginInfo, SizeOf(LLoginInfo));
    LLoginInfo.wPort := FCCTVInfo.Port;
    StrToCharArray(@LLoginInfo.sDeviceAddress[0], FCCTVInfo.IP);
    StrToCharArray(@LLoginInfo.sUserName[0], FCCTVInfo.ID);
    StrToCharArray(@LLoginInfo.sPassword[0], FCCTVInfo.Password);
    FUserID := FNET_DVR_Login_V40(LLoginInfo, LDeviceInfoV40);
  finally
    FLoggingIn := False;
  end;

  result := Self.Logined;
  if result then
  begin
    FLoggingIn := False;
    FRetryCount := 0;
    TLogging.Obj.ApplicationMessage(msInfo, 'NH_LoginSuccess', 'ID=%s,UserID=%d', [FID, FUserID]);
    ChCount := LDeviceInfoV40.struDeviceV30.byChanNum;
    TLogging.Obj.ApplicationMessage(msInfo, 'ND_ChannelList', 'ID=%s,Count=%d', [FID, ChCount]);
  end
  else
  begin
    FRetryCount := FRetryCount + 1;

    if Assigned(OnException) then
      OnException(Self, GetLastError);
  end;
end;

function TPlayerNetHIK.Logined: Boolean;
begin
  result := FUserID > -1;
end;

function TPlayerNetHIK.Played: Boolean;
begin
  result := FRealHandle > -1;
end;

function TPlayerNetHIK._Logout: Boolean;
begin
  result := FNET_DVR_Logout(FUserID);
end;

function TPlayerNetHIK._RealPlay: IntPtr;
var
  LPreviewInfo: NET_DVR_PREVIEWINFO;
begin
  ZeroMemory(@LPreviewInfo, SizeOf(LPreviewInfo));
  LPreviewInfo.hPlayWnd := FPlayControl.Handle;
  LPreviewInfo.lChannel := FChannel;
  LPreviewInfo.dwStreamType := FStreamType;
  LPreviewInfo.dwLinkMode := 0;
  LPreviewInfo.bBlocked := True;
  LPreviewInfo.dwDisplayBufNum := 1;
  LPreviewInfo.byProtoType := 0;
  LPreviewInfo.byPreviewMode := 0;
  result := FNET_DVR_RealPlay_V40(FUserID, LPreviewInfo, OnSourceDataCallBack, IntPtr(Self));
end;

function TPlayerNetHIK._StopRealPlay: Boolean;
begin
  result := FNET_DVR_StopRealPlay(FRealHandle);
end;

{ TNetHIK }

function TNetHIK.Cleanup: Boolean;
var
  MyPlayer: TPlayerAbstract;
begin
  if not FInited then
    Exit(False);

  for MyPlayer in FPlayerDic.Values do
  begin
    MyPlayer.StopRealPlay;
    MyPlayer.Logout;
  end;

  result := FNET_DVR_Cleanup;
  if result then
  begin
    FInited := False;
    TLogging.Obj.ApplicationMessage(msDebug, 'NH_Cleanup')
  end
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'NH_Cleanup', GetLastError)
end;

function TNetHIK.CreatePlayer(const AID: string): TPlayerAbstract;
begin
  result := TPlayerNetHIK.Create(AID);
end;

function TNetHIK.GetLastError: string;
var
  Code: Int32;
begin
  if FDLLHandle = 0 then
    Exit('Unload DLL');

  Code := FNET_DVR_GetLastError;
  result := format('%s(%d)', [FNET_DVR_GetErrorMsg(Code), Code]);
end;

function TNetHIK.InitLib: Boolean;
begin
  result := FNET_DVR_Init;
end;

procedure TNetHIK.LoadDLL(const APath: string);

  function GetModuleSymbol(const SymbolName: string): Pointer;
  begin
    result := GetProcAddress(FDLLHandle, PWideChar(SymbolName));
    if result = nil then
      raise Exception.Create('Invalid HCNetSDK.dll version')
  end;

begin
  SetCurrentDir(APath);
  FDLLHandle := LoadLibrary(NETHIKSDK_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  @FNET_DVR_Login_V30 := GetModuleSymbol('NET_DVR_Login_V30');
  @FNET_DVR_Login_V40 := GetModuleSymbol('NET_DVR_Login_V40');
  @FNET_DVR_Logout := GetModuleSymbol('NET_DVR_Logout');
  @FNET_DVR_GetLastError := GetModuleSymbol('NET_DVR_GetLastError');
  @FNET_DVR_GetErrorMsg := GetModuleSymbol('NET_DVR_GetErrorMsg');
  @FNET_DVR_Init := GetModuleSymbol('NET_DVR_Init');
  @FNET_DVR_Cleanup := GetModuleSymbol('NET_DVR_Cleanup');
  @FNET_DVR_RealPlay_V40 := GetModuleSymbol('NET_DVR_RealPlay_V40');
  @FNET_DVR_StopRealPlay := GetModuleSymbol('NET_DVR_StopRealPlay');
  @FNET_DVR_RigisterDrawFun := GetModuleSymbol('NET_DVR_RigisterDrawFun');
  @FNET_DVR_GetPlayBackPlayerIndex := GetModuleSymbol('NET_DVR_GetPlayBackPlayerIndex');
  @FNET_DVR_SetCapturePictureMode := GetModuleSymbol('NET_DVR_SetCapturePictureMode');
  @FNET_DVR_CapturePicture := GetModuleSymbol('NET_DVR_CapturePicture');
  @FNET_DVR_CaptureJPEGPicture := GetModuleSymbol('NET_DVR_CaptureJPEGPicture');
  @FNET_DVR_MakeKeyFrame := GetModuleSymbol('NET_DVR_MakeKeyFrame');
  @FNET_DVR_SaveRealData := GetModuleSymbol('NET_DVR_SaveRealData');
  @FNET_DVR_StopSaveRealData := GetModuleSymbol('NET_DVR_StopSaveRealData');

  SetCurrentDir(TPath.GetDocumentsPath);
  TLogging.Obj.ApplicationMessage(msDebug, 'NH_LoadDLL', 'Path=%s,Handle=%d', [APath, FDLLHandle]);
end;

end.
