unit ToolFunctionUnit;

interface

uses
  Windows, SysUtils, Variants, Classes, CameraParamsUnit;

// Declare interface of MV_CAMCTRL_API
function MV_CC_EnumerateTls(): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_GetSDKVersion(): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_EnumDevices(nTLayerType: DWORD; Var pstDevList: MV_CC_DEVICE_INFO_LIST): Integer;
  stdcall; external 'MvCameraControl.dll';
function MV_CC_CreateHandle(Var handle: PPointer; Var pstDevInfo: MV_CC_DEVICE_INFO): Integer;
  stdcall; external 'MvCameraControl.dll';
function MV_CC_CreateHandleWithoutLog(Var handle: PPointer; Var pstDevInfo: MV_CC_DEVICE_INFO)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_OpenDevice(Var handle: Pointer; nAccessMode: DWORD = 1; nSwitchoverKey: Word = 0)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_CloseDevice(Var handle: Pointer): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_DestroyHandle(Var handle: Pointer): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_StartGrabbing(Var handle: Pointer): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_Display(Var handle: Pointer; hWnd: Integer): Integer; stdcall;
  external 'MvCameraControl.dll';
function MV_CC_StopGrabbing(Var handle: Pointer): Integer; stdcall; external 'MvCameraControl.dll';

function MV_CC_GetOneFrameTimeout(Var handle: Pointer; pData: PAnsiChar; nDataSize: Cardinal;
  pFrameInfo: PMV_FRAME_OUT_INFO_EX; nMsec: Integer): Integer; stdcall;
  external 'MvCameraControl.dll';
function MV_CC_SaveImageEx2(Var handle: Pointer; pSaveParam: PMV_SAVE_IMAGE_PARAM_EX): Integer;
  stdcall; external 'MvCameraControl.dll';

function MV_CC_GetIntValue(Var handle: Pointer; const strKey: PAnsiChar; pIntValue: PMVCC_INTVALUE)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_SetIntValue(Var handle: Pointer; const strKey: PAnsiChar; nValue: Cardinal): Integer;
  stdcall; external 'MvCameraControl.dll';
function MV_CC_GetEnumValue(Var handle: Pointer; const strKey: PAnsiChar;
  pEnumValue: PMVCC_ENUMVALUE): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_SetEnumValue(Var handle: Pointer; const strKey: PAnsiChar; nValue: Cardinal)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_GetFloatValue(Var handle: Pointer; const strKey: PAnsiChar; pFloatValue: PSingle)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_SetFloatValue(Var handle: Pointer; const strKey: PAnsiChar; fValue: Single): Integer;
  stdcall; external 'MvCameraControl.dll';
function MV_CC_GetBoolValue(Var handle: Pointer; const strKey: PAnsiChar; pBoolValue: PBool)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_SetBoolValue(Var handle: Pointer; const strKey: PAnsiChar; bBoolValue: Bool)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_GetStringValue(Var handle: Pointer; const strKey: PAnsiChar;
  pStringValue: PMVCC_STRINGVALUE): Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_SetStringValue(Var handle: Pointer; const strKey: PAnsiChar; sValue: PAnsiChar)
  : Integer; stdcall; external 'MvCameraControl.dll';
function MV_CC_SetCommandValue(Var handle: Pointer; const strKey: PAnsiChar): Integer; stdcall;
  external 'MvCameraControl.dll';

function MV_CC_GetOptimalPacketSize(Var handle: Pointer): Integer; stdcall;
  external 'MvCameraControl.dll';

function MV_CC_RotateImage(Var handle: Pointer; pstRotateParam: PMV_CC_ROTATE_IMAGE_PARAM): Integer;
  stdcall; external 'MvCameraControl.dll';
function MV_CC_FlipImage(Var handle: Pointer; pstFlipParam: PMV_CC_FLIP_IMAGE_PARAM): Integer;
  stdcall; external 'MvCameraControl.dll';

// Support common operation
function GigeDeviceInfoToShow(pDeviceInfo: MV_CC_DEVICE_INFO; Var pInfoToShow: string): Integer;
function USB3DeviceInfoToShow(pDeviceInfo: MV_CC_DEVICE_INFO; Var pInfoToShow: string): Integer;

function GetIntValue(hDevHandle: PPointer; const strKey: String; pnIntValue: PCardinal): Integer;
function SetIntValue(hDevHandle: PPointer; const strKey: String; nIntValue: Cardinal): Integer;
function GetEnumValue(hDevHandle: PPointer; const strKey: String; pnEnumValue: PCardinal): Integer;
function SetEnumValue(hDevHandle: PPointer; const strKey: String; nEnumValue: Cardinal): Integer;
function GetFloatValue(hDevHandle: PPointer; Const strKey: String; pfFloatValue: PSingle): Integer;
function SetFloatValue(hDevHandle: PPointer; const strKey: String; fFloatValue: Single): Integer;
function GetBoolValue(hDevHandle: PPointer; const strKey: String; pbBoolValue: PBool): Integer;
function SetBoolValue(hDevHandle: PPointer; const strKey: String; bBoolValue: Bool): Integer;
function GetStringValue(hDevHandle: PPointer; const strKey: String; Var pStrValue: String): Integer;
function SetStringValue(hDevHandle: PPointer; const strKey: String; pStrValue: String): Integer;
function SetCommandValue(hDevHandle: PPointer; const strKey: String): Integer;

// Convert the C-Style char sequences to String
function _FormaCtName(const Format: string): string; cdecl;

// allows us to use "varargs" in Delphi
const
FormatCName:
function(const Format: string): string;
cdecl varargs = _FormaCtName;

implementation

Var
  ShowCalendar: THandle;

  // Parse and display the Gige device information
function GigeDeviceInfoToShow(pDeviceInfo: MV_CC_DEVICE_INFO; Var pInfoToShow: string): Integer;
Var
  nIp1: Integer;
  nIp2: Integer;
  nIp3: Integer;
  nIp4: Integer;
  strUserName: PAnsiChar;

begin
  nIp1 := ((pDeviceInfo.SpecialInfo.stGigEInfo.nCurrentIp and $FF000000) shr 24);
  nIp2 := ((pDeviceInfo.SpecialInfo.stGigEInfo.nCurrentIp and $00FF0000) shr 16);
  nIp3 := ((pDeviceInfo.SpecialInfo.stGigEInfo.nCurrentIp and $0000FF00) shr 8);
  nIp4 := (pDeviceInfo.SpecialInfo.stGigEInfo.nCurrentIp and $000000FF);

  if 0 <> pDeviceInfo.SpecialInfo.stGigEInfo.chUserDefinedName[0] then
  begin
    strUserName := PAnsiChar(@pDeviceInfo.SpecialInfo.stGigEInfo.chUserDefinedName);
  end
  else
  begin
    strUserName := PAnsiChar(@pDeviceInfo.SpecialInfo.stGigEInfo.chManufacturerName);
  end;
  pInfoToShow := 'GigE: ' + strUserName + ' (' + IntToStr(nIp1) + '.' + IntToStr(nIp2) + '.' +
    IntToStr(nIp3) + '.' + IntToStr(nIp4) + ')';
  Result := MV_OK;
end;

// Parse and display the U3V device information
function USB3DeviceInfoToShow(pDeviceInfo: MV_CC_DEVICE_INFO; Var pInfoToShow: string): Integer;
Var
  strUserName: PAnsiChar;
begin
  if 0 <> pDeviceInfo.SpecialInfo.stUsb3VInfo.chUserDefinedName[0] then
  begin
    strUserName := PAnsiChar(@pDeviceInfo.SpecialInfo.stUsb3VInfo.chUserDefinedName);
  end
  else
  begin
    strUserName := PAnsiChar(@pDeviceInfo.SpecialInfo.stUsb3VInfo.chManufacturerName);
  end;
  pInfoToShow := 'UsbV3: ' + strUserName;
  Result := MV_OK;
end;

// Get Integer value
function GetIntValue(hDevHandle: PPointer; const strKey: String; pnIntValue: PCardinal): Integer;
Var
  pAnsiKey: PAnsiChar;
  nIntNode: MVCC_INTVALUE;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) or (Nil = pnIntValue) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  ZeroMemory(@nIntNode, sizeof(MVCC_INTVALUE));
  Result := MV_CC_GetIntValue(hDevHandle^, pAnsiKey, @nIntNode);
  pnIntValue^ := nIntNode.nCurValue;
end;

// Set Integer value
function SetIntValue(hDevHandle: PPointer; const strKey: String; nIntValue: Cardinal): Integer;
Var
  pAnsiKey: PAnsiChar;
begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  Result := MV_CC_SetIntValue(hDevHandle^, pAnsiKey, nIntValue);
end;

// Get Enum value
function GetEnumValue(hDevHandle: PPointer; const strKey: String; pnEnumValue: PCardinal): Integer;
Var
  pAnsiKey: PAnsiChar;
  enEnumNode: MVCC_ENUMVALUE;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) or (Nil = pnEnumValue) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  ZeroMemory(@enEnumNode, sizeof(MVCC_ENUMVALUE));
  Result := MV_CC_GetEnumValue(hDevHandle^, pAnsiKey, @enEnumNode);
  pnEnumValue^ := enEnumNode.nCurValue;
end;

// Set Enum value
function SetEnumValue(hDevHandle: PPointer; const strKey: String; nEnumValue: Cardinal): Integer;
Var
  pAnsiKey: PAnsiChar;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  Result := MV_CC_SetEnumValue(hDevHandle^, pAnsiKey, nEnumValue);
end;

// Get Float value
function GetFloatValue(hDevHandle: PPointer; const strKey: String; pfFloatValue: PSingle): Integer;
Var
  pAnsiKey: PAnsiChar;
  fFloatNode: MVCC_FLOATVALUE;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) or (Nil = pfFloatValue) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  ZeroMemory(@fFloatNode, sizeof(MVCC_FLOATVALUE));
  Result := MV_CC_GetFloatValue(hDevHandle^, pAnsiKey, @fFloatNode);

  pfFloatValue^ := fFloatNode.fCurValue;
end;

// Set Float value
function SetFloatValue(hDevHandle: PPointer; const strKey: String; fFloatValue: Single): Integer;
Var
  pAnsiKey: PAnsiChar;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  Result := MV_CC_SetFloatValue(hDevHandle^, pAnsiKey, fFloatValue);
end;

// Get Boolean value
function GetBoolValue(hDevHandle: PPointer; const strKey: String; pbBoolValue: PBool): Integer;
Var
  pAnsiKey: PAnsiChar;
  nBoolNode: Bool;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) or (Nil = pbBoolValue) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  Result := MV_CC_GetBoolValue(hDevHandle^, pAnsiKey, @nBoolNode);
  pbBoolValue^ := nBoolNode;
end;

// Set Boolean value
function SetBoolValue(hDevHandle: PPointer; const strKey: String; bBoolValue: Bool): Integer;
Var
  pAnsiKey: PAnsiChar;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  Result := MV_CC_SetBoolValue(hDevHandle^, pAnsiKey, bBoolValue);
end;

// Get String value
function GetStringValue(hDevHandle: PPointer; const strKey: String; Var pStrValue: String): Integer;
Var
  pAnsiKey: PAnsiChar;
  strNode: MVCC_STRINGVALUE;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  ZeroMemory(@strNode, sizeof(MVCC_STRINGVALUE));
  Result := MV_CC_GetStringValue(hDevHandle^, pAnsiKey, @strNode);
  pStrValue := PAnsiChar(@(strNode.chCurValue));
end;

// Set String value
function SetStringValue(hDevHandle: PPointer; const strKey: String; pStrValue: String): Integer;
Var
  pAnsiKey: PAnsiChar;
  pAnsiValue: PAnsiChar;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  pAnsiValue := PAnsiChar(AnsiString(pStrValue));
  Result := MV_CC_SetStringValue(hDevHandle^, pAnsiKey, pAnsiValue);
end;

// Set Command value
function SetCommandValue(hDevHandle: PPointer; const strKey: String): Integer;
Var
  pAnsiKey: PAnsiChar;

begin
  if (Nil = hDevHandle) or (0 = length(strKey)) then
  begin
    Result := MV_E_PARAMETER;
    exit
  end;

  pAnsiKey := PAnsiChar(AnsiString(strKey));
  Result := MV_CC_SetCommandValue(hDevHandle^, pAnsiKey);
end;

// Convert the C-Style char sequences to String
function _FormaCtName(const Format: string): string; cdecl;
const
  StackSlotSize = sizeof(Pointer);
var
  Args: va_list;
  Buffer: array [0 .. 63] of Char;
begin
  Args := va_list(PAnsiChar(@Format) + ((sizeof(Format) + StackSlotSize - 1) and
    not(StackSlotSize - 1)));
  SetString(Result, Buffer, wvsprintf(Buffer, PChar(Format), Args));
end;

end.
