unit CANProPlusDll;

interface

uses
  Windows, System.SysUtils, System.Classes, System.Generics.Collections;

const
{$IFDEF WIN32}
  CANPRO_PLUS_DLL = 'CANProPlus_NET.dll';
{$ELSE}
  CANPRO_PLUS_DLL = 'CANProPlus_64.dll';
{$ENDIF}

type
  TCanDeviceInfo = packed record
    device_type: Byte;
    serial: Array [0 .. 15] of AnsiChar;
  end;

  PCanDeviceInfo = ^TCanDeviceInfo;

  TCanDeviceInfoList = Array of TCanDeviceInfo;
  PCanDeviceInfoList = ^TCanDeviceInfoList;

  TBits = packed record
    reserved: array [0 .. 2] of Boolean;
    hi_volt: Boolean;
    extended: Boolean;
    remote: Boolean;
    _type: array [0 .. 1] of Boolean;
  end;

  TInfo = packed record
    Info: Byte;
    Bits: TBits;
  end;

  TData = Array [0 .. 7] of Byte;
  TData4 = Array [0 .. 3] of Byte;

  TCANProMessage = packed record
    Info: TInfo;
    // time: UInt64;
    Id: UInt32;
    dlc: Byte;
    data: TData;
    function ToECanMessage: TeCanMessage;
  end;

  TCreateInstance = function(): THandle; stdcall;
  TDisposeInstance = procedure(AInstance: THandle); cdecl;

  TGetLibraryVersion = function(AInstance: THandle; out version: USHORT): Integer; stdcall;
  TGetVersion = function(AInstance: THandle; out version: USHORT): Integer; stdcall;

  TGetDeviceCount2 = function(): Integer; stdcall;

  TGetDeviceCount = function(AInstance: THandle): Integer; cdecl;
  TGetDeviceList = function(AInstance: THandle; out device_count: Integer): PCanDeviceInfo; stdcall;
  TConnect = function(AInstance: THandle; serial: PAnsiChar): Integer; stdcall;
  TDisconnect = procedure(AInstance: THandle); cdecl;

  TFree = procedure(AInstance: THandle; AHandle: Pointer); cdecl;

  TStartTrace = function(AInstance: THandle): Integer; cdecl;
  TStopTrace = function(AInstance: THandle): Integer; cdecl;
  TGetTraceMsgCount = function(AInstance: THandle): Integer; cdecl;
  TPopTraceMsg = function(AInstance: THandle; out msg: TCANProMessage): Integer; stdcall;

  TPushTxMsg = function(AInstance: THandle; var msg: TCANProMessage): Integer; stdcall;
  TStartTxMsg = function(AInstance: THandle): Integer; cdecl;

  TCanProPlus = class
  private
    FDLLHandle: THandle;

    FCreateInstance: TCreateInstance;
    FDisposeInstance: TDisposeInstance;

    FGetDeviceCount2: TGetDeviceCount2;
    FGetDeviceCount: TGetDeviceCount;
    FGetLibraryVersion: TGetLibraryVersion;
    FGetVersion: TGetVersion;

    FGetDeviceList: TGetDeviceList;
    FConnect: TConnect;

    FFree: TFree;

    FDisconnect: TDisconnect;
    FStartTrace: TStartTrace;
    FStopTrace: TStopTrace;
    FGetTraceMsgCount: TGetTraceMsgCount;
    FPopTraceMsg: TPopTraceMsg;

    FPushTxMsg: TPushTxMsg;
    FStartTxMsg: TStartTxMsg;

    FInstance: THandle;
    procedure FreeDLL;
    procedure LoadDLL;
  public
    constructor Create();
    destructor Destroy; override;

    procedure FreeMem(pt: Pointer);

    function CreateInstance: Boolean;
    procedure DisposeInstance;

    function GetDeviceCount: Integer;
    function GetDeviceList(out count: Integer): PCanDeviceInfo;

    function GetVersion: WORD;
    function GetLibraryVersion: WORD;

    function Connect(ASerial: PAnsiChar): Boolean;
    procedure Disconnect();

    function StartTrace(): Boolean;
    function StopTrace(): Boolean;

    function GetTraceMsgCount: Integer;
    function PopTraceMsg: TCANProMessage;

    function PushTxMsg(var msg: TCANProMessage): Integer;
    function StartTxMsg: Integer;
  end;

implementation

{ TCanProPlus }

function TCanProPlus.Connect(ASerial: PAnsiChar): Boolean;
var
  rlt: Integer;
begin
  rlt := FConnect(FInstance, ASerial);
  result := rlt = 0;
end;

constructor TCanProPlus.Create;
begin
  FInstance := 0;
  FDLLHandle := 0;
  LoadDLL;
end;

function TCanProPlus.CreateInstance: Boolean;
begin
  FInstance := FCreateInstance;
  result := FInstance > 0;
end;

destructor TCanProPlus.Destroy;
begin
  if FInstance > 0 then
    DisposeInstance;

  FreeDLL;
  inherited;
end;

procedure TCanProPlus.Disconnect;
begin
  FDisconnect(FInstance);
end;

procedure TCanProPlus.DisposeInstance;
begin
  FDisposeInstance(FInstance);
  FInstance := 0;
end;

procedure TCanProPlus.FreeDLL;
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

procedure TCanProPlus.FreeMem(pt: Pointer);
begin
  FFree(FInstance, pt);
end;

function TCanProPlus.GetDeviceCount(): Integer;
begin
  result := FGetDeviceCount(FInstance);
end;

function TCanProPlus.GetDeviceList(out count: Integer): PCanDeviceInfo;
begin
  result := FGetDeviceList(FInstance, count);
end;

procedure TCanProPlus.LoadDLL;
begin
  FDLLHandle := LoadLibrary(CANPRO_PLUS_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  // @FGetDeviceCount2 := GetProcAddress(FDLLHandle, 'GetDeviceCount');

  @FCreateInstance := GetProcAddress(FDLLHandle, 'W_CreateCANProPlusCore');
  @FDisposeInstance := GetProcAddress(FDLLHandle, 'W_DisposeCANProPlusCore');

  @FGetLibraryVersion := GetProcAddress(FDLLHandle, 'W_GetLibraryVersion');
  @FGetVersion := GetProcAddress(FDLLHandle, 'W_GetVersion');
  @FGetDeviceCount := GetProcAddress(FDLLHandle, 'W_GetDeviceCount');

  @FGetDeviceList := GetProcAddress(FDLLHandle, 'W_GetDeviceList');
  @FConnect := GetProcAddress(FDLLHandle, 'W_Connect');

  @FFree := GetProcAddress(FDLLHandle, 'W_Free');

  @FDisconnect := GetProcAddress(FDLLHandle, 'W_Disconnect');
  @FStartTrace := GetProcAddress(FDLLHandle, 'W_StartTrace');
  @FStopTrace := GetProcAddress(FDLLHandle, 'W_StopTrace');
  @FGetTraceMsgCount := GetProcAddress(FDLLHandle, 'W_GetTraceMsgCount');
  @FPopTraceMsg := GetProcAddress(FDLLHandle, 'W_PopTraceMsg');

  @FPushTxMsg := GetProcAddress(FDLLHandle, 'W_PushTxMsg');
  @FStartTxMsg := GetProcAddress(FDLLHandle, 'W_StartTxMsg');
end;

function TCanProPlus.PopTraceMsg: TCANProMessage;
var
  rlt: Integer;
begin
  rlt := FPopTraceMsg(FInstance, result);
end;

function TCanProPlus.PushTxMsg(var msg: TCANProMessage): Integer;
begin
  result := FPushTxMsg(FInstance, msg);
end;

function TCanProPlus.StartTrace: Boolean;
begin
  result := FStartTrace(FInstance) = 0;
end;

function TCanProPlus.StartTxMsg: Integer;
begin
  result := FStartTxMsg(FInstance);
end;

function TCanProPlus.StopTrace: Boolean;
begin
  result := FStopTrace(FInstance) = 0;
end;

function TCanProPlus.GetLibraryVersion: WORD;
var
  rlt: Integer;
begin
  rlt := FGetLibraryVersion(FInstance, result);
end;

function TCanProPlus.GetTraceMsgCount: Integer;
begin
  result := FGetTraceMsgCount(FInstance);
end;

function TCanProPlus.GetVersion: WORD;
var
  rlt: Integer;
begin
  rlt := FGetVersion(FInstance, result);
end;

{ TCANProMessage }

function TCANProMessage.ToECanMessage: TeCanMessage;
begin
  result._type := Self.Info.Info;
  result.Id := Self.Id;
  result.dlc := Self.dlc;
  result.data := Self.data;
end;

end.
