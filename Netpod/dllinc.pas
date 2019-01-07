unit DLLinc;
// ------------------------------------------------------------------------------
// Description: Pascal/Delphi interface for NetPod driver DLL
// Copyright Keynes Controls (C) 1997-1998 all rights reserved
// Author : Paul Bayton
// Version : 1.01
// ------------------------------------------------------------------------------

interface

uses windows;

const
  // Flags
  NP_RAW = 1;
  NP_CAL = 2;
  NP_PROC = 3;
  NP_DIGITAL = 4;
  NP_TIME = 5;

  // ADC Types
  ADC_16 = 1;
  ADC_24 = 2;

  // status
  NP_ISCFGDLGSHOWN = 1; // Is Config dialog shown
  NP_ISRUNNING = 2; // Are we running
  NP_ISCALLBACK = 3; // Windows initialised (for hook)
  NP_ISINITSCAN = 4; // have we performed initial scan?
  NP_ISFINUPDATE = 5;
  NP_INSTANCECOUNT = 6;
  NP_OFFLINEPODS = 7;
  NP_GETOFFLINEPID = 8;
  NP_ERROR = 65535; // Error

  // commands
  NP_STARTRUN = 3; // start running
  NP_STOPRUN = 4; // stop running
  NP_TOGGLERUN = 5; // toggle running status
  NP_SETCALLBACK = 10; // Create windows and hooks
  NP_SHOWCFGDLG = 11; // show config dialog
  NP_HIDECFGDLG = 12; // hide config dialog
  NP_SHOWCFGDLG2 = 13;
  NP_RUNPODMNG = 21; // run Pod Manager
  NP_SETUPCHAN = 31;
  NP_SETUPPOD = 32;
  NP_SCANNET = 41;

  //
  // Configuration Write
  NP_SETSAMPLERATE = 5;

  // Error codes
  NO_NOERR = 0;

type

  TSample = LONGLONG;
  TSampleL = LONGLONG;
  TAccurateTime = TFileTime;

  TFloatArray = packed record
    data: packed array [0 .. 15] of single;
  end;

  TShortString = packed array [0 .. 31] of AnsiChar;
  TPodList = packed array [0 .. 100] of integer;

  TPodInfoStruct = packed record
    // Fixed:
    PodId: integer;
    Ethernet: array [0 .. 5] of byte;
    StrEthernet: TShortString;
    SWVersion: integer;
    PartNumber: TShortString;
    SerialNumber: TShortString;
    ManYY, ManMM, ManDD: integer;
    StrManDate: TShortString;
    // Editable:
    IPAddr: array [0 .. 1] of SmallInt;
    StrIPAddr: TShortString;
    Name: TShortString;
    SampleRate: Double;
    // digital
    DigDDR: word;
    DigDefault: word;
    PortType: array [0 .. 15] of byte;
  end;

  TChannelInfoStruct = packed record
    PodId: integer;
    Channel: integer; // 0 to 15
    // fixed EEPROM:
    IsInstalled: integer;
    ADCType: integer; // 16 or 24 bit
    ADCResolution: integer;
    PortType: integer; // thermocouple etc
    SWVersion: integer;
    PartNumber: TShortString;
    SerialNumber: TShortString;
    ManYY, ManMM, ManDD: integer; // date of manufacture
    StrManDate: TShortString;
    // changeable EEPROM:
    Gain: integer;
    Name: TShortString; // char[32]
    Units: TShortString; // processed units
    DefCal: packed array [0 .. 1] of single; // default calibration
    Cal: packed array [0 .. 3] of single; // calibration coeffients
  end;

  TBufParamStruct = packed record
    StartSample: TSampleL;
    StartTime: TAccurateTime;
    LatestSample: TSampleL;
    LatestTime: TAccurateTime;
    LatestContSample: TSampleL;
    LatestContTime: TAccurateTime;
    reserved: array [0 .. 64] of integer;
  end;

function NP_SimpleRead(pid, flag: integer; var results): integer;
stdcall external 'netpod.dll' name 'NP_SimpleRead';
function NP_SetDigital(pid, value: integer): integer;
stdcall external 'netpod.dll' name 'NP_SetDigital'
function NP_GetStatus(stat: integer): integer;
stdcall external 'netpod.dll' name 'NP_GetStatus'
function NP_SetStatus(stat: integer): integer;
stdcall external 'netpod.dll' name 'NP_SetStatus'
function NP_GetPodInfo(pid: integer; var Info: TPodInfoStruct): integer;
stdcall;
external 'netpod.dll' name 'NP_GetPodInfo';
function NP_GetChannelInfo(pid, cid: integer; var Info: TChannelInfoStruct): integer; stdcall;
  external 'netpod.dll' name 'NP_GetChannelInfo';
function NP_GetPodList(data: TArray<integer>): integer; stdcall;
  external 'netpod.dll' name 'NP_GetPodList';
function NP_PutPodInfo(pid: integer; var Info: TPodInfoStruct): integer; stdcall;
  external 'netpod.dll' name 'NP_PutPodInfo';
function NP_PutChannelInfo(pid, cid: integer; var Info: TChannelInfoStruct): integer; stdcall;
  external 'netpod.dll' name 'NP_PutChannelInfo';
function NP_ChannelBufRead(pid, cid, flag: integer; StartP: TSampleL; length: integer;
  results: Pointer): integer; stdcall; stdcall; external 'netpod.dll' name 'NP_ChannelBufRead';
function NP_GetBufParam(pid: integer; var Param: TBufParamStruct): integer; stdcall;
  external 'netpod.dll' name 'NP_GetBufParam';

implementation

end.
