unit DLLinc;
// ------------------------------------------------------------------------------
// Description: Pascal/Delphi interface for NetPod driver DLL
// Copyright Keynes Controls (C) 1997-1998 all rights reserved
// Author : Paul Bayton
// Version : 1.01
// ------------------------------------------------------------------------------

interface

uses Windows, SysUtils;

const
  NETPOD_DLL = 'NETPOD.DLL';

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
    data: packed array [0 .. 15] of Single;
  end;

  TShortString = packed array [0 .. 31] of AnsiChar;
  TPodList = packed array [0 .. 100] of Integer;

  TPodInfoStruct = packed record
    // Fixed:
    PodId: Integer;
    Ethernet: array [0 .. 5] of Byte;
    StrEthernet: TShortString;
    SWVersion: Integer;
    PartNumber: TShortString;
    SerialNumber: TShortString;
    ManYY, ManMM, ManDD: Integer;
    StrManDate: TShortString;
    // Editable:
    IPAddr: array [0 .. 1] of SmallInt;
    StrIPAddr: TShortString;
    Name: TShortString;
    SampleRate: Double;
    // digital
    DigDDR: word;
    DigDefault: word;
    PortType: array [0 .. 15] of Byte;
  end;

  TChannelInfoStruct = packed record
    PodId: Integer;
    Channel: Integer; // 0 to 15
    // fixed EEPROM:
    IsInstalled: Integer;
    ADCType: Integer; // 16 or 24 bit
    ADCResolution: Integer;
    PortType: Integer; // thermocouple etc
    SWVersion: Integer;
    PartNumber: TShortString;
    SerialNumber: TShortString;
    ManYY, ManMM, ManDD: Integer; // date of manufacture
    StrManDate: TShortString;
    // changeable EEPROM:
    Gain: Integer;
    Name: TShortString; // char[32]
    Units: TShortString; // processed units
    DefCal: packed array [0 .. 1] of Single; // default calibration
    Cal: packed array [0 .. 3] of Single; // calibration coeffients
  end;

  TBufParamStruct = packed record
    StartSample: TSampleL;
    StartTime: TAccurateTime;
    LatestSample: TSampleL;
    LatestTime: TAccurateTime;
    LatestContSample: TSampleL;
    LatestContTime: TAccurateTime;
    reserved: array [0 .. 64] of Integer;
    function LatestDateTime: TDateTime;
    function LatestDateTimeStr: String;
    function SampleCount: Int64;
    function ToString: string;
  end;

  TNP_GetStatus = function(stat: Integer): Integer stdcall;
  TNP_SetStatus = function(stat: Integer): Integer stdcall;
  TNP_GetPodInfo = function(pid: Integer; var Info: TPodInfoStruct): Integer stdcall;
  TNP_GetChannelInfo = function(pid, cid: Integer; var Info: TChannelInfoStruct)
    : Integer stdcall;
  TNP_GetPodList = function(data: TArray<Integer>): Integer stdcall;
  TNP_ChannelBufRead = function(pid, cid, flag: Integer; StartP: TSampleL; length: Integer;
    results: Pointer): Integer stdcall;
  TNP_GetBufParam = function(pid: Integer; var Param: TBufParamStruct): Integer stdcall;

implementation

{ TBufParamStruct }

function TBufParamStruct.LatestDateTime: TDateTime;
var
  stime: SYSTEMTIME;
begin
  FileTimeToSystemTime(Self.LatestTime, stime);
  result := SystemTimeToDateTime(stime) + 9 / 24; // 표준시에 9시간 더함
end;

function TBufParamStruct.LatestDateTimeStr: String;
begin
  result := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', Self.LatestDateTime);
end;

function TBufParamStruct.SampleCount: Int64;
begin
  result := Self.LatestSample - Self.StartSample;
end;

function TBufParamStruct.ToString: string;
begin
  result := Format('StartSample=%s,LastestSample=%s, LatestDateTime=%s',
    [Self.StartSample.ToString, Self.LatestSample.ToString, Self.LatestDateTimeStr])
end;

end.
