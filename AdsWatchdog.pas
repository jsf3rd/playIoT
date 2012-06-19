{*******************************************************}
{                                                       }
{       Advantech Watchdog Timer                        }
{                                                       }
{       Copyright (C) 2008 (주)에이티맥스, isul         }
{                                                       }
{*******************************************************}



unit AdsWatchdog;

interface

function WDT_IsEnabled(hHandle: Integer; var bEnabled: Boolean): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_Init(var hHandle: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_DeInit(var hHandle: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_Enable(hHandle: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_Disable(hHandle: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_GetMode(hHandle: Integer; var watchmode: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_SetMode(hHandle: Integer; watchmode: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_GetType(hHandle: Integer; var WatchdogType: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_SetTimerSpan(hHandle: Integer; dwIndex: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_Reboot(hHandle: Integer): Integer; stdcall; external 'AdsWatchdog.dll';
function WDT_Strobe(hHandle: Integer): Integer; stdcall; external 'AdsWatchdog.dll';


const
  WATCH_MODE_SYSTEM       = 0;
  WATCH_MODE_APPLICATION  = 1;



implementation



end.
 