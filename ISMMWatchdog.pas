unit ISMMWatchdog;

interface

Const
  WDT_START_VALUE = 1;
  WDT_STOP_VALUE = 0;

  RESULT_WDT_START_VALUE = 1; //WDT_START 성공 1
  RESULT_WDT_STOP_VALUE = 0; //WDT_START  실패 0

  WDT_TIMEOPTION_SECOND = 1;
  WDT_TIMEOPTION_MINUTE = 2;

function ISMM_Open: Integer; stdcall; external 'ISMM.dll';
function ISMM_Close(hHandle: THandle): Integer; stdcall; external 'ISMM.dll';
function WDT_Start(hHandle: THandle; bControlState: Integer): Integer; stdcall;
  external 'ISMM.dll';
function WDT_Restart(hHandle: THandle): Integer; stdcall; external 'ISMM.dll';
function WDT_SetTimeout(hHandle: THandle; wTimeout: word; wTimeOption: word)
  : Integer; stdcall; external 'ISMM.dll';
function WDT_GetTimeoutOption(hHandle: THandle; var pwTimeout: word;
  var pwTimeOption: word): Integer; stdcall; external 'ISMM.dll';

implementation

end.
