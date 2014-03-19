unit SPBDLL;

interface

uses
  Windows;

const
  SIMPLEPB_DLL = 'SimplePB.DLL';

function OpenPort(Com, Baud: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function OpenIPPort(IPAdd: PAnsiChar; IPPort: Integer): Integer; stdcall;
  external SIMPLEPB_DLL;
function ClosePort: Integer; stdcall; external SIMPLEPB_DLL;
function CloseIPPort: Integer; stdcall; external SIMPLEPB_DLL;
function GetClock(Address, Device: Integer; out pData: PAnsiChar;
  out pLgh: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function SetClock(Address, Device: Integer; out pData: PAnsiChar;
  out pLgh: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function GetValue(Address, Device, Swath: Integer; Table, Field: PAnsiChar;
  out pData: PAnsiChar; out pLgh: Integer): Integer; stdcall;
  external SIMPLEPB_DLL;
function SetValue(Address, Device: Integer; Table, Field: PAnsiChar;
  val: PAnsiChar): Integer; stdcall; external SIMPLEPB_DLL;
function GetData(Address, Device, Table, Recrd: Integer; out pData: PAnsiChar;
  out pLgh: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function GetDataHeader(Address, Device, Table: Integer; out pData: PAnsiChar;
  out pLgh: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function GetCommaData(Address, Device, Table, Recrd: Integer;
  out pData: PAnsiChar; out pLgh: Integer): Integer; stdcall;
  external SIMPLEPB_DLL;
function File_Send(Address, Device: Integer; FStr: PAnsiChar;
  out pData: PAnsiChar; out pLgh: Integer): Integer; stdcall;
  external SIMPLEPB_DLL;
function GetAddress(Device: Integer; out pData: PAnsiChar; out pLgh: Integer)
  : Integer; stdcall; external SIMPLEPB_DLL;
function GetStatus(Address, Device: Integer; out pData: PAnsiChar;
  out pLgh: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function GetTableNames(Address, Device: Integer; out pData: PAnsiChar;
  out pLgh: Integer): Integer; stdcall; external SIMPLEPB_DLL;
function GetDLLVersion(out pData: PAnsiChar; out pLgh: Integer): Integer;
  stdcall; external SIMPLEPB_DLL;

implementation

end.
