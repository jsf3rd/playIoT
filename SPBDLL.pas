unit SPBDLL;

interface

uses
  Windows;

const
  SIMPLEPB_DLL = 'SimplePB.DLL';

type
  TOpenPort = function(Com, Baud: Integer): Integer; stdcall;
  TOpenIPPort = function(IPAdd: PAnsiChar; IPPort: Integer): Integer; stdcall;
  TClosePort = function: Integer; stdcall;
  TCloseIPPort = function: Integer; stdcall;
  TGetClock = function(Address, Device: Integer; out pData: PAnsiChar; out pLgh: Integer)
    : Integer; stdcall;
  TSetClock = function(Address, Device: Integer; out pData: PAnsiChar; out pLgh: Integer)
    : Integer; stdcall;
  TGetValue = function(Address, Device, Swath: Integer; Table, Field: PAnsiChar;
    out pData: PAnsiChar; out pLgh: Integer): Integer; stdcall;
  TSetValue = function(Address, Device: Integer; Table, Field: PAnsiChar; val: PAnsiChar)
    : Integer; stdcall;
  TGetData = function(Address, Device, Table, Recrd: Integer; out pData: PAnsiChar;
    out pLgh: Integer): Integer; stdcall;
  TGetDataHeader = function(Address, Device, Table: Integer; out pData: PAnsiChar;
    out pLgh: Integer): Integer; stdcall;
  TGetCommaData = function(Address, Device, Table, Recrd: Integer; out pData: PAnsiChar;
    out pLgh: Integer): Integer; stdcall;
  TFile_Send = function(Address, Device: Integer; FStr: PAnsiChar; out pData: PAnsiChar;
    out pLgh: Integer): Integer; stdcall;
  TGetAddress = function(Device: Integer; out pData: PAnsiChar; out pLgh: Integer)
    : Integer; stdcall;
  TGetStatus = function(Address, Device: Integer; out pData: PAnsiChar; out pLgh: Integer)
    : Integer; stdcall;
  TGetTableNames = function(Address, Device: Integer; out pData: PAnsiChar; out pLgh: Integer)
    : Integer; stdcall;
  TGetDLLVersion = function(out pData: PAnsiChar; out pLgh: Integer): Integer; stdcall;

implementation

end.
