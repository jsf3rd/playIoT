library MMFReader;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  System.SysUtils,
  System.Classes, JdcSharedMem.Reader, JdcSharedMem.Common;

{$R *.res}

function EchoStr(AValue: pointer; const str: PAnsiChar): PAnsiChar; export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  result := PAnsiChar(AnsiString(Reader.EchoString(String(str))));
end;

function OpenMMF(const AValue: PAnsiChar): pointer; export; stdcall;
begin
  result := TJdcSharedMemReader.Create(String(AValue));
end;

procedure CloseMMF(AValue: pointer); export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  Reader.Free;
end;

function GetFirst(AValue: pointer): pointer; export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  result := Reader.GetFirstPointer;
end;

function GetNext(AValue: pointer): pointer; export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  result := Reader.GetNextPointer;
end;

function GetLast(AValue: pointer): pointer; export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  result := Reader.GetLastPointer;
end;

function GetCurrentSequence(AValue: pointer): Int32; export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  result := Reader.GetCurrentSequence;
end;

function GetLastSequence(AValue: pointer): Int32; export; stdcall;
var
  Reader: TJdcSharedMemReader absolute AValue;
begin
  result := Reader.GetLastSequence;
end;

exports EchoStr, OpenMMF, CloseMMF, GetFirst, GetNext, GetLast, GetCurrentSequence, GetLastSequence;

begin

end.
