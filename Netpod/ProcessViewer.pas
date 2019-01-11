unit ProcessViewer;

interface

uses
  Windows, SysUtils, Classes, ShellAPI, TLHelp32, Forms;

const
  SleepForReCheck = 5000;

type
  TProcessInfo = record
    FileName: string;
    Caption: string;
    Visible: boolean;
    Handle: DWord;
    PClass: string;
    ThreadID: DWord;
    PID: DWord;
  end;

var
  ProcessInfo: array of TProcessInfo;

function KillProcessByPID(PID: DWord): boolean;
function KillProcessByFileName(FileName: string; KillAll: boolean): boolean;
procedure GetProcessList;
function IsFileActive(FileName: String): boolean;

implementation

function GetProcessPath(PID: DWord): string;
var
  H: THandle;
  pe: TModuleEntry32;
  more: boolean;
  tmp: string;
begin
  if PID = 0 then
  begin
    result := '';
    exit;
  end;

  H := CreateToolhelp32Snapshot(TH32CS_SNAPMODULE, PID);
  if H = 0 then
    exit;
  try
    pe.dwSize := SizeOf(pe);
    more := Module32First(H, pe);

    while more do
    begin
      if StrPas(pe.szModule) = ExtractFileName(StrPas(pe.szExePath)) then
        tmp := StrPas(pe.szExePath);
      break;
      more := Module32First(H, pe);
    end;
  finally
    result := tmp;
    CloseHandle(H);
  end;
end;

procedure GetProcessList;
var
  H: THandle;
  pe: TProcessEntry32;
  B: boolean;
  i: Integer;
begin
  H := CreateToolhelp32Snapshot(TH32CS_SNAPALL, 0);
  try
    if H = 0 then
      exit;

    pe.dwSize := SizeOf(pe);
    B := Process32First(H, pe);
    i := 1;
    SetLength(ProcessInfo, i);
    while B do
    begin
      // ProcessInfo[i - 1].Caption   := GetProcessPath(pe.th32ProcessId);
      ProcessInfo[i - 1].FileName := StrPas(pe.szExeFile);
      ProcessInfo[i - 1].PID := pe.th32ProcessId;
      B := Process32Next(H, pe);
      if B then
      begin
        inc(i);
        SetLength(ProcessInfo, i);
      end;
    end;
  finally
    CloseHandle(H);
  end;
end;

function IsFileActive(FileName: String): boolean;
var
  i: Integer;
begin
  result := false;
  if FileName = '' then
    exit;
  GetProcessList;
  FileName := UpperCase(ExtractFileName(FileName));
  for i := 0 to Length(ProcessInfo) - 1 do
    if Pos(FileName, UpperCase(ProcessInfo[i].FileName)) > 0 then
    begin
      result := true;
      break;
    end;
end;

function KillProcessByPID(PID: DWord): boolean;
var
  myhandle: THandle;
  i: Integer;
begin
  result := true;
  myhandle := OpenProcess(PROCESS_TERMINATE, false, PID);
  TerminateProcess(myhandle, 0);
  for i := 0 to SleepForReCheck do
    Application.ProcessMessages; // Genug Zeit geben
  GetProcessList;
  for i := 0 to Length(ProcessInfo) - 1 do
    if ProcessInfo[i].PID = PID then
    begin
      result := false;
      exit;
    end;
end;

function KillProcessByFileName(FileName: string; KillAll: boolean): boolean;
var
  i: Integer;
  FileFound: boolean;
begin
  result := false;
  if FileName = '' then
    exit;
  FileName := UpperCase(ExtractFileName(FileName));
  result := true;
  GetProcessList;
  if KillAll then
  begin
    // Kill all
    repeat
      GetProcessList;
      FileFound := false;
      for i := 0 to Length(ProcessInfo) - 1 do
        if UpperCase(ProcessInfo[i].FileName) = FileName then
        begin
          FileFound := true;
          break;
        end;

      if i < Length(ProcessInfo) then
        if not KillProcessByPID(ProcessInfo[i].PID) then
        begin
          result := false;
          exit;
        end;
    until not FileFound;
  end
  else
  begin
    // Kill one except me
    for i := 0 to Length(ProcessInfo) - 1 do
      if (GetCurrentProcessId <> ProcessInfo[i].PID) and
        (UpperCase(ProcessInfo[i].FileName) = FileName) then
        break;

    if i < Length(ProcessInfo) then
      if not KillProcessByPID(ProcessInfo[i].PID) then
      begin
        result := false;
        exit;
      end;
  end;
end;

initialization

finalization

SetLength(ProcessInfo, 0);

end.
