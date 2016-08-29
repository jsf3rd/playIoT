{ -----------------------------------------------------------------------------
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: SharedMMFMem, released on 2003-12-05

  The Initial Developers of the Original Code are: Andreas Hausladen <Andreas dott Hausladen att gmx dott de>
  Copyright (c) 2003 Andreas Hausladen
  All Rights Reserved.

  Contributor(s):
  You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
  located at http://jcl.sourceforge.net
  ----------------------------------------------------------------------------- }

unit SharedMMFMem;

{$I jedi/jedi.inc}

interface

uses
  Windows, SysUtils, JdcGlobal;

type
  ESharedMemError = class(Exception);

  { Functions for the shared memory owner }

  { SharedGetMem return ERROR_ALREADY_EXISTS if the shared memory is already,
    otherwise it return 0.
    Throws ESharedMemError if the Name is invalid. }
function SharedGetMem(var p { : Pointer }; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;

{ SharedAllocMem calls SharedGetMem and then fills the memory with zero if
  it was not already allocated.
  Throws ESharedMemError if the Name is invalid. }
function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;

{ SharedFreeMem releases the shared memory if it was the last reference. }
function SharedFreeMem(var p { : Pointer } ): Boolean;

{ Functions for the shared memory user }

{ SharedOpenMem returns True if the shared memory was already allocated by
  SharedGetMem or SharedAllocMem. Otherwise it return False.
  Throws ESharedMemError if the Name is invalid. }

function SharedOpenMem(var p { : Pointer }; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean; overload;

{ SharedOpenMem return nil if the shared memory was not already allocated
  by SharedGetMem or SharedAllocMem.
  Throws ESharedMemError if the Name is invalid. }
function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer; overload;

{ SharedCloseMem releases the shared memory if it was the last reference. }
function SharedCloseMem(var p { : Pointer } ): Boolean;

implementation

resourcestring
  SInvalidMMFName = 'Invalid MMF name "%s"';

type
  PMMFHandleList = ^TMMFHandleList;

  TMMFHandleList = record
    Next: PMMFHandleList;
    Memory: Pointer;
    Handle: THandle;
    Name: string;
    References: Integer;
  end;

var
  MMFHandleList: PMMFHandleList;

function SharedGetMem(var p { : Pointer }; const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Integer;
var
  h: THandle;
  n, Iterate: PMMFHandleList;
  Protect: Cardinal;
begin
  Result := 0;
  Pointer(p) := nil;

  if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) or (Win32MajorVersion < 5)
  then
    if (Name = '') or (Pos('\', Name) > 0) then
      raise ESharedMemError.CreateFmt(SInvalidMMFName, [Name]);

  // search for same name
  Iterate := MMFHandleList;
  while Iterate <> nil do
  begin
    if CompareText(Iterate^.Name, Name) = 0 then
    begin
      Inc(Iterate^.References);
      Pointer(p) := Iterate^.Memory;
      Result := ERROR_ALREADY_EXISTS;
      PrintDebug('[IncRef] Name=%s,Ref=%d', [Name, Iterate^.References]);
      Exit;
    end;
    Iterate := Iterate^.Next;
  end;

  // open file mapping
  h := OpenFileMapping(DesiredAccess, False, PChar(Name));
  if h = 0 then
  begin
    if Size = 0 then
      Exit; // nur ?fnen, nicht erzeugen
    Protect := PAGE_READWRITE;
    if (Win32Platform = VER_PLATFORM_WIN32_WINDOWS) and
      (DesiredAccess = FILE_MAP_COPY) then
      Protect := PAGE_WRITECOPY;

    h := CreateFileMapping(INVALID_HANDLE_VALUE, nil, Protect, 0, Size,
      PChar(Name));
    PrintDebug('[CreateMMF] Name=%s,Size=%u,Protect=%u', [Name, Size, Protect]);
  end
  else
  begin
    Result := ERROR_ALREADY_EXISTS;
    PrintDebug('[OpenMMF] Name=%s,DesiredAccess=%u', [Name, DesiredAccess]);
  end;

  case GetLastError of
    ERROR_ALREADY_EXISTS:
      Result := ERROR_ALREADY_EXISTS;
  else
    if h = 0 then
{$IFDEF COMPILER6_UP}
      RaiseLastOSError;
{$ELSE}
      RaiseLastWin32Error;
{$ENDIF COMPILER6_UP}
  end;

  // map view
  Pointer(p) := MapViewOfFile(h, DesiredAccess, 0, 0, Size);
  if Pointer(p) = nil then
  begin
    try
{$IFDEF COMPILER6_UP}
      RaiseLastOSError;
{$ELSE}
      RaiseLastWin32Error;
{$ENDIF COMPILER6_UP}
    except
      CloseHandle(h);
      raise;
    end;
  end;

  // add list item to MMFHandleList
  New(n);
  n^.Name := Name;
  n^.Handle := h;
  n^.Memory := Pointer(p);
  n^.Next := nil;
  n^.References := 1;

  if MMFHandleList = nil then
    MMFHandleList := n
  else
  begin
    Iterate := MMFHandleList;
    while Iterate^.Next <> nil do
      Iterate := Iterate^.Next;
    Iterate^.Next := n;
  end;
end;

function SharedAllocMem(const Name: string; Size: Cardinal;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  if SharedGetMem(Result, Name, Size, DesiredAccess) <> ERROR_ALREADY_EXISTS
  then
    if ((DesiredAccess and (FILE_MAP_WRITE or FILE_MAP_COPY)) <> 0) and
      (Size > 0) and (Result <> nil) then
      FillChar(Pointer(Result)^, Size, 0);
end;

function SharedFreeMem(var p { : Pointer } ): Boolean;
var
  n, Iterate: PMMFHandleList;
begin
  if Pointer(p) <> nil then
  begin
    Result := False;
    Iterate := MMFHandleList;
    n := nil;
    while Iterate <> nil do
    begin
      if Iterate^.Memory = Pointer(p) then
      begin
        if Iterate^.References > 1 then
        begin
          Dec(Iterate^.References);
          Pointer(p) := nil;
          Result := True;
          PrintDebug('[DecRef] Name=%s,Ref=%d',
            [Iterate^.Name, Iterate^.References]);
          Exit;
        end;

        if UnmapViewOfFile(Iterate^.Memory) then
          PrintDebug('[CloseMMF] Name=%s', [Iterate^.Name]);
        CloseHandle(Iterate^.Handle);

        if n = nil then
          MMFHandleList := Iterate^.Next
        else
        begin
          n^.Next := Iterate^.Next;
          Dispose(Iterate);
          Pointer(p) := nil;
          Result := True;
          Break;
        end;
      end;
      n := Iterate;
      Iterate := Iterate^.Next;
    end;
  end
  else
    Result := True;
end;

function SharedOpenMem(var p { : Pointer }; const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Boolean;
begin
  Result := SharedGetMem(p, Name, 0, DesiredAccess) = ERROR_ALREADY_EXISTS;
end;

function SharedOpenMem(const Name: string;
  DesiredAccess: Cardinal = FILE_MAP_ALL_ACCESS): Pointer;
begin
  SharedGetMem(Result, Name, 0, DesiredAccess);
end;

function SharedCloseMem(var p { : Pointer } ): Boolean;
begin
  Result := SharedFreeMem(p);
end;

procedure FinalizeMMFHandleList;
var
  n, Iterate: PMMFHandleList;
begin
  Iterate := MMFHandleList;
  while Iterate <> nil do
  begin
    UnmapViewOfFile(Iterate^.Memory);
    CloseHandle(Iterate^.Handle);

    n := Iterate^.Next;
    Dispose(Iterate);
    Iterate := n;
  end;
end;

initialization

MMFHandleList := nil;

finalization

FinalizeMMFHandleList;

end.
