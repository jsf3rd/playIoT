// *******************************************************
//
// playIoT SharedMemory Common
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
// Update 2016. 08. 24
//
// *******************************************************

unit JdcSharedMem.Common;

interface

uses System.Classes, System.SysUtils, Winapi.Windows;

type
  TDataCount = (dc256 = 256, dc512 = 512, dc1024 = 1024, dc2048 = 2048);

  TDataInfo = record
    DataLength: Cardinal;
    MaxCount: Cardinal;
    Mask: Cardinal;
    LastSequence: Cardinal;
  end;

  PDataInfo = ^TDataInfo;

  TJdcSharedMemCommon = class
  public
    class function OpenMem(AName: String; DesiredAccess: Cardinal): Pointer;
  end;

const
  DATA_INFO = '_DataInfo';
  DATA_LIST = '_DataList';

implementation

uses SharedMMFMem;

{ TSharedMemCommon }

class function TJdcSharedMemCommon.OpenMem(AName: String;
  DesiredAccess: Cardinal): Pointer;
begin
  if not SharedOpenMem(result, AName, DesiredAccess) then
    raise Exception.Create('Open memory error, ' + AName);
end;

end.
