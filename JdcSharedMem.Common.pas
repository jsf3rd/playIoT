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
  TDataCount = (dc1 = 1, dc128 = 128, dc256 = 256, dc512 = 512, dc1024 = 1024, dc2048 = 2048);

  TDataInfo = packed record
    DataLength: Cardinal;
    MaxCount: Cardinal;
    Mask: Cardinal;
    LastSequence: Cardinal;
  end;

  PDataInfo = ^TDataInfo;

const
  GLOBAL_PREFIX = 'Global\';

  DATA_INFO = '_DataInfo';
  DATA_LIST = '_DataList';

implementation

end.
