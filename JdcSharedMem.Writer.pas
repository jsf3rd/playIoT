// *******************************************************
//
// playIoT SharedMemory Writer
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
// Update 2016. 08. 24
//
// *******************************************************

unit JdcSharedMem.Writer;

interface

uses System.Classes, System.SysUtils, Winapi.Windows, JdcSharedMem.Common;

type
  TJdcSharedMemWriter = class
  private
    FCodeName: String;
    FDataInfo: PDataInfo;
    FDataList: Pointer;
    function GetSequence: Cardinal;
  public
    constructor Create(ACodeName: String; ADataSize: Cardinal;
      AMaxCount: TDataCount = dc256);
    destructor Destroy; override;

    function GetDataInfo: TDataInfo;
    procedure PutData(AData: TStream);
    property Sequence: Cardinal read GetSequence;
  end;

implementation

uses SharedMMFMem, JdcGlobal;

{ TJdcSharedMemWriter }

constructor TJdcSharedMemWriter.Create(ACodeName: String; ADataSize: Cardinal;
  AMaxCount: TDataCount);
begin
  FCodeName := ACodeName;
  FDataInfo := SharedAllocMem(FCodeName + DATA_INFO, SizeOf(TDataInfo));
  PrintDebug('[SharedMemAlloc] Name=%s,Addr=%p,Size=%d',
    [FCodeName + DATA_INFO, FDataInfo, SizeOf(TDataInfo)]);

  FDataInfo.MaxCount := UInt32(AMaxCount);
  FDataInfo.Mask := UInt32(AMaxCount) - 1;
  FDataInfo.LastSequence := 0;
  FDataInfo.DataLength := SizeOf(Cardinal) + ADataSize;

  FDataList := SharedAllocMem(FCodeName + DATA_LIST, FDataInfo.DataLength *
    FDataInfo.MaxCount);

  PrintDebug('[SharedMemAlloc] Name=%s,Addr=%p,Size=%d,SaveCount=%d',
    [FCodeName + DATA_LIST, FDataList, FDataInfo.DataLength,
    FDataInfo.MaxCount]);
end;

destructor TJdcSharedMemWriter.Destroy;
begin
  if SharedFreeMem(FDataInfo) and SharedFreeMem(FDataList) then
    PrintDebug('[SharedMemDealloc] Name=%s', [FCodeName]);
end;

function TJdcSharedMemWriter.GetDataInfo: TDataInfo;
begin
  result := FDataInfo^;
end;

function TJdcSharedMemWriter.GetSequence: Cardinal;
begin
  result := FDataInfo.LastSequence;
end;

procedure TJdcSharedMemWriter.PutData(AData: TStream);
var
  PData: Pointer;
  Position: Cardinal;
begin
  if FDataInfo.DataLength <> AData.Size + SizeOf(Cardinal) then
    raise Exception.Create('[DataSize] CodeName=' + FCodeName + ',Size=' +
      AData.Size.ToString);

  FDataInfo.LastSequence := FDataInfo.LastSequence + 1;
  Position := FDataInfo.LastSequence and FDataInfo.Mask;

  PData := FDataList;
  PData := Ptr(UInt32(PData) + (Position * FDataInfo.DataLength));
  // PrintDebug('[PutData] CodeName=%s,Sequence=%u,Positon=%u,Address=%p',
  // [FCodeName, FDataInfo.LastSequence, Position, PData]);

  CopyMemory(PData, @FDataInfo.LastSequence, SizeOf(Cardinal));
  PData := Ptr(Integer(PData) + SizeOf(Cardinal));
  AData.Position := 0;
  AData.Read(PData^, AData.Size);
end;

end.
