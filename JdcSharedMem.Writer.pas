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

uses System.Classes, System.SysUtils, Winapi.Windows, JdcSharedMem.Common, JdcLogging;

type
  TJdcSharedMemWriter = class
  private
    FCodeName: String;
    FDataInfo: PDataInfo;
    FDataList: Pointer;
    function GetSequence: UInt64;
  public
    constructor Create(ACodeName: String; ADataSize: Cardinal; AMaxCount: TDataCount = dc256);
    destructor Destroy; override;

    function GetDataInfo: TDataInfo;
    procedure PutData(AData: TStream);
    property Sequence: UInt64 read GetSequence;
  end;

implementation

uses SharedMMFMem, JdcGlobal;

{ TJdcSharedMemWriter }

constructor TJdcSharedMemWriter.Create(ACodeName: String; ADataSize: Cardinal;
  AMaxCount: TDataCount);
begin
  FCodeName := ACodeName;
  FDataInfo := SharedAllocMem(FCodeName + DATA_INFO, SizeOf(TDataInfo));

  FDataInfo.MaxCount := UInt32(AMaxCount);
  FDataInfo.Mask := UInt32(AMaxCount) - 1;
  FDataInfo.LastSequence := 0;
  FDataInfo.DataLength := SizeOf(UInt64) + ADataSize;

  FDataList := SharedAllocMem(FCodeName + DATA_LIST, FDataInfo.DataLength * FDataInfo.MaxCount);
end;

destructor TJdcSharedMemWriter.Destroy;
begin
  SharedFreeMem(FDataInfo);
  SharedFreeMem(FDataList);
end;

function TJdcSharedMemWriter.GetDataInfo: TDataInfo;
begin
  result := FDataInfo^;
end;

function TJdcSharedMemWriter.GetSequence: UInt64;
begin
  result := FDataInfo.LastSequence;
end;

procedure TJdcSharedMemWriter.PutData(AData: TStream);
var
  PData: Pointer;
  Position: Cardinal;
begin
  if FDataInfo.DataLength <> AData.Size + SizeOf(UInt64) then
    raise Exception.Create(Format('[DataSize] CodeName=%s,Size=%d,Expected=%d',
      [FCodeName, AData.Size, FDataInfo.DataLength]));

  FDataInfo.LastSequence := FDataInfo.LastSequence + 1;
  Position := FDataInfo.LastSequence and FDataInfo.Mask;

  PData := FDataList;
  PData := Ptr(NativeUInt(PData) + (Position * FDataInfo.DataLength));
  // PrintDebug('[PutData] CodeName=%s,Sequence=%u,Positon=%u', [FCodeName, FDataInfo.LastSequence, Position]);

  CopyMemory(PData, @FDataInfo.LastSequence, SizeOf(UInt64));
  PData := Ptr(NativeUInt(PData) + SizeOf(UInt64));
  AData.Position := 0;
  AData.Read(PData^, AData.Size);
end;

end.
