// *******************************************************
//
// playIoT SharedMemory Reader
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
// Update 2016. 08. 24
//
// *******************************************************

unit JdcSharedMem.Reader;

interface

uses System.Classes, System.SysUtils, JdcSharedMem.Common, Winapi.Windows, JdcLogging;

type
  TJdcSharedMemReader = class
  private
    FDataInfo: PDataInfo;
    FDataList: Pointer;
    FCodeName: String;
    FCurrentSequence: Cardinal;
    function GetPointer: Pointer;
  public
    constructor Create(ACodeName: String);
    destructor Destroy; override;

    function GetFirstPointer: Pointer;
    function GetNextPointer: Pointer;
    function GetPreviousPointer: Pointer;
    function GetLastPointer: Pointer;

    property Sequence: Cardinal read FCurrentSequence;
  end;

implementation

uses SharedMMFMem;

{ TSharedMemoryReader }

constructor TJdcSharedMemReader.Create(ACodeName: String);
begin
  FCodeName := ACodeName;
  FCurrentSequence := 0;
  if not SharedOpenMem(FDataInfo, FCodeName + DATA_INFO, FILE_MAP_READ) then
    raise Exception.Create('[OpenMemError] ' + FCodeName + DATA_INFO);

  if not SharedOpenMem(FDataList, FCodeName + DATA_LIST, FILE_MAP_READ) then
    raise Exception.Create('[OpenMemError] ' + FCodeName + DATA_LIST);
end;

destructor TJdcSharedMemReader.Destroy;
begin
  SharedFreeMem(FDataInfo);
  SharedFreeMem(FDataList);
end;

function TJdcSharedMemReader.GetPointer: Pointer;
var
  PData: Pointer;
  ReadPosition: Cardinal;
  Sequence: Cardinal;
begin
  PData := FDataList;
  ReadPosition := FCurrentSequence and FDataInfo.Mask;
  PData := Ptr(UInt32(PData) + ReadPosition * FDataInfo.DataLength);

  // PrintDebug('[GetData] CodeName=%s,Seq=%u,Pos=%u',
  // [FCodeName, FCurrentSequence, ReadPosition]);

  CopyMemory(@Sequence, PData, SizeOf(Cardinal));

  if Sequence = 0 then
    Exit(nil);

  if FCurrentSequence <> Sequence then
  begin
    PrintDebug('[SeqError] CodeName=%s,CurSeq=%u,DataSeq=%u', [FCodeName, FCurrentSequence, Sequence]);
    Exit(nil);
  end;

  PData := Ptr(Integer(PData) + SizeOf(Cardinal));
  result := PData;
end;

function TJdcSharedMemReader.GetPreviousPointer: Pointer;
begin
  FCurrentSequence := FCurrentSequence - 1;
  result := GetPointer;

  if not Assigned(result) then
    FCurrentSequence := FDataInfo.LastSequence;
end;

function TJdcSharedMemReader.GetFirstPointer: Pointer;
begin
  if FDataInfo.LastSequence = 0 then
    FCurrentSequence := 0
  else if FDataInfo.LastSequence < FDataInfo.Mask then
    FCurrentSequence := 1
  else
    FCurrentSequence := FDataInfo.LastSequence - FDataInfo.Mask;

  result := GetPointer;
end;

function TJdcSharedMemReader.GetLastPointer: Pointer;
begin
  FCurrentSequence := FDataInfo.LastSequence;
  result := GetPointer;
end;

function TJdcSharedMemReader.GetNextPointer: Pointer;
begin
  if FCurrentSequence > FDataInfo.LastSequence then
    FCurrentSequence := FDataInfo.LastSequence;

  if FCurrentSequence = FDataInfo.LastSequence then
    Exit(nil);

  FCurrentSequence := FCurrentSequence + 1;
  result := GetPointer;

  if not Assigned(result) then
    FCurrentSequence := FDataInfo.LastSequence;
end;

end.
