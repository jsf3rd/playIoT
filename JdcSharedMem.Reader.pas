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

uses System.Classes, System.SysUtils, JdcSharedMem.Common, Winapi.Windows;

type
  TJdcSharedMemReader = class
  private
    FDataInfo: PDataInfo;
    FDataList: Pointer;
    FCodeName: String;
    FCurrentSequence: Cardinal;
    function GetData: TStream;
  public
    constructor Create(ACodeName: String);
    function GetFirstData: TStream;
    function GetNextData: TStream;
    function GetLastData: TStream;

    property Sequence: Cardinal read FCurrentSequence;
  end;

implementation

uses SharedMMFMem, JdcGlobal;

{ TSharedMemoryReader }

constructor TJdcSharedMemReader.Create(ACodeName: String);
begin
  FCodeName := ACodeName;
  FCurrentSequence := 0;
  if SharedOpenMem(FDataInfo, FCodeName + DATA_INFO, FILE_MAP_READ) then
    PrintDebug('[SharedMemOpened] Name=%s,Addr=%p',
      [FCodeName + DATA_INFO, FDataInfo])
  else
    raise Exception.Create('[OpenMemError] ' + FCodeName + DATA_INFO);

  if SharedOpenMem(FDataList, FCodeName + DATA_LIST, FILE_MAP_READ) then
    PrintDebug('[SharedMemOpened] Name=%s,Addr=%p',
      [FCodeName + DATA_LIST, FDataList])
  else
    raise Exception.Create('[OpenMemError] ' + FCodeName + DATA_LIST);
end;

function TJdcSharedMemReader.GetData: TStream;
var
  PData: Pointer;
  ReadPosition: Cardinal;
  Sequence: Cardinal;
  MyVersion: Byte;
begin
  PData := FDataList;
  ReadPosition := FCurrentSequence and FDataInfo.Mask;
  PData := Ptr(UInt32(PData) + ReadPosition * FDataInfo.DataLength);
  // PrintDebug('[GetData] CodeName=%s,Seq=%u,Pos=%u,Addr=%p',
  // [FCodeName, FCurrentSequence, ReadPosition, PData]);

  CopyMemory(@Sequence, PData, SizeOf(Cardinal));

  if Sequence = 0 then
    Exit(nil);

  if FCurrentSequence <> Sequence then
  begin
    PrintDebug('[SeqError] CodeName=%s,CurSeq=%u,DataSeq=%u',
      [FCodeName, FCurrentSequence, Sequence]);
    Exit(nil);
  end;

  PData := Ptr(Integer(PData) + SizeOf(Cardinal));
  CopyMemory(@MyVersion, PData, SizeOf(Byte));

  if MyVersion = 0 then
    Exit(nil);

  result := TMemoryStream.Create;
  result.Write(PData, FDataInfo.DataLength - SizeOf(Cardinal));
end;

function TJdcSharedMemReader.GetFirstData: TStream;
begin
  if FDataInfo.LastSequence = 0 then
    FCurrentSequence := 0
  else if FDataInfo.LastSequence < FDataInfo.Mask then
    FCurrentSequence := 1
  else
    FCurrentSequence := FDataInfo.LastSequence - FDataInfo.Mask;

  result := GetData;
end;

function TJdcSharedMemReader.GetLastData: TStream;
begin
  FCurrentSequence := FDataInfo.LastSequence;
  result := GetData;
end;

function TJdcSharedMemReader.GetNextData: TStream;
begin
  if FCurrentSequence > FDataInfo.LastSequence then
    FCurrentSequence := FDataInfo.LastSequence;

  if FCurrentSequence = FDataInfo.LastSequence then
    Exit(nil);

  FCurrentSequence := FCurrentSequence + 1;
  result := GetData;

  if not Assigned(result) then
    FCurrentSequence := FDataInfo.LastSequence;
end;

end.
