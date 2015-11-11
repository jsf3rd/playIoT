// *******************************************************
//
// Judico DataSnap Common
//
// Copyright(c) 2014 ENBGroup.
//
// jsf3rd@enbgroup.co.kr
//
// Update 2014. 11. 09
//
// *******************************************************

unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DBXPlatform,
  FireDAC.Comp.DataSet, REST.JSON, XSuperObject, System.Generics.Collections
{$IF CompilerVersion  > 26} // upper XE5
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

type
  TDSCommon = class
  private

  public
    // Clear JSONObject Members
    class procedure ClearJSONObject(AObject: TJSONObject); overload;
    class procedure ClearJSONObject(AObject: TJSONArray); overload;

    // FDQuery to TSream
    class function DataSetToStream(ADataSet: TFDQuery): TStream; overload;

    // FDMemTable to TStream
    class function DataSetToStream(ADataSet: TFDMemTable): TStream; overload;

    // TStream to TBytesStream
    class function DSStreamToBytesStream(AValue: TStream): TBytesStream;

    // FDQuery's TStream to FDMemTable
    class procedure StreamToMemTable(AStream: TStream; ATable: TFDMemTable);
      deprecated 'Use StreamToFDDataSet';

    // FDQuery's TStream to TFDDataSet
    class procedure StreamToFDDataSet(AStream: TStream;
      ADataSet: TFDDataSet); static;
  end;

  TFDQueryHelper = class helper for TFDQuery
  public
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToRecord<T: Record >(AName: String = ''): T;
    function GetNameValue(AJSONValue: TJSONValue; AName: String): TJSONValue;
    function GetJSONObject(AObject: TJSONObject; AName: String): TJSONObject;
    function GetJSONArray(AObject: TJSONArray; AName: String): TJSONArray;
  end;

  TFDMemTableHelper = class helper for TFDMemTable
  public
    function Clone: TFDMemTable;
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);
  end;

implementation

uses JdcGlobal, JdcGlobal.ClassHelper;

{ TDSCommon }

class procedure TDSCommon.ClearJSONObject(AObject: TJSONArray);
begin
{$IF CompilerVersion  > 26} // upper XE5
  while AObject.Count > 0 do
{$ELSE}
  while AObject.Size > 0 do
{$ENDIF}
  begin
    AObject.Remove(0).Free;
  end;
end;

class function TDSCommon.DataSetToStream(ADataSet: TFDQuery): TStream;
begin
  result := TBytesStream.Create;
  ADataSet.Close;
  ADataSet.Open;
  ADataSet.FetchAll;
  ADataSet.SaveToStream(result, sfBinary);
  result.Position := 0;
  ADataSet.Close;
end;

class function TDSCommon.DataSetToStream(ADataSet: TFDMemTable): TStream;
begin
  result := TBytesStream.Create;
  ADataSet.SaveToStream(result, sfBinary);
  result.Position := 0;
end;

class procedure TDSCommon.ClearJSONObject(AObject: TJSONObject);
var
  Name: String;
begin
  if not Assigned(AObject) then
    Exit;

{$IF CompilerVersion  > 26} // upper XE5
  while AObject.Count > 0 do
  begin
    Name := AObject.Pairs[0].JsonString.Value;
{$ELSE}
  while AObject.Size > 0 do
  begin
    Name := AObject.Get(0).JsonString.Value;
{$ENDIF}
    AObject.RemovePair(Name).Free;
  end;
end;

class function TDSCommon.DSStreamToBytesStream(AValue: TStream): TBytesStream;
const
  BufferSize = 1024 * 16;
var
  Buffer: TBytes;
  BytesReadCount: integer;
begin
  result := TBytesStream.Create;

  SetLength(Buffer, BufferSize);
  repeat
    BytesReadCount := AValue.Read(Buffer[0], BufferSize);
    result.Write(Buffer[0], BytesReadCount);
  until BytesReadCount < BufferSize;

  result.Position := 0;
end;

class procedure TDSCommon.StreamToFDDataSet(AStream: TStream;
  ADataSet: TFDDataSet);
var
  BytesStream: TBytesStream;
begin
  BytesStream := DSStreamToBytesStream(AStream);
  try
    if BytesStream.Size = 0 then
      raise Exception.Create('Reveiced Null Stream, ' + ADataSet.Name);

    ADataSet.LoadFromStream(BytesStream, sfBinary);
  finally
    FreeAndNil(BytesStream);
  end;
end;

class procedure TDSCommon.StreamToMemTable(AStream: TStream;
  ATable: TFDMemTable);
begin
  StreamToFDDataSet(AStream, ATable);
end;

{ TFDQueryHelper }
procedure TFDQueryHelper.LoadFromDSStream(AStream: TStream);
begin
  TDSCommon.StreamToFDDataSet(AStream, Self);
end;

function TFDQueryHelper.ToStream: TStream;
begin
  result := TDSCommon.DataSetToStream(Self);
end;

function TFDQueryHelper.ToRecord<T>(AName: String): T;
var
  MyRecord: T;
  TempContainer: TJSONObject;
  ResultValue: TJSONValue;
begin
  TempContainer := TJSON.RecordToJsonObject<T>(MyRecord);
  try
    ResultValue := GetNameValue(TempContainer, AName);
    try
      result := TJSON.JsonToRecord<T>(ResultValue as TJSONObject);
    finally
      ResultValue.Free;
    end;
  finally
    TempContainer.Free;
  end;
end;

function TFDQueryHelper.GetJSONArray(AObject: TJSONArray; AName: String)
  : TJSONArray;
var
  I: integer;
  Key: string;
begin
  result := TJSONArray.Create;

{$IF CompilerVersion  > 26} // upper XE5
  for I := 0 to AObject.Count - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    result.AddElement(GetNameValue(AObject.Items[I], Key));
  end;
{$ELSE}
  for I := 0 to AObject.Size - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    result.AddElement(GetNameValue(AObject.Get(I), Key));
  end;
{$ENDIF}
end;

function TFDQueryHelper.GetJSONObject(AObject: TJSONObject; AName: String)
  : TJSONObject;
var
  MyElem: TJSONPair;
  Key: string;
begin
  result := TJSONObject.Create;
  for MyElem in AObject do
  begin
    if AName.IsEmpty then
      Key := MyElem.JsonString.Value
    else
      Key := AName + '_' + MyElem.JsonString.Value;

    result.AddPair(MyElem.JsonString.Value,
      GetNameValue(MyElem.JSONValue, Key));
  end;
end;

function TFDQueryHelper.GetNameValue(AJSONValue: TJSONValue; AName: String)
  : TJSONValue;
begin
  if AJSONValue is TJSONObject then
    result := GetJSONObject(AJSONValue as TJSONObject, AName)
  else if AJSONValue is TJSONArray then
    result := GetJSONArray(AJSONValue as TJSONArray, AName)
  else if AJSONValue is TJSONNumber then
    result := TJSONNumber.Create(Self.FieldByName(AName).AsFloat)
  else if AJSONValue is TJSONString then
    result := TJSONString.Create(Self.FieldByName(AName).AsString)
  else
    raise Exception.Create('Unknown Type - ' + AName);
end;

{ TFDMemTableHelper }

function TFDMemTableHelper.Clone: TFDMemTable;
var
  Stream: TStream;
  StoreItems: TFDStoreItems;
begin
  result := TFDMemTable.Create(Self.Owner);

  StoreItems := Self.ResourceOptions.StoreItems;
  Self.ResourceOptions.StoreItems := [siData, siDelta, siMeta];
  Stream := TMemoryStream.Create;
  try
    Self.SaveToStream(Stream);
    Stream.Position := 0;
    result.LoadFromStream(Stream);
  finally
    Self.ResourceOptions.StoreItems := StoreItems;
    FreeAndNil(Stream);
  end;
end;

procedure TFDMemTableHelper.LoadFromDSStream(AStream: TStream);
begin
  TDSCommon.StreamToFDDataSet(AStream, Self);
end;

function TFDMemTableHelper.ToStream: TStream;
begin
  result := TDSCommon.DataSetToStream(Self);
end;

end.
