// *******************************************************
//
// playIoT DataSnap Common
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@nate.com
//
// Update 2016. 04. 22
//
// *******************************************************

unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DBXPlatform,
  FireDAC.Comp.DataSet, FireDAC.Stan.Param, REST.JSON, XSuperObject,
  System.Generics.Collections, System.DateUtils, Data.DB, Data.SqlTimSt,
  JdcGlobal
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
    class procedure ClearJSONObject(AValue: TJSONObject); overload;
    class procedure ClearJSONObject(AValue: TJSONArray); overload;

    // TStream to TBytesStream
    class function DSStreamToBytesStream(AStream: TStream): TBytesStream;

    // FDQuery to TSream
    class function DataSetToStream(AQuery: TFDQuery): TStream; overload;
      deprecated 'TFDQueryHelper.ToStream';

    // FDMemTable to TStream
    class function DataSetToStream(AMemTab: TFDMemTable): TStream; overload;
      deprecated 'TFDMemTableHelper.ToStream';

    // DataSnap TStream to TFDDataSet
    class procedure DSStreamToFDDataSet(AStream: TStream; ADataSet: TFDDataSet);
      static; deprecated 'TFDDataSetHelper.LoadFromDSStream';
  end;

  TFDDataSetHelper = class helper for TFDDataSet
  private
    function FieldToJSONVlaue(AField: TField): TJSONValue;
    function GetNameValue(AValue: TJSONValue; AName: String): TJSONValue;
    function GetJSONObject(AValue: TJSONObject; AName: String): TJSONObject;
    function GetJSONArray(AValue: TJSONArray; AName: String): TJSONArray;
  public
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;
  end;

  TFDQueryHelper = class helper for TFDQuery
  private
    procedure ParamByJsonValue(AParam: TFDParam; AValue: TJSONValue;
      AProc: TLogProc = nil); overload;
    procedure ParamByJsonValue(AValue: TJSONValue; AName: String;
      AProc: TLogProc = nil); overload;
    procedure ParamByJSONArray(AValue: TJSONArray; AName: String;
      AProc: TLogProc = nil);
  public
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;

    procedure ParamByJSONObject(AObject: TJSONObject; AProc: TLogProc = nil);
  end;

  TFDMemTableHelper = class helper for TFDMemTable
  public
    function Clone: TFDMemTable;
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;
  end;

implementation

uses JdcGlobal.ClassHelper;

{ TDSCommon }

class procedure TDSCommon.ClearJSONObject(AValue: TJSONArray);
begin
{$IF CompilerVersion  > 26} // upper XE5
  while AValue.Count > 0 do
{$ELSE}
  while AObject.Size > 0 do
{$ENDIF}
  begin
    AValue.Remove(0).Free;
  end;
end;

class function TDSCommon.DataSetToStream(AQuery: TFDQuery): TStream;
begin
  result := AQuery.ToStream;
end;

class function TDSCommon.DataSetToStream(AMemTab: TFDMemTable): TStream;
begin
  result := AMemTab.ToStream;
end;

class procedure TDSCommon.ClearJSONObject(AValue: TJSONObject);
var
  Name: String;
begin
  if not Assigned(AValue) then
    Exit;

{$IF CompilerVersion  > 26} // upper XE5
  while AValue.Count > 0 do
  begin
    Name := AValue.Pairs[0].JsonString.Value;
{$ELSE}
  while AObject.Size > 0 do
  begin
    Name := AObject.Get(0).JsonString.Value;
{$ENDIF}
    AValue.RemovePair(Name).Free;
  end;
end;

class function TDSCommon.DSStreamToBytesStream(AStream: TStream): TBytesStream;
const
  BufferSize = 1024 * 16;
var
  Buffer: TBytes;
  BytesReadCount: integer;
begin
  result := TBytesStream.Create;

  SetLength(Buffer, BufferSize);
  repeat
    BytesReadCount := AStream.Read(Buffer[0], BufferSize);
    result.Write(Buffer[0], BytesReadCount);
  until BytesReadCount < BufferSize;

  result.Position := 0;
end;

class procedure TDSCommon.DSStreamToFDDataSet(AStream: TStream;
  ADataSet: TFDDataSet);
begin
  ADataSet.LoadFromDSStream(AStream);
end;

{ TFDQueryHelper }
procedure TFDQueryHelper.LoadFromDSStream(AStream: TStream);
begin
  TFDDataSet(Self).LoadFromDSStream(AStream);
end;

procedure TFDQueryHelper.ParamByJsonValue(AParam: TFDParam; AValue: TJSONValue;
  AProc: TLogProc);
var
  Msg: String;
begin
  if not Assigned(AParam) then
    Exit;

  Msg := Format('DataSet=%s,ParamName=%s,DataType=%d,Value=%s',
    [Self.Name, AParam.Name, integer(AParam.DataType), AValue.Value]);

  if Assigned(AProc) then
    AProc(mtDebug, 'SQLParameter', Msg)
  else
    PrintDebug(Msg);

  case AParam.DataType of
    ftUnknown:
      raise Exception.Create(Format('DataSet=%s,ParamName=%s,Unknown DataType',
        [Self.Name, AParam.Name]));
    ftString, ftWideString:
      AParam.AsString := (AValue as TJSONString).Value;
    ftSmallint, ftInteger, ftWord, ftShortint:
      AParam.AsInteger := (AValue as TJSONNumber).AsInt;
    ftBoolean:
      AParam.AsBoolean := (AValue as TJSONBool).AsBoolean;
    ftFloat:
      AParam.AsFloat := (AValue as TJSONNumber).AsDouble;
    ftCurrency:
      AParam.AsCurrency := (AValue as TJSONNumber).AsDouble;
    ftDate:
      AParam.AsDate := ISO8601ToDate((AValue as TJSONString).Value);
    ftTime:
      AParam.AsTime := ISO8601ToDate((AValue as TJSONString).Value);
    ftDateTime:
      AParam.AsDateTime := ISO8601ToDate((AValue as TJSONString).Value);
    ftTimeStamp:
      AParam.AsSQLTimeStamp := DateTimeToSQLTimeStamp
        (ISO8601ToDate((AValue as TJSONString).Value));
    ftLargeint:
      AParam.AsLargeInt := (AValue as TJSONNumber).AsInt64;
    ftGuid:
      AParam.AsGUID := TGuid.Create((AValue as TJSONString).Value);
    ftExtended:
      AParam.AsExtended := (AValue as TJSONNumber).AsDouble;
    ftLongWord:
      AParam.AsLongword := (AValue as TJSONNumber).AsInt;
    ftSingle:
      AParam.AsSingle := (AValue as TJSONNumber).AsDouble;
  else
    raise Exception.Create
      (Format('DataSet=%s,ParamName=%s,UnsurportDataType=%s',
      [Self.Name, AParam.Name, AParam.DataTypeName]));
  end;

end;

procedure TFDQueryHelper.ParamByJSONArray(AValue: TJSONArray; AName: String;
  AProc: TLogProc);
var
  MyElem: TJSONValue;
  Index: integer;
begin
  Index := 1;
  for MyElem in AValue do
  begin
    ParamByJsonValue(MyElem, AName + '_' + Index.ToString, AProc);
    Inc(Index);
  end;
end;

procedure TFDQueryHelper.ParamByJSONObject(AObject: TJSONObject;
  AProc: TLogProc);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    ParamByJsonValue(MyElem.JSONValue, MyElem.JsonString.Value, AProc);
end;

procedure TFDQueryHelper.ParamByJsonValue(AValue: TJSONValue; AName: String;
  AProc: TLogProc);
begin
  if AValue is TJSONObject then
    ParamByJSONObject(AValue as TJSONObject, AProc)
  else if AValue is TJSONArray then
  begin
    ParamByJSONArray(AValue as TJSONArray, AName, AProc)
  end
  else
    ParamByJsonValue(Self.FindParam(AName), AValue, AProc);
end;

function TFDQueryHelper.ToStream: TStream;
begin
  result := TBytesStream.Create;
  Self.Close;
  Self.Open;
  Self.FetchAll;
  Self.SaveToStream(result, sfBinary);
  result.Position := 0;
  Self.Close;
end;

function TFDQueryHelper.ToJSON: TJSONObject;
begin
  result := TFDDataSet(Self).ToJSON;
end;

function TFDQueryHelper.ToRecord<T>(AName: String): T;
begin
  result := TFDDataSet(Self).ToRecord<T>(AName);
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
  TFDDataSet(Self).LoadFromDSStream(AStream);
end;

function TFDMemTableHelper.ToJSON: TJSONObject;
begin
  result := TFDDataSet(Self).ToJSON;
end;

function TFDMemTableHelper.ToRecord<T>(AName: String): T;
begin
  result := TFDDataSet(Self).ToRecord<T>(AName);
end;

function TFDMemTableHelper.ToStream: TStream;
begin
  result := TBytesStream.Create;
  Self.SaveToStream(result, sfBinary);
  result.Position := 0;
end;

{ TFDDataSetHelper }

function TFDDataSetHelper.FieldToJSONVlaue(AField: TField): TJSONValue;
begin
  if not Assigned(AField) then
    Exit(TJSONString.Create);

  case AField.DataType of
    ftUnknown:
      raise Exception.Create(Format('DataSet=%s,FieldName=%s,Unknown DataType',
        [Self.Name, AField.FieldName]));
    ftString, ftWideString:
      result := TJSONString.Create(AField.AsString);
    ftSmallint, ftInteger, ftWord, ftShortint:
      result := TJSONNumber.Create(AField.AsInteger);
    ftBoolean:
      result := TJSONBool.Create(AField.AsBoolean);
    ftFloat:
      result := TJSONNumber.Create(AField.AsFloat);
    ftCurrency:
      result := TJSONNumber.Create(AField.AsCurrency);
    ftDate, ftTime, ftDateTime:
      result := TJSONString.Create(AField.AsDateTime.ToISO8601);
    ftTimeStamp:
      result := TJSONString.Create(SQLTimeStampToDateTime(AField.AsSQLTimeStamp)
        .ToISO8601);
    ftLargeint:
      result := TJSONNumber.Create(AField.AsLargeInt);
    ftGuid:
      result := TJSONString.Create(TGuidField(AField).AsGUID.ToString);
    ftExtended:
      result := TJSONNumber.Create(AField.AsExtended);
    ftLongWord:
      result := TJSONNumber.Create(AField.AsLongword);
    ftSingle:
      result := TJSONNumber.Create(AField.AsSingle);
  else
    raise Exception.Create
      (Format('DataSet=%s,FieldName=%s,UnsurportDataType=TFieldType(%d)',
      [Self.Name, AField.FieldName, integer(AField.DataType)]));
  end;

  // PrintDebug('DataSet=%s,FieldName=%s,DataType=%d,Value=%s',
  // [Self.Name, AField.FieldName, integer(AField.DataType), result.Value]);
end;

function TFDDataSetHelper.GetJSONArray(AValue: TJSONArray; AName: String)
  : TJSONArray;
var
  I: integer;
  Key: string;
begin
  result := TJSONArray.Create;

{$IF CompilerVersion  > 26} // upper XE5
  for I := 0 to AValue.Count - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    result.AddElement(GetNameValue(AValue.Items[I], Key));
  end;
{$ELSE}
  for I := 0 to AObject.Size - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    result.AddElement(GetNameValue(AObject.Get(I), Key));
  end;
{$ENDIF}
end;

function TFDDataSetHelper.GetJSONObject(AValue: TJSONObject; AName: String)
  : TJSONObject;
var
  MyElem: TJSONPair;
  Key: string;
begin
  result := TJSONObject.Create;
  for MyElem in AValue do
  begin
    Key := AName + '_';
    if AName.IsEmpty then
      Key := '';

    Key := Key + MyElem.JsonString.Value;
    result.AddPair(MyElem.JsonString.Value,
      GetNameValue(MyElem.JSONValue, Key));
  end;
end;

function TFDDataSetHelper.GetNameValue(AValue: TJSONValue; AName: String)
  : TJSONValue;
begin
  if AValue is TJSONObject then
    result := GetJSONObject(AValue as TJSONObject, AName)
  else if AValue is TJSONArray then
    result := GetJSONArray(AValue as TJSONArray, AName)
  else
    result := FieldToJSONVlaue(Self.Fields.FindField(AName));
end;

procedure TFDDataSetHelper.LoadFromDSStream(AStream: TStream);
var
  BytesStream: TBytesStream;
begin
  BytesStream := TDSCommon.DSStreamToBytesStream(AStream);
  try
    if BytesStream.Size = 0 then
      raise Exception.Create('Reveiced Null Stream, ' + Self.Name);

    Self.LoadFromStream(BytesStream, sfBinary);
  finally
    FreeAndNil(BytesStream);
  end;
end;

function TFDDataSetHelper.ToJSON: TJSONObject;
var
  MyField: TField;
  JSONValue: TJSONValue;
begin
  result := TJSONObject.Create;
  for MyField in Self.Fields do
  begin
    JSONValue := FieldToJSONVlaue(MyField);
    result.AddPair(MyField.FieldName, JSONValue);
  end;
end;

function TFDDataSetHelper.ToRecord<T>(AName: String): T;
var
  MyRecord: T;
  TempContainer, ResultValue: TJSONObject;
begin
  FillChar(MyRecord, SizeOf(T), 0);
  TempContainer := REST.JSON.TJSON.RecordToJsonObject<T>(MyRecord);
  try
    ResultValue := GetJSONObject(TempContainer, AName);
    try
      result := REST.JSON.TJSON.JsonToRecord<T>(ResultValue);
    finally
      ResultValue.Free;
    end;
  finally
    TempContainer.Free;
  end;
end;

end.
