// *******************************************************
//
// playIoT DataSnap Common
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
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

    // TDBXStream to TBytesStream
    class function DSStreamToBytesStream(AStream: TStream): TBytesStream;
      deprecated 'DBXStreamToMemoryStream';

    // TDBXStream to TMemoryStream
    class function DBXStreamToMemoryStream(AStream: TStream): TMemoryStream;

    // FDQuery to TSream
    class function DataSetToStream(AQuery: TFDQuery): TStream; overload;
      deprecated 'TFDQueryHelper.ToStream';

    // FDMemTable to TStream
    class function DataSetToStream(AMemTab: TFDMemTable): TStream; overload;
      deprecated 'TFDMemTableHelper.ToStream';

    // DataSnap TStream to TFDDataSet
    class procedure DSStreamToFDDataSet(AStream: TStream; ADataSet: TFDDataSet); static;
      deprecated 'TFDDataSetHelper.LoadFromDSStream';

    class procedure AddJSONValue<T: record >(var AObject: TJSONObject; ARecord: T;
      APreFix: string = '');
    class procedure AddJSONArray<T: record >(var AObject: TJSONObject; ARecord: T);
  end;

  TFDDataSetHelper = class helper for TFDDataSet
  private
    function FieldToJSONVlaue(AField: TField): TJSONValue;
    function GetNameValue(AValue: TJSONValue; AName: String): TJSONValue;
    function GetJSONObject(AValue: TJSONObject; AName: String): TJSONObject;
    function GetJSONArray(AValue: TJSONArray; AName: String): TJSONArray;

    procedure FieldByJsonValue(AField: TField; AValue: TJSONValue;
      AProc: TLogProc = nil); overload;
    procedure FieldByJsonValue(AValue: TJSONValue; AName: String;
      AProc: TLogProc = nil); overload;
    procedure FieldByJSONArray(AValue: TJSONArray; AName: String; AProc: TLogProc = nil);
  public
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;

    procedure FieldByJSONObject(AObject: TJSONObject; AProc: TLogProc = nil); overload;
    procedure FieldByJSONObject(AJSON: String; AProc: TLogProc = nil); overload;
  end;

  TFDQueryHelper = class helper for TFDQuery
  private
    procedure ParamByJsonValue(AParam: TFDParam; AValue: TJSONValue;
      AProc: TLogProc = nil); overload;
    procedure ParamByJsonValue(AValue: TJSONValue; AName: String;
      AProc: TLogProc = nil); overload;
    procedure ParamByJSONArray(AValue: TJSONArray; AName: String; AProc: TLogProc = nil);
  public
    function Clone: TFDQuery;
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;

    procedure ParamByJSONObject(AObject: TJSONObject; AProc: TLogProc = nil);
    procedure FieldByJSONObject(AObject: TJSONObject; AProc: TLogProc = nil); overload;
    procedure FieldByJSONObject(AJSON: String; AProc: TLogProc = nil); overload;
  end;

  TFDMemTableHelper = class helper for TFDMemTable
  public
    function Clone: TFDMemTable;
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;
    procedure FieldByJSONObject(AObject: TJSONObject; AProc: TLogProc = nil); overload;
    procedure FieldByJSONObject(AJSON: String; AProc: TLogProc = nil); overload;
  end;

function GetFieldTypeName(AType: TFieldType): string;

const
  FieldTypeName: array [0 .. 51] of string = //
    ('Unknown', 'String', 'Smallint', 'Integer', 'Word', // 0..4
    'Boolean', 'Float', 'Currency', 'BCD', 'Date', 'Time', 'DateTime', // 5..11
    'Bytes', 'VarBytes', 'AutoInc', 'Blob', 'Memo', 'Graphic', 'FmtMemo', // 12..18
    'ParadoxOle', 'DBaseOle', 'TypedBinary', 'Cursor', 'FixedChar', 'WideString', // 19..24
    'Largeint', 'ADT', 'Array', 'Reference', 'DataSet', 'OraBlob', 'OraClob', // 25..31
    'Variant', 'Interface', 'IDispatch', 'Guid', 'TimeStamp', 'FMTBcd', // 32..37
    'FixedWideChar', 'WideMemo', 'OraTimeStamp', 'OraInterval', // 38..41
    'LongWord', 'Shortint', 'Byte', 'Extended', 'Connection', 'Params', 'Stream', // 42..48
    'TimeStampOffset', 'Object', 'Single'); // 49..51

implementation

uses JdcGlobal.ClassHelper;

function GetFieldTypeName(AType: TFieldType): string;
begin
  if Integer(AType) > Length(FieldTypeName) - 1 then
    result := Integer(AType).ToString
  else
    result := FieldTypeName[Integer(AType)];
end;

{ TDSCommon }

class procedure TDSCommon.AddJSONArray<T>(var AObject: TJSONObject; ARecord: T);
var
  tmp: TJSONObject;
  MyPair: TJSONPair;
  ArrayTemp: TStringList;
  MyElem: TJSONValue;
begin
  tmp := REST.JSON.TJson.RecordToJsonObject<T>(ARecord);
  for MyPair in tmp do
  begin
    if MyPair.JsonValue is TJSONObject then
      Continue;

    if MyPair.JsonValue is TJSONArray then
    begin
      ArrayTemp := TStringList.Create;
      for MyElem in TJSONArray(MyPair.JsonValue) do
      begin
        if MyElem is TJSONObject then
          Continue;

        ArrayTemp.Add(MyElem.Value);
      end;
      AObject.AddPair(MyPair.JsonString.Value, ArrayTemp.CommaText);
      ArrayTemp.Free;
    end
    else if MyPair.JsonValue is TJSONString then
      AObject.AddPair(MyPair.Clone as TJSONPair);
  end;
  tmp.Free;
end;

class procedure TDSCommon.AddJSONValue<T>(var AObject: TJSONObject; ARecord: T;
  APreFix: string);
var
  tmp: TJSONObject;
  MyPair: TJSONPair;
  NewKey: string;
begin
  tmp := REST.JSON.TJson.RecordToJsonObject<T>(ARecord);

  if not APreFix.IsEmpty then
  begin
    for MyPair in tmp do
    begin
      NewKey := APreFix + MyPair.JsonString.Value;
      MyPair.JsonString.Free;
      MyPair.JsonString := TJSONString.Create(NewKey);
    end;
  end;

  try
    for MyPair in tmp do
    begin
      AObject.AddPair(MyPair.Clone as TJSONPair);
    end;
  finally
    tmp.Free;
  end;

end;

class procedure TDSCommon.ClearJSONObject(AValue: TJSONArray);
begin
{$IF CompilerVersion  > 26} // upper XE5
  while AValue.Count > 0 do
{$ELSE}
  while AValue.Size > 0 do
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
  while AValue.Size > 0 do
  begin
    Name := AValue.Get(0).JsonString.Value;
{$ENDIF}
    AValue.RemovePair(Name).Free;
  end;
end;

class function TDSCommon.DSStreamToBytesStream(AStream: TStream): TBytesStream;
const
  BufferSize = 1024 * 32;
var
  Buffer: TBytes;
  BytesReadCount: Integer;
begin
  result := TBytesStream.Create;

  SetLength(Buffer, BufferSize);
  repeat
    BytesReadCount := AStream.Read(Buffer[0], BufferSize);
    result.Write(Buffer[0], BytesReadCount);
  until BytesReadCount < BufferSize;

  result.Position := 0;
end;

class function TDSCommon.DBXStreamToMemoryStream(AStream: TStream): TMemoryStream;
const
  BufferSize = 1024 * 32;
var
  Buffer: TBytes;
  ReadCount: Integer;
begin
  result := TMemoryStream.Create;
  try
    SetLength(Buffer, BufferSize);
    repeat
      ReadCount := AStream.Read(Buffer[0], BufferSize);
      result.Write(Buffer[0], ReadCount);
    until ReadCount < BufferSize;
    result.Position := 0;
  except
    result.Free;
    raise;
  end;
end;

class procedure TDSCommon.DSStreamToFDDataSet(AStream: TStream; ADataSet: TFDDataSet);
begin
  ADataSet.LoadFromDSStream(AStream);
end;

{ TFDQueryHelper }
function TFDQueryHelper.Clone: TFDQuery;
var
  I: Integer;
begin
  result := TFDQuery.Create(Self);
  result.OnExecuteError := Self.OnExecuteError;
  result.OnReconcileError := Self.OnReconcileError;
  result.SQL.Text := Self.SQL.Text;
  result.Name := Self.Name + '_' + Format('%0.5d', [Random(100000)]);
  for I := 0 to Self.ParamCount - 1 do
    result.Params.Items[I].DataType := Self.Params.Items[I].DataType;
end;

procedure TFDQueryHelper.FieldByJSONObject(AObject: TJSONObject; AProc: TLogProc);
begin
  TFDDataSet(Self).FieldByJSONObject(AObject, AProc);
end;

procedure TFDQueryHelper.FieldByJSONObject(AJSON: String; AProc: TLogProc);
begin
  TFDDataSet(Self).FieldByJSONObject(AJSON, AProc);
end;

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

  Msg := Format('DataSet=%s,ParamName=%s,DataType=%s,Value=%s',
    [Self.Name, AParam.Name, GetFieldTypeName(AParam.DataType), AValue.Value]);

  if Assigned(AProc) then
    AProc(msDebug, 'SQLParameter', Msg);
  PrintDebug(Msg);

  case AParam.DataType of
    ftUnknown:
      raise Exception.Create(Format('DataSet=%s,ParamName=%s,Unknown DataType',
        [Self.Name, AParam.Name]));

    ftString, ftWideString:
      if Self.Params.ArraySize = 1 then
        AParam.AsString := (AValue as TJSONString).Value
      else
        AParam.AsStrings[Self.Tag] := (AValue as TJSONString).Value;

    ftSmallint, ftInteger, ftWord, ftShortint:
      if Self.Params.ArraySize = 1 then
        AParam.AsInteger := (AValue as TJSONNumber).AsInt
      else
        AParam.AsIntegers[Self.Tag] := (AValue as TJSONNumber).AsInt;

{$IF CompilerVersion  > 26} // upper XE5
    ftBoolean:
      if Self.Params.ArraySize = 1 then
        AParam.AsBoolean := (AValue as TJSONBool).AsBoolean
      else
        AParam.AsBooleans[Self.Tag] := (AValue as TJSONBool).AsBoolean;

    ftDate:
      if Self.Params.ArraySize = 1 then
        AParam.AsDate := StrToDate((AValue as TJSONString).Value, DefaultFormatSettings)
      else
        AParam.AsDates[Self.Tag] := StrToDate((AValue as TJSONString).Value,
          DefaultFormatSettings);

    ftTime:
      if Self.Params.ArraySize = 1 then
        AParam.AsTime := StrToTime((AValue as TJSONString).Value, DefaultFormatSettings)
      else
        AParam.AsTimes[Self.Tag] := StrToTime((AValue as TJSONString).Value,
          DefaultFormatSettings);

    ftDateTime:
      if Self.Params.ArraySize = 1 then
        AParam.AsDateTime := ISO8601ToDate((AValue as TJSONString).Value)
      else
        AParam.AsDateTimes[Self.Tag] := ISO8601ToDate((AValue as TJSONString).Value);

    ftTimeStamp:
      if Self.Params.ArraySize = 1 then
        AParam.AsSQLTimeStamp := DateTimeToSQLTimeStamp
          (ISO8601ToDate((AValue as TJSONString).Value))
      else
        AParam.AsSQLTimeStamps[Self.Tag] :=
          DateTimeToSQLTimeStamp(ISO8601ToDate((AValue as TJSONString).Value));
{$ELSE}
    ftBoolean:
      if Self.Params.ArraySize = 1 then
        AParam.AsBoolean := AValue is TJSONTrue
      else
        AParam.AsBooleans[Self.Tag] := AValue is TJSONTrue;

    ftDate, ftTime, ftDateTime, ftTimeStamp:
      if Self.Params.ArraySize = 1 then
        AParam.Value := AValue.Value
      else
        AParam.Values[Self.Tag] := AValue.Value;
{$ENDIF}
    ftFloat:
      if Self.Params.ArraySize = 1 then
        AParam.AsFloat := (AValue as TJSONNumber).AsDouble
      else
        AParam.AsFloats[Self.Tag] := (AValue as TJSONNumber).AsDouble;

    ftCurrency:
      if Self.Params.ArraySize = 1 then
        AParam.AsCurrency := (AValue as TJSONNumber).AsDouble
      else
        AParam.AsCurrencys[Self.Tag] := (AValue as TJSONNumber).AsDouble;

    ftGuid:
      if Self.Params.ArraySize = 1 then
        AParam.AsGUID := TGUID.Create((AValue as TJSONString).Value)
      else
        AParam.AsGUIDs[Self.Tag] := TGUID.Create((AValue as TJSONString).Value);

    ftLargeint:
      if Self.Params.ArraySize = 1 then
        AParam.AsLargeInt := (AValue as TJSONNumber).AsInt64
      else
        AParam.AsLargeInts[Self.Tag] := (AValue as TJSONNumber).AsInt64;

    ftExtended:
      if Self.Params.ArraySize = 1 then
        AParam.AsExtended := (AValue as TJSONNumber).AsDouble
      else
        AParam.AsExtendeds[Self.Tag] := (AValue as TJSONNumber).AsDouble;

    ftLongWord:
      if Self.Params.ArraySize = 1 then
        AParam.AsLongword := (AValue as TJSONNumber).AsInt
      else
        AParam.AsLongwords[Self.Tag] := (AValue as TJSONNumber).AsInt;

    ftSingle:
      if Self.Params.ArraySize = 1 then
        AParam.AsSingle := (AValue as TJSONNumber).AsDouble
      else
        AParam.AsSingles[Self.Tag] := (AValue as TJSONNumber).AsDouble;

  else
    raise Exception.Create(Format('DataSet=%s,ParamName=%s,UnsurportDataType=%s',
      [Self.Name, AParam.Name, GetFieldTypeName(AParam.DataType)]));
  end;

end;

procedure TFDQueryHelper.ParamByJSONArray(AValue: TJSONArray; AName: String; AProc: TLogProc);
var
  MyElem: TJSONValue;
  Index: Integer;
begin
  Index := 1;
  for MyElem in AValue do
  begin
    ParamByJsonValue(MyElem, AName + '_' + Index.ToString, AProc);
    Inc(Index);
  end;
end;

procedure TFDQueryHelper.ParamByJSONObject(AObject: TJSONObject; AProc: TLogProc);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    ParamByJsonValue(MyElem.JsonValue, MyElem.JsonString.Value, AProc);
end;

procedure TFDQueryHelper.ParamByJsonValue(AValue: TJSONValue; AName: String; AProc: TLogProc);
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
  result := TMemoryStream.Create;
  Self.Close;
  Self.Open;
  Self.Refresh;
  Self.SaveToStream(result, sfBinary);
  result.Position := 0;
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

procedure TFDMemTableHelper.FieldByJSONObject(AObject: TJSONObject; AProc: TLogProc);
begin
  TFDDataSet(Self).FieldByJSONObject(AObject, AProc);
end;

procedure TFDMemTableHelper.FieldByJSONObject(AJSON: String; AProc: TLogProc);
begin
  TFDDataSet(Self).FieldByJSONObject(AJSON, AProc);
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
  result := TMemoryStream.Create;
  Self.SaveToStream(result, sfBinary);
  result.Position := 0;
end;

{ TFDDataSetHelper }

procedure TFDDataSetHelper.FieldByJsonValue(AField: TField; AValue: TJSONValue;
  AProc: TLogProc);
var
  Msg: String;
begin
  if not Assigned(AField) then
    Exit;

  Msg := Format('DataSet=%s,FieldName=%s,DataType=%s,Value=%s',
    [Self.Name, AField.Name, GetFieldTypeName(AField.DataType), AValue.Value]);

  if Assigned(AProc) then
    AProc(msDebug, 'SQLParameter', Msg);
  PrintDebug(Msg);

  case AField.DataType of
    ftUnknown:
      raise Exception.Create(Format('DataSet=%s,ParamName=%s,Unknown DataType',
        [Self.Name, AField.Name]));

    ftString, ftWideString:
      AField.AsString := (AValue as TJSONString).Value;

    ftSmallint, ftInteger, ftWord, ftShortint:
      AField.AsInteger := (AValue as TJSONNumber).AsInt;

{$IF CompilerVersion  > 26} // upper XE5
    ftBoolean:
      AField.AsBoolean := (AValue as TJSONBool).AsBoolean;

    ftDate, ftTime, ftDateTime:
      AField.AsDateTime := ISO8601ToDate((AValue as TJSONString).Value);

    ftTimeStamp:
      AField.AsSQLTimeStamp := DateTimeToSQLTimeStamp
        (ISO8601ToDate((AValue as TJSONString).Value));

{$ELSE}
    ftBoolean:
      AField.AsBoolean := AValue is TJSONTrue;

    ftDate, ftTime, ftDateTime, ftTimeStamp:
      AField.Value := AValue.Value;
{$ENDIF}
    ftGuid:
      TGuidField(AField).AsGUID := TGUID.Create((AValue as TJSONString).Value);

    ftFloat:
      AField.AsFloat := (AValue as TJSONNumber).AsDouble;

    ftCurrency:
      AField.AsCurrency := (AValue as TJSONNumber).AsDouble;

    ftLargeint:
      AField.AsLargeInt := (AValue as TJSONNumber).AsInt64;

    ftExtended:
      AField.AsExtended := (AValue as TJSONNumber).AsDouble;

    ftLongWord:
      AField.AsLongword := (AValue as TJSONNumber).AsInt;

    ftSingle:
      AField.AsSingle := (AValue as TJSONNumber).AsDouble;
  else
    raise Exception.Create(Format('DataSet=%s,ParamName=%s,UnsurportDataType=%s',
      [Self.Name, AField.Name, GetFieldTypeName(AField.DataType)]));
  end;
end;

procedure TFDDataSetHelper.FieldByJsonValue(AValue: TJSONValue; AName: String;
  AProc: TLogProc);
begin
  if AValue is TJSONObject then
    FieldByJSONObject(AValue as TJSONObject, AProc)
  else if AValue is TJSONArray then
  begin
    FieldByJSONArray(AValue as TJSONArray, AName, AProc)
  end
  else
    FieldByJsonValue(Self.FindField(AName), AValue, AProc);
end;

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
    ftSmallint, ftInteger, ftWord, ftShortint, ftAutoInc:
      result := TJSONNumber.Create(AField.AsInteger);

{$IF CompilerVersion  > 26} // upper XE5
    ftBoolean:
      result := TJSONBool.Create(AField.AsBoolean);
    ftDate, ftTime, ftDateTime:
      result := TJSONString.Create(AField.AsDateTime.ToISO8601);
    ftTimeStamp:
      result := TJSONString.Create(SQLTimeStampToDateTime(AField.AsSQLTimeStamp).ToISO8601);
{$ELSE}
    ftBoolean:
      begin
        if AField.AsBoolean then
          result := TJSONTrue.Create
        else
          result := TJSONFalse.Create;
      end;
    ftDate, ftTime, ftDateTime:
      result := TJSONString.Create(AField.AsDateTime.ToString);
    ftTimeStamp:
      result := TJSONString.Create(SQLTimeStampToDateTime(AField.AsSQLTimeStamp).ToString);
{$ENDIF}
    ftFloat:
      result := TJSONNumber.Create(AField.AsFloat);
    ftCurrency:
      result := TJSONNumber.Create(AField.AsCurrency);
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
    raise Exception.Create(Format('DataSet=%s,FieldName=%s,UnsurportDataType=%s',
      [Self.Name, AField.FieldName, GetFieldTypeName(AField.DataType)]));
  end;

  PrintDebug('DataSet=%s,FieldName=%s,DataType=%s,Value=%s',
    [Self.Name, AField.FieldName, GetFieldTypeName(AField.DataType), result.Value]);
end;

procedure TFDDataSetHelper.FieldByJSONArray(AValue: TJSONArray; AName: String;
  AProc: TLogProc);
var
  MyElem: TJSONValue;
  Index: Integer;
begin
  Index := 1;
  for MyElem in AValue do
  begin
    FieldByJsonValue(MyElem, AName + '_' + Index.ToString, AProc);
    Inc(Index);
  end;
end;

procedure TFDDataSetHelper.FieldByJSONObject(AObject: TJSONObject; AProc: TLogProc = nil);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    FieldByJsonValue(MyElem.JsonValue, MyElem.JsonString.Value);
end;

procedure TFDDataSetHelper.FieldByJSONObject(AJSON: String; AProc: TLogProc);
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  try
    FieldByJSONObject(JSONObject, AProc);
  finally
    JSONObject.Free;
  end;
end;

function TFDDataSetHelper.GetJSONArray(AValue: TJSONArray; AName: String): TJSONArray;
var
  I: Integer;
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
  for I := 0 to AValue.Size - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    result.AddElement(GetNameValue(AValue.Get(I), Key));
  end;
{$ENDIF}
end;

function TFDDataSetHelper.GetJSONObject(AValue: TJSONObject; AName: String): TJSONObject;
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
    result.AddPair(MyElem.JsonString.Value, GetNameValue(MyElem.JsonValue, Key));
  end;
end;

function TFDDataSetHelper.GetNameValue(AValue: TJSONValue; AName: String): TJSONValue;
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
  LStream: TMemoryStream;
begin
  LStream := TDSCommon.DBXStreamToMemoryStream(AStream);
  try
    if LStream.Size = 0 then
      raise Exception.Create('Reveiced Null Stream, ' + Self.Name);

    Self.LoadFromStream(LStream, sfBinary);
  finally
    FreeAndNil(LStream);
  end;
end;

function TFDDataSetHelper.ToJSON: TJSONObject;
var
  MyField: TField;
  JsonValue: TJSONValue;
begin
  result := TJSONObject.Create;
  for MyField in Self.Fields do
  begin
    JsonValue := FieldToJSONVlaue(MyField);
    result.AddPair(MyField.FieldName, JsonValue);
  end;
end;

function TFDDataSetHelper.ToRecord<T>(AName: String): T;
var
  MyRecord: T;
  TempContainer, ResultValue: TJSONObject;
begin
  FillChar(MyRecord, SizeOf(T), 0);
  TempContainer := REST.JSON.TJson.RecordToJsonObject<T>(MyRecord);
  try
    ResultValue := GetJSONObject(TempContainer, AName);
    try
      result := REST.JSON.TJson.JsonToRecord<T>(ResultValue);
    finally
      ResultValue.Free;
    end;
  finally
    TempContainer.Free;
  end;
end;

end.
