// *******************************************************
//
// DACO DataSnap Common
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@e-daco.net
//
// *******************************************************

unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DBXPlatform,
  FireDAC.Comp.DataSet, FireDAC.Stan.Param, REST.JSON,
  System.Generics.Collections, System.DateUtils, Data.DB, Data.SqlTimSt,
  JdcGlobal, JdcLogging, JclDebug, Rtti
{$IF CompilerVersion  > 26} // upper XE5
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

type
  TDSOpenProc = function(const ARequestFilter: string = ''): TStream of object;
  TDSOpenParamProc = function(AParams: TJSONObject; const ARequestFilter: string = ''): TStream of object;

  TDSCommon = class
  private

  public
    class function GetRecordName<T: Record >: String;

    // Clear JSONObject Members
    class procedure ClearJSONObject(AValue: TJSONObject); overload;
    class procedure ClearJSONObject(AValue: TJSONArray); overload;

    // TDBXStream to TBytesStream
    class function DSStreamToBytesStream(AStream: TStream): TBytesStream;
      deprecated 'DBXStreamToMemoryStream';

    // TDBXStream to TMemoryStream
    class function DBXStreamToMemoryStream(AStream: TStream): TMemoryStream;

    // FDQuery to TSream
    class function DataSetToStream(AQuery: TFDQuery): TStream; overload; deprecated 'TFDQueryHelper.ToStream';

    // FDMemTable to TStream
    class function DataSetToStream(AMemTab: TFDMemTable): TStream; overload;
      deprecated 'TFDMemTableHelper.ToStream';

    // DataSnap TStream to TFDDataSet
    class procedure DSStreamToFDDataSet(AStream: TStream; ADataSet: TFDDataSet); static;
      deprecated 'TFDDataSetHelper.LoadFromDSStream';

    class procedure AddJSONValue<T: record >(var AObject: TJSONObject; ARecord: T; APreFix: string = '');
    class procedure AddJSONArray<T: record >(var AObject: TJSONObject; ARecord: T);

    class function DSProcToRecord<T: record >(AProc: TDSOpenProc): T; overload;
    class function DSProcToRecord<T: record >(AProc: TDSOpenParamProc; AParam: TJSONObject): T; overload;

    class function DSProcToRecordArray<T: record >(AProc: TDSOpenProc): TArray<T>; overload;
    class function DSProcToRecordArray<T: record >(AProc: TDSOpenParamProc; AParam: TJSONObject)
      : TArray<T>; overload;

    // Init TFDQuery Parameter DataType
    class procedure InitDataType(ASender: TComponent; AConn: TFDConnection);
  end;

  TFDDataSetHelper = class helper for TFDDataSet
  private
    function FieldToJSONVlaue(AField: TField): TJSONValue;
    function GetNameValue(AValue: TJSONValue; AName: String): TJSONValue;
    function GetJSONObject(AValue: TJSONObject; AName: String): TJSONObject;
    function GetJSONArray(AValue: TJSONArray; AName: String): TJSONArray;

    procedure FieldByJsonValue(AField: TField; AValue: TJSONValue); overload;
    procedure FieldByJsonValue(AValue: TJSONValue; AName: String); overload;
    procedure FieldByJSONArray(AValue: TJSONArray; AName: String);
  public
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;
    function ToRecordArray<T: Record >(AName: String = ''): TArray<T>;

    procedure FieldByJSONObject(AObject: TJSONObject); overload;
    procedure FieldByJSONObject(AJSON: String); overload;
  end;

  TFDQueryHelper = class helper for TFDQuery
  private
    function GetExternalProc(OnlyProcName: Boolean = False): string;

    procedure ParamByJsonValue(AParam: TFDParam; AValue: TJSONValue); overload;
    procedure ParamByJsonValue(AValue: TJSONValue; AName: String); overload;
    procedure ParamByJSONArray(AValue: TJSONArray; AName: String);

    function OpenQuery(AConn: TFDConnection; AParams: TJSONObject; AType: TMessageType): TStream;
    function ExecQuery(AConn: TFDConnection; AParams: TJSONObject; AType: TMessageType): Boolean; overload;
    function ExecQuery(AConn: TFDConnection; AParams: TJSONArray; AType: TMessageType): Boolean; overload;
    function ApplyUpdate(AConn: TFDConnection; AStream: TStream; AType: TMessageType): Boolean;
  public
    function Clone: TFDQuery;
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;
    function ToRecordArray<T: Record >(AName: String = ''): TArray<T>;

    procedure ParamByJSONObject(AObject: TJSONObject);
    procedure FieldByJSONObject(AObject: TJSONObject); overload;
    procedure FieldByJSONObject(AJSON: String); overload;

    // OpenQuery
    function OpenInstantQuery(AConn: TFDConnection; AParams: TJSONObject; AType: TMessageType = msInfo)
      : TStream; overload;

    // ExecQuery
    function ExecInstantQuery(AConn: TFDConnection; AParams: TJSONObject; AType: TMessageType = msInfo)
      : Boolean; overload;

    // Array DML
    function ExecInstantQuery(AConn: TFDConnection; AParams: TJSONArray; AType: TMessageType = msInfo)
      : Boolean; overload;

    // ApplyUpdate
    function ApplyInstantUpdate(AConn: TFDConnection; AStream: TStream; AType: TMessageType = msInfo)
      : Boolean;
  end;

  TFDMemTableHelper = class helper for TFDMemTable
  public
    function Clone: TFDMemTable;
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(AName: String = ''): T;
    function ToRecordArray<T: Record >(AName: String = ''): TArray<T>;
    procedure FieldByJSONObject(AObject: TJSONObject); overload;
    procedure FieldByJSONObject(AJSON: String); overload;
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
    Result := Integer(AType).ToString
  else
    Result := FieldTypeName[Integer(AType)];
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

class procedure TDSCommon.AddJSONValue<T>(var AObject: TJSONObject; ARecord: T; APreFix: string);
var
  tmp: TJSONObject;
  MyPair, NewPair: TJSONPair;
  NewKey: string;
begin
  tmp := REST.JSON.TJson.RecordToJsonObject<T>(ARecord);
  try
    for MyPair in tmp do
    begin
      NewKey := APreFix + MyPair.JsonString.Value;
      AObject.AddPair(NewKey, MyPair.JsonValue.Clone as TJSONValue);
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
  Result := AQuery.ToStream;
end;

class function TDSCommon.DataSetToStream(AMemTab: TFDMemTable): TStream;
begin
  Result := AMemTab.ToStream;
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

class function TDSCommon.DSProcToRecord<T>(AProc: TDSOpenParamProc; AParam: TJSONObject): T;
var
  MemTab: TFDMemTable;
begin
  MemTab := TFDMemTable.Create(nil);
  try
    MemTab.Name := 'mt' + GetRecordName<T>.Substring(1);
    MemTab.LoadFromDSStream(AProc(AParam));
    if MemTab.RecordCount > 1 then
      raise Exception.Create('query did not return a unique result : ' + MemTab.RecordCount.ToString);

    Result := MemTab.ToRecord<T>;
  finally
    MemTab.Free;
  end;
end;

class function TDSCommon.DSProcToRecord<T>(AProc: TDSOpenProc): T;
var
  MemTab: TFDMemTable;
begin
  MemTab := TFDMemTable.Create(nil);
  try
    MemTab.Name := 'mt' + GetRecordName<T>.Substring(1);
    MemTab.LoadFromDSStream(AProc);
    if MemTab.RecordCount > 1 then
      raise Exception.Create('query did not return a unique result : ' + MemTab.RecordCount.ToString);

    Result := MemTab.ToRecord<T>;
  finally
    MemTab.Free;
  end;
end;

class function TDSCommon.DSProcToRecordArray<T>(AProc: TDSOpenParamProc; AParam: TJSONObject): TArray<T>;
var
  MemTab: TFDMemTable;
begin
  MemTab := TFDMemTable.Create(nil);
  try
    MemTab.Name := 'mt' + GetRecordName<T>.Substring(1);
    MemTab.LoadFromDSStream(AProc(AParam));
    Result := MemTab.ToRecordArray<T>;
  finally
    MemTab.Free;
  end;
end;

class function TDSCommon.DSProcToRecordArray<T>(AProc: TDSOpenProc): TArray<T>;
var
  MemTab: TFDMemTable;
begin
  MemTab := TFDMemTable.Create(nil);
  try
    MemTab.Name := 'mt' + GetRecordName<T>.Substring(1);
    MemTab.LoadFromDSStream(AProc);
    Result := MemTab.ToRecordArray<T>;
  finally
    MemTab.Free;
  end;
end;

class function TDSCommon.DSStreamToBytesStream(AStream: TStream): TBytesStream;
const
  BufferSize = 1024 * 32;
var
  Buffer: TBytes;
  BytesReadCount: Integer;
begin
  Result := TBytesStream.Create;

  SetLength(Buffer, BufferSize);
  repeat
    BytesReadCount := AStream.Read(Buffer[0], BufferSize);
    Result.Write(Buffer[0], BytesReadCount);
  until BytesReadCount < BufferSize;

  Result.Position := 0;
end;

class function TDSCommon.DBXStreamToMemoryStream(AStream: TStream): TMemoryStream;
const
  BufferSize = 1024 * 32;
var
  Buffer: TBytes;
  ReadCount: Integer;
begin
  Result := TMemoryStream.Create;
  try
    SetLength(Buffer, BufferSize);
    repeat
      ReadCount := AStream.Read(Buffer[0], BufferSize);
      Result.Write(Buffer[0], ReadCount);
    until ReadCount < BufferSize;
    Result.Position := 0;
  except
    Result.Free;
    raise;
  end;
end;

class procedure TDSCommon.DSStreamToFDDataSet(AStream: TStream; ADataSet: TFDDataSet);
begin
  ADataSet.LoadFromDSStream(AStream);
end;

class function TDSCommon.GetRecordName<T>: String;
var
  LContext: TRttiContext;
  LRecord: TRttiRecordType;
begin
  LContext := TRttiContext.Create;
  LRecord := LContext.GetType(TypeInfo(T)).AsRecord;
  Result := LRecord.Name;
end;

class procedure TDSCommon.InitDataType(ASender: TComponent; AConn: TFDConnection);
var
  tmp: TFDQuery;
  Query: TFDQuery;
  MyField: TField;
  MyParam: TFDParam;
  I, J: Integer;
begin
  for I := 0 to ASender.ComponentCount - 1 do
  begin
    if not(ASender.Components[I] is TFDQuery) then
      Continue;

    Query := ASender.Components[I] as TFDQuery;
    if Query.Params.Count = 0 then
      Continue;

    if Query.UpdateOptions.UpdateTableName = '' then
    begin
      for J := 0 to Query.Params.Count - 1 do
      begin
        if Query.Params.Items[J].DataType <> ftUnknown then
          raise Exception.Create(Format('Unknown Type,Query=%s,Field=%s',
            [Query.Name, Query.Params.Items[J].Name]));
      end;
      Continue;
    end;

    tmp := TFDQuery.Create(nil);
    try
      tmp.Connection := AConn;
      tmp.Open('select * from ' + Query.UpdateOptions.UpdateTableName + ' where false ');

      for J := 0 to Query.Params.Count - 1 do
      begin
        MyParam := Query.Params.Items[J];
        if MyParam.DataType <> ftUnknown then
          Continue;

        MyField := tmp.FindField(MyParam.Name);
        if MyField = nil then
          raise Exception.Create(Format('Unknown Field,Query=%s,Table=%s,Field=%s',
            [Query.Name, Query.UpdateOptions.UpdateTableName, MyParam.Name]));
        MyParam.DataType := MyField.DataType;
      end;
    finally
      tmp.Free;
    end;
  end;
end;

{ TFDQueryHelper }

function TFDQueryHelper.ApplyInstantUpdate(AConn: TFDConnection; AStream: TStream;
  AType: TMessageType): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := Self.Clone;
  try
    Result := MyQuery.ApplyUpdate(AConn, AStream, AType);
  finally
    MyQuery.Free;
  end;
end;

function TFDQueryHelper.ApplyUpdate(AConn: TFDConnection; AStream: TStream; AType: TMessageType): Boolean;
var
  ExecTime: TDateTime;
  Errors: Integer;
begin
  Result := False;
  try
    try
      Self.Connection := AConn;
      Self.LoadFromDSStream(AStream);

      ExecTime := Now;
      Errors := Self.ApplyUpdates;

      TLogging.Obj.ApplicationMessage(AType, JdcGlobal.GetCurrentProc,
        Format('Query=%s,ChangeCount=%d,Errors=%d,ExecTime=%s,Caller=%s', [Self.Name, Self.ChangeCount,
        Errors, FormatDateTime('NN:SS.zzz', Now - ExecTime), GetExternalProc]));

      Result := Errors = 0;
    except
      on E: Exception do
      begin
        TLogging.Obj.ApplicationMessage(msError, JdcGlobal.GetCurrentProc, 'Query=%s,E=%s,Caller=%s',
          [Self.Name, E.Message, GetExternalProc]);
      end;
    end;
  finally
    AConn.Free;
  end;

end;

function TFDQueryHelper.Clone: TFDQuery;
var
  I: Integer;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := Self.Connection;
  Result.OnExecuteError := Self.OnExecuteError;
  Result.OnReconcileError := Self.OnReconcileError;
  Result.CachedUpdates := Self.CachedUpdates;
  Result.UpdateOptions.KeyFields := Self.UpdateOptions.KeyFields;
  Result.UpdateOptions.UpdateTableName := Self.UpdateOptions.UpdateTableName;

  Result.SQL.Text := Self.SQL.Text;
  Result.Name := Self.Name + '_' + Format('%0.5d', [Random(100000)]);

  for I := 0 to Self.ParamCount - 1 do
    Result.Params.Items[I].DataType := Self.Params.Items[I].DataType;
end;

procedure TFDQueryHelper.FieldByJSONObject(AObject: TJSONObject);
begin
  TFDDataSet(Self).FieldByJSONObject(AObject);
end;

function TFDQueryHelper.ExecInstantQuery(AConn: TFDConnection; AParams: TJSONObject;
  AType: TMessageType): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := Self.Clone;
  try
    Result := MyQuery.ExecQuery(AConn, AParams, AType);
  finally
    MyQuery.Free;
  end;
end;

function TFDQueryHelper.ExecInstantQuery(AConn: TFDConnection; AParams: TJSONArray;
  AType: TMessageType): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := Self.Clone;
  try
    Result := MyQuery.ExecQuery(AConn, AParams, AType);
  finally
    MyQuery.Free;
  end;
end;

function TFDQueryHelper.ExecQuery(AConn: TFDConnection; AParams: TJSONArray; AType: TMessageType): Boolean;
var
  ExecTime: TDateTime;
  I: Integer;
  Params: TJSONObject;
begin
  Result := False;
  AConn.StartTransaction;
  try
    Self.Connection := AConn;
    try
      Self.Params.ArraySize := AParams.Count;
      for I := 0 to AParams.Count - 1 do
      begin
        Params := AParams.Items[I] as TJSONObject;
        Self.Tag := I;
        Self.ParamByJSONObject(Params);
      end;

      ExecTime := Now;
      Self.Execute(AParams.Count);
      TLogging.Obj.ApplicationMessage(AType, JdcGlobal.GetCurrentProc,
        Format('Query=%s,Requested=%d,RowsAffected=%d,ExecTime=%s,Caller=%s', [Self.Name, AParams.Count,
        Self.RowsAffected, FormatDateTime('NN:SS.zzz', Now - ExecTime), GetExternalProc]));
      Result := Self.RowsAffected = AParams.Count;

      if Result then
        AConn.Commit
      else
        AConn.Rollback;
    except
      on E: Exception do
      begin
        AConn.Rollback;
        TLogging.Obj.ApplicationMessage(msError, JdcGlobal.GetCurrentProc, 'Query=%s,E=%s,Caller=%s',
          [Self.Name, E.Message, GetExternalProc]);
      end;
    end;
  finally
    AConn.Free;
  end;

end;

function TFDQueryHelper.ExecQuery(AConn: TFDConnection; AParams: TJSONObject; AType: TMessageType): Boolean;
var
  ExecTime: TDateTime;
begin
  Result := False;
  try
    Self.Connection := AConn;
    try
      if Assigned(AParams) then
        Self.ParamByJSONObject(AParams);

      ExecTime := Now;
      Self.ExecSQL;
      TLogging.Obj.ApplicationMessage(AType, JdcGlobal.GetCurrentProc,
        Format('Query=%s,RowsAffected=%d,ExecTime=%s,Caller=%s', [Self.Name, Self.RowsAffected,
        FormatDateTime('NN:SS.zzz', Now - ExecTime), GetExternalProc]));
      Result := True;
    except
      on E: Exception do
        TLogging.Obj.ApplicationMessage(msError, JdcGlobal.GetCurrentProc, 'Query=%s,E=%s,Caller=%s',
          [Self.Name, E.Message, GetExternalProc]);
    end;
  finally
    AConn.Free;
  end;
end;

procedure TFDQueryHelper.FieldByJSONObject(AJSON: String);
begin
  TFDDataSet(Self).FieldByJSONObject(AJSON);
end;

function TFDQueryHelper.GetExternalProc(OnlyProcName: Boolean): string;
var
  Proc: string;
begin
  // 0.ThisMethod - 1.OpenQuery - 2.OpenInstansQuery - 3.RealCaller
  Proc := JdcGlobal.GetProcByLevel(3, OnlyProcName);
  Result := Proc;
end;

procedure TFDQueryHelper.LoadFromDSStream(AStream: TStream);
begin
  TFDDataSet(Self).LoadFromDSStream(AStream);
end;

function TFDQueryHelper.OpenInstantQuery(AConn: TFDConnection; AParams: TJSONObject;
  AType: TMessageType): TStream;
var
  MyQuery: TFDQuery;
begin
  MyQuery := Self.Clone;
  try
    Result := MyQuery.OpenQuery(AConn, AParams, AType);
  finally
    MyQuery.Free;
  end;
end;

function TFDQueryHelper.OpenQuery(AConn: TFDConnection; AParams: TJSONObject; AType: TMessageType): TStream;
var
  ExecTime: TDateTime;
begin
  try
    Self.Connection := AConn;
    try
      if Assigned(AParams) then
        ParamByJSONObject(AParams);

      ExecTime := Now;
      Result := Self.ToStream;
      TLogging.Obj.ApplicationMessage(AType, JdcGlobal.GetCurrentProc,
        Format('Query=%s,RecordCount=%d,ExecTime=%s,Caller=%s', [Self.Name, Self.RecordCount,
        FormatDateTime('NN:SS.zzz', Now - ExecTime), GetExternalProc]));
    except
      on E: Exception do
        raise Exception.Create(Format('Query=%s,E=%s,Caller=%s', [Self.Name, E.Message, GetExternalProc]));
    end;
  finally
    AConn.Free;
  end;
end;

procedure TFDQueryHelper.ParamByJsonValue(AParam: TFDParam; AValue: TJSONValue);
var
  Msg: String;
begin
  if not Assigned(AParam) then
    Exit;

  Msg := Format('DataSet=%s,ParamName=%s,DataType=%s,Value=%s',
    [Self.Name, AParam.Name, GetFieldTypeName(AParam.DataType), AValue.Value]);

  TLogging.Obj.ApplicationMessage(msSystem, 'SQLParameter', Msg);
  case AParam.DataType of
    ftUnknown:
      raise Exception.Create(Format('DataSet=%s,ParamName=%s,Unknown DataType', [Self.Name, AParam.Name]));

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
        AParam.AsDates[Self.Tag] := StrToDate((AValue as TJSONString).Value, DefaultFormatSettings);

    ftTime:
      if Self.Params.ArraySize = 1 then
        AParam.AsTime := StrToTime((AValue as TJSONString).Value, DefaultFormatSettings)
      else
        AParam.AsTimes[Self.Tag] := StrToTime((AValue as TJSONString).Value, DefaultFormatSettings);

    ftDateTime:
      if Self.Params.ArraySize = 1 then
        AParam.AsDateTime := ISO8601ToDate((AValue as TJSONString).Value)
      else
        AParam.AsDateTimes[Self.Tag] := ISO8601ToDate((AValue as TJSONString).Value);

    ftTimeStamp:
      if Self.Params.ArraySize = 1 then
        AParam.AsSQLTimeStamp := DateTimeToSQLTimeStamp(ISO8601ToDate((AValue as TJSONString).Value))
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

    ftWideMemo:
      if Self.Params.ArraySize = 1 then
        AParam.AsWideMemo := (AValue as TJSONString).Value
      else
        AParam.AsWideMemos[Self.Tag] := (AValue as TJSONString).Value;
  else
    raise Exception.Create(Format('DataSet=%s,ParamName=%s,UnsurportDataType=%s',
      [Self.Name, AParam.Name, GetFieldTypeName(AParam.DataType)]));
  end;

end;

procedure TFDQueryHelper.ParamByJSONArray(AValue: TJSONArray; AName: String);
var
  MyElem: TJSONValue;
  Index: Integer;
begin
  Index := 1;
  for MyElem in AValue do
  begin
    ParamByJsonValue(MyElem, AName + '_' + Index.ToString);
    Inc(Index);
  end;
end;

procedure TFDQueryHelper.ParamByJSONObject(AObject: TJSONObject);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    ParamByJsonValue(MyElem.JsonValue, MyElem.JsonString.Value);
end;

procedure TFDQueryHelper.ParamByJsonValue(AValue: TJSONValue; AName: String);
begin
  if AValue is TJSONObject then
    ParamByJSONObject(AValue as TJSONObject)
  else if AValue is TJSONArray then
  begin
    ParamByJSONArray(AValue as TJSONArray, AName)
  end
  else
    ParamByJsonValue(Self.FindParam(AName), AValue);
end;

function TFDQueryHelper.ToStream: TStream;
begin
  Result := TMemoryStream.Create;
  Self.Close;
  Self.Open;
  Self.Refresh;
  Self.SaveToStream(Result, sfBinary);
  Result.Position := 0;
end;

function TFDQueryHelper.ToRecordArray<T>(AName: String): TArray<T>;
begin
  Result := TFDDataSet(Self).ToRecordArray<T>(AName);
end;

function TFDQueryHelper.ToJSON: TJSONObject;
begin
  Result := TFDDataSet(Self).ToJSON;
end;

function TFDQueryHelper.ToRecord<T>(AName: String): T;
begin
  Result := TFDDataSet(Self).ToRecord<T>(AName);
end;

{ TFDMemTableHelper }

function TFDMemTableHelper.Clone: TFDMemTable;
var
  Stream: TStream;
  StoreItems: TFDStoreItems;
begin
  Result := TFDMemTable.Create(Self.Owner);

  StoreItems := Self.ResourceOptions.StoreItems;
  Self.ResourceOptions.StoreItems := [siData, siDelta, siMeta];
  Stream := TMemoryStream.Create;
  try
    Self.SaveToStream(Stream);
    Stream.Position := 0;
    Result.LoadFromStream(Stream);
  finally
    Self.ResourceOptions.StoreItems := StoreItems;
    FreeAndNil(Stream);
  end;
end;

procedure TFDMemTableHelper.FieldByJSONObject(AObject: TJSONObject);
begin
  TFDDataSet(Self).FieldByJSONObject(AObject);
end;

procedure TFDMemTableHelper.FieldByJSONObject(AJSON: String);
begin
  TFDDataSet(Self).FieldByJSONObject(AJSON);
end;

procedure TFDMemTableHelper.LoadFromDSStream(AStream: TStream);
begin
  TFDDataSet(Self).LoadFromDSStream(AStream);
end;

function TFDMemTableHelper.ToRecordArray<T>(AName: String): TArray<T>;
begin
  Result := TFDDataSet(Self).ToRecordArray<T>(AName);
end;

function TFDMemTableHelper.ToJSON: TJSONObject;
begin
  Result := TFDDataSet(Self).ToJSON;
end;

function TFDMemTableHelper.ToRecord<T>(AName: String): T;
begin
  Result := TFDDataSet(Self).ToRecord<T>(AName);
end;

function TFDMemTableHelper.ToStream: TStream;
begin
  Result := TMemoryStream.Create;
  Self.SaveToStream(Result, sfBinary);
  Result.Position := 0;
end;

{ TFDDataSetHelper }

procedure TFDDataSetHelper.FieldByJsonValue(AField: TField; AValue: TJSONValue);
var
  Msg: String;
begin
  if not Assigned(AField) then
    Exit;

  Msg := Format('DataSet=%s,FieldName=%s,DataType=%s,Value=%s',
    [Self.Name, AField.Name, GetFieldTypeName(AField.DataType), AValue.Value]);

  TLogging.Obj.ApplicationMessage(msSystem, 'SQLParameter', Msg);
  case AField.DataType of
    ftUnknown:
      raise Exception.Create(Format('DataSet=%s,ParamName=%s,Unknown DataType', [Self.Name, AField.Name]));

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
      AField.AsSQLTimeStamp := DateTimeToSQLTimeStamp(ISO8601ToDate((AValue as TJSONString).Value));

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

procedure TFDDataSetHelper.FieldByJsonValue(AValue: TJSONValue; AName: String);
begin
  if AValue is TJSONObject then
    FieldByJSONObject(AValue as TJSONObject)
  else if AValue is TJSONArray then
  begin
    FieldByJSONArray(AValue as TJSONArray, AName)
  end
  else
    FieldByJsonValue(Self.FindField(AName), AValue);
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
      Result := TJSONString.Create(AField.AsString);
    ftSmallint, ftInteger, ftWord, ftShortint, ftAutoInc:
      Result := TJSONNumber.Create(AField.AsInteger);

{$IF CompilerVersion  > 26} // upper XE5
    ftBoolean:
      Result := TJSONBool.Create(AField.AsBoolean);
    ftDate, ftTime, ftDateTime:
      Result := TJSONString.Create(AField.AsDateTime.ToISO8601);
    ftTimeStamp:
      Result := TJSONString.Create(SQLTimeStampToDateTime(AField.AsSQLTimeStamp).ToISO8601);
{$ELSE}
    ftBoolean:
      begin
        if AField.AsBoolean then
          Result := TJSONTrue.Create
        else
          Result := TJSONFalse.Create;
      end;
    ftDate, ftTime, ftDateTime:
      Result := TJSONString.Create(AField.AsDateTime.ToString);
    ftTimeStamp:
      Result := TJSONString.Create(SQLTimeStampToDateTime(AField.AsSQLTimeStamp).ToString);
{$ENDIF}
    ftFloat:
      Result := TJSONNumber.Create(AField.AsFloat);
    ftCurrency:
      Result := TJSONNumber.Create(AField.AsCurrency);
    ftLargeint:
      Result := TJSONNumber.Create(AField.AsLargeInt);
    ftGuid:
      Result := TJSONString.Create(TGuidField(AField).AsGUID.ToString);
    ftExtended:
      Result := TJSONNumber.Create(AField.AsExtended);
    ftLongWord:
      Result := TJSONNumber.Create(AField.AsLongword);
    ftSingle:
      Result := TJSONNumber.Create(AField.AsSingle);
  else
    raise Exception.Create(Format('DataSet=%s,FieldName=%s,UnsurportDataType=%s',
      [Self.Name, AField.FieldName, GetFieldTypeName(AField.DataType)]));
  end;

  TLogging.Obj.ApplicationMessage(msSystem, 'FieldToJSONVlaue',
    'DataSet=%s,FieldName=%s,DataType=%s,Value=%s', [Self.Name, AField.FieldName,
    GetFieldTypeName(AField.DataType), Result.Value]);
end;

procedure TFDDataSetHelper.FieldByJSONArray(AValue: TJSONArray; AName: String);
var
  MyElem: TJSONValue;
  Index: Integer;
begin
  Index := 1;
  for MyElem in AValue do
  begin
    FieldByJsonValue(MyElem, AName + '_' + Index.ToString);
    Inc(Index);
  end;
end;

procedure TFDDataSetHelper.FieldByJSONObject(AObject: TJSONObject);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    FieldByJsonValue(MyElem.JsonValue, MyElem.JsonString.Value);
end;

procedure TFDDataSetHelper.FieldByJSONObject(AJSON: String);
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  try
    FieldByJSONObject(JSONObject);
  finally
    JSONObject.Free;
  end;
end;

function TFDDataSetHelper.GetJSONArray(AValue: TJSONArray; AName: String): TJSONArray;
var
  I: Integer;
  Key: string;
begin
  Result := TJSONArray.Create;

{$IF CompilerVersion  > 26} // upper XE5
  for I := 0 to AValue.Count - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    Result.AddElement(GetNameValue(AValue.Items[I], Key));
  end;
{$ELSE}
  for I := 0 to AValue.Size - 1 do
  begin
    Key := AName + '_' + (I + 1).ToString;
    Result.AddElement(GetNameValue(AValue.Get(I), Key));
  end;
{$ENDIF}
end;

function TFDDataSetHelper.GetJSONObject(AValue: TJSONObject; AName: String): TJSONObject;
var
  MyElem: TJSONPair;
  Key: string;
begin
  Result := TJSONObject.Create;
  for MyElem in AValue do
  begin
    Key := AName + '_';
    if AName.IsEmpty then
      Key := '';

    Key := Key + MyElem.JsonString.Value;
    Result.AddPair(MyElem.JsonString.Value, GetNameValue(MyElem.JsonValue, Key));
  end;
end;

function TFDDataSetHelper.GetNameValue(AValue: TJSONValue; AName: String): TJSONValue;
begin
  if AValue is TJSONObject then
    Result := GetJSONObject(AValue as TJSONObject, AName)
  else if AValue is TJSONArray then
    Result := GetJSONArray(AValue as TJSONArray, AName)
  else
    Result := FieldToJSONVlaue(Self.Fields.FindField(AName));
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

function TFDDataSetHelper.ToRecordArray<T>(AName: String): TArray<T>;
var
  _record: T;
begin
  SetLength(Result, 0);

  Self.First;
  while not Self.Eof do
  begin
    _record := Self.ToRecord<T>(AName);
    Result := Result + [_record];
    Self.Next;
  end;
end;

function TFDDataSetHelper.ToJSON: TJSONObject;
var
  MyField: TField;
  JsonValue: TJSONValue;
begin
  Result := TJSONObject.Create;
  for MyField in Self.Fields do
  begin
    JsonValue := FieldToJSONVlaue(MyField);
    Result.AddPair(MyField.FieldName, JsonValue);
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
      Result := REST.JSON.TJson.JsonToRecord<T>(ResultValue);
    finally
      ResultValue.Free;
    end;
  finally
    TempContainer.Free;
  end;
end;

end.
