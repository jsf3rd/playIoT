// *******************************************************
//
// Jdc DataSnap Common
//
// Copyright(c) 2020.
//
// jsf3rd@nate.com
//
// *******************************************************

unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DBXPlatform,
  FireDAC.Comp.DataSet, FireDAC.Stan.Param, REST.JSON, JdcGlobal,
  System.Generics.Collections, System.DateUtils, Data.DB, Data.SqlTimSt, JdcLogging, Rtti
{$IF CompilerVersion  > 26} // upper XE5
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

type
  TOpenToFunc<T> = reference to function(const AQuery: TFDQuery): T;
  TDSOpenProc = function(const ARequestFilter: string = ''): TStream of object;
  TDSOpenParamProc = function(AParams: TJSONObject; const ARequestFilter: string = '')
    : TStream of object;

  TDSCommon = class
  private

  public
    class function GetRecordName<T: Record >: String;

    // Clear JSONObject Members
    class procedure ClearJSONObject(const AValue: TJSONObject); overload;
    class procedure ClearJSONObject(const AValue: TJSONArray); overload;

    // TDBXStream to TBytesStream
    class function DSStreamToBytesStream(const AStream: TStream): TBytesStream;
      deprecated 'DBXStreamToMemoryStream';

    // TDBXStream to TMemoryStream
    class function DBXStreamToMemoryStream(const AStream: TStream): TMemoryStream;

    // FDQuery to TSream
    class function DataSetToStream(const AQuery: TFDQuery): TStream; overload;
      deprecated 'TFDQueryHelper.ToStream';

    // FDMemTable to TStream
    class function DataSetToStream(const AMemTab: TFDMemTable): TStream; overload;
      deprecated 'TFDMemTableHelper.ToStream';

    // DataSnap TStream to TFDDataSet
    class procedure DSStreamToFDDataSet(const AStream: TStream; const ADataSet: TFDDataSet); static;
      deprecated 'TFDDataSetHelper.LoadFromDSStream';

    class procedure AddJSONValue<T: record >(var AObject: TJSONObject; const ARecord: T;
      const APreFix: string = '');
    class procedure AddJSONArray<T: record >(var AObject: TJSONObject; const ARecord: T);

    class function DSProcToRecord<T: record >(const AProc: TDSOpenProc): T; overload;
    class function DSProcToRecord<T: record >(const AProc: TDSOpenParamProc;
      const AParam: TJSONObject): T; overload;

    class function DSProcToRecordArray<T: record >(const AProc: TDSOpenProc): TArray<T>; overload;
    class function DSProcToRecordArray<T: record >(const AProc: TDSOpenParamProc;
      const AParam: TJSONObject): TArray<T>; overload;

    // Init TFDQuery Parameter DataType
    class procedure InitDataType(const ASender: TComponent; const AConn: TFDConnection);

    class function CalcExecTime(const ABegin: TDateTime; const ALimitSec: Integer = 5): Double;
  end;

  TFDDataSetHelper = class helper for TFDDataSet
  private
    function FieldToJSONVlaue(const AField: TField): TJSONValue;
    function GetNameValue(const AValue: TJSONValue; const AName: String): TJSONValue;
    function GetJSONObject(const AValue: TJSONObject; const AName: String): TJSONObject;
    function GetJSONArray(const AValue: TJSONArray; const AName: String): TJSONArray;

    procedure FieldByJsonValue(const AField: TField; const AValue: TJSONValue); overload;
    procedure FieldByJsonValue(const AValue: TJSONValue; const AName: String); overload;
    procedure FieldByJSONArray(const AValue: TJSONArray; const AName: String);
  public
    procedure LoadFromDSStream(const AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(const AName: String = ''): T;
    function ToRecordArray<T: Record >(const AName: String = ''): TArray<T>;

    procedure FieldByJSONObject(const AObject: TJSONObject); overload;
    procedure FieldByJSONObject(const AJSON: String); overload;
  end;

  TFDQueryHelper = class helper for TFDQuery
  private
    procedure ParamByJsonValue(const AParam: TFDParam; const AValue: TJSONValue); overload;
    procedure ParamByJsonValue(const AValue: TJSONValue; const AName: String); overload;
    procedure ParamByJSONArray(const AValue: TJSONArray; const AName: String);

    function OpenTo<T>(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType; const AProc: TOpenToFunc<T>): T;

    function ExecQuery(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType): Boolean; overload;
    function ExecQuery(const AConn: TFDConnection; const AParams: TJSONArray;
      const AType: TMessageType; const CommitAnyway: Boolean = False): Boolean; overload;
    function ApplyUpdate(const AConn: TFDConnection; const AStream: TStream;
      const AType: TMessageType): Boolean;
  public
    function Clone: TFDQuery;
    function ToStream: TStream;
    function ToMemTable: TFDMemTable;
    procedure LoadFromDSStream(const AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(const AName: String = ''): T;
    function ToRecordArray<T: Record >(const AName: String = ''): TArray<T>;

    procedure ParamByJSONObject(const AObject: TJSONObject);
    procedure FieldByJSONObject(const AObject: TJSONObject); overload;
    procedure FieldByJSONObject(const AJSON: String); overload;

    // OpenQuery
    function OpenQuery(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType): TStream;
    function OpenToMemTab(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType): TFDMemTable;

    function OpenInstantQuery(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType = msInfo): TStream; overload;
    function OpenInstantToMemTab(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType = msInfo): TFDMemTable; overload;

    // ExecQuery
    function ExecInstantQuery(const AConn: TFDConnection; const AParams: TJSONObject;
      const AType: TMessageType = msInfo): Boolean; overload;

    // Array DML
    function ExecInstantQuery(const AConn: TFDConnection; const AParams: TJSONArray;
      const AType: TMessageType = msInfo; const CommitAnyway: Boolean = False): Boolean; overload;

    // ApplyUpdate
    function ApplyInstantUpdate(const AConn: TFDConnection; const AStream: TStream;
      const AType: TMessageType = msInfo): Boolean;
  end;

  TFDMemTableHelper = class helper for TFDMemTable
  public
    function Clone: TFDMemTable;
    function ToStream: TStream;
    procedure LoadFromDSStream(const AStream: TStream);

    function ToJSON: TJSONObject;
    function ToRecord<T: Record >(const AName: String = ''): T;
    function ToRecordArray<T: Record >(const AName: String = ''): TArray<T>;
    procedure FieldByJSONObject(const AObject: TJSONObject); overload;
    procedure FieldByJSONObject(const AJSON: String); overload;
  end;

function GetFieldTypeName(const AType: TFieldType): string;

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

function GetFieldTypeName(const AType: TFieldType): string;
begin
  if Integer(AType) > Length(FieldTypeName) - 1 then
    Result := Integer(AType).ToString
  else
    Result := FieldTypeName[Integer(AType)];
end;

{ TDSCommon }

class procedure TDSCommon.AddJSONArray<T>(var AObject: TJSONObject; const ARecord: T);
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

class procedure TDSCommon.AddJSONValue<T>(var AObject: TJSONObject; const ARecord: T;
  const APreFix: string);
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

class function TDSCommon.CalcExecTime(const ABegin: TDateTime;
  const ALimitSec: Integer = 5): Double;
begin
  Result := MilliSecondsBetween(Now, ABegin) / 1000;
  if Result > ALimitSec then
    TLogging.Obj.ApplicationMessage(msWarning, 'Delay', 'ExecSec=%0.2f', [Result]);
end;

class procedure TDSCommon.ClearJSONObject(const AValue: TJSONArray);
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

class function TDSCommon.DataSetToStream(const AQuery: TFDQuery): TStream;
begin
  Result := AQuery.ToStream;
end;

class function TDSCommon.DataSetToStream(const AMemTab: TFDMemTable): TStream;
begin
  Result := AMemTab.ToStream;
end;

class procedure TDSCommon.ClearJSONObject(const AValue: TJSONObject);
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

class function TDSCommon.DSProcToRecord<T>(const AProc: TDSOpenParamProc;
  const AParam: TJSONObject): T;
var
  MemTab: TFDMemTable;
begin
  MemTab := TFDMemTable.Create(nil);
  try
    MemTab.Name := 'mt' + GetRecordName<T>.Substring(1);
    MemTab.LoadFromDSStream(AProc(AParam));
    if MemTab.RecordCount > 1 then
      raise Exception.Create('query did not return a unique result : ' +
        MemTab.RecordCount.ToString);

    Result := MemTab.ToRecord<T>;
  finally
    MemTab.Free;
  end;
end;

class function TDSCommon.DSProcToRecord<T>(const AProc: TDSOpenProc): T;
var
  MemTab: TFDMemTable;
begin
  MemTab := TFDMemTable.Create(nil);
  try
    MemTab.Name := 'mt' + GetRecordName<T>.Substring(1);
    MemTab.LoadFromDSStream(AProc);
    if MemTab.RecordCount > 1 then
      raise Exception.Create('query did not return a unique result : ' +
        MemTab.RecordCount.ToString);

    Result := MemTab.ToRecord<T>;
  finally
    MemTab.Free;
  end;
end;

class function TDSCommon.DSProcToRecordArray<T>(const AProc: TDSOpenParamProc;
  const AParam: TJSONObject): TArray<T>;
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

class function TDSCommon.DSProcToRecordArray<T>(const AProc: TDSOpenProc): TArray<T>;
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

class function TDSCommon.DSStreamToBytesStream(const AStream: TStream): TBytesStream;
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

class function TDSCommon.DBXStreamToMemoryStream(const AStream: TStream): TMemoryStream;
const
  BufferSize = 1024 * 32;
var
  Buffer: TBytes;
  ReadCount: Integer;
begin
  AStream.Position := 0;
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

class procedure TDSCommon.DSStreamToFDDataSet(const AStream: TStream; const ADataSet: TFDDataSet);
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

class procedure TDSCommon.InitDataType(const ASender: TComponent; const AConn: TFDConnection);
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

function TFDQueryHelper.ApplyInstantUpdate(const AConn: TFDConnection; const AStream: TStream;
  const AType: TMessageType): Boolean;
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

function TFDQueryHelper.ApplyUpdate(const AConn: TFDConnection; const AStream: TStream;
  const AType: TMessageType): Boolean;
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

      TLogging.Obj.ApplicationMessage(AType, 'ApplyUpdate',
        Format('Query=%s,ChangeCount=%d,Errors=%d,ExecSec=%0.2f', [Self.Name, Self.ChangeCount,
        Errors, TDSCommon.CalcExecTime(ExecTime)]));

      Result := Errors = 0;
    except
      on E: Exception do
      begin
        TLogging.Obj.ApplicationMessage(msError, 'ApplyUpdate', 'Query=%s,E=%s',
          [Self.Name, E.Message]);
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

procedure TFDQueryHelper.FieldByJSONObject(const AObject: TJSONObject);
begin
  TFDDataSet(Self).FieldByJSONObject(AObject);
end;

function TFDQueryHelper.ExecInstantQuery(const AConn: TFDConnection; const AParams: TJSONObject;
  const AType: TMessageType): Boolean;
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

function TFDQueryHelper.ExecInstantQuery(const AConn: TFDConnection; const AParams: TJSONArray;
  const AType: TMessageType; const CommitAnyway: Boolean): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := Self.Clone;
  try
    Result := MyQuery.ExecQuery(AConn, AParams, AType, CommitAnyway);
  finally
    MyQuery.Free;
  end;
end;

function TFDQueryHelper.ExecQuery(const AConn: TFDConnection; const AParams: TJSONArray;
  const AType: TMessageType; const CommitAnyway: Boolean): Boolean;
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
      Result := Self.RowsAffected = AParams.Count;

      if CommitAnyway or Result then
      begin
        AConn.Commit;
        TLogging.Obj.ApplicationMessage(AType, 'ExecQuery',
          'Commited,Query=%s,Requested=%d,RowsAffected=%d,ExecSec=%0.2f',
          [Self.Name, AParams.Count, Self.RowsAffected, TDSCommon.CalcExecTime(ExecTime)]);
      end
      else
      begin
        AConn.Rollback;
        TLogging.Obj.ApplicationMessage(AType, 'ExecQuery',
          'Rollbacked,Query=%s,Requested=%d,RowsAffected=%d,ExecSec=%0.2f',
          [Self.Name, AParams.Count, Self.RowsAffected, TDSCommon.CalcExecTime(ExecTime)]);
      end;
    except
      on E: Exception do
      begin
        AConn.Rollback;
        TLogging.Obj.ApplicationMessage(msError, 'ExecQuery', 'Query=%s,E=%s',
          [Self.Name, E.Message]);
      end;
    end;
  finally
    AConn.Free;
  end;

end;

function TFDQueryHelper.ExecQuery(const AConn: TFDConnection; const AParams: TJSONObject;
  const AType: TMessageType): Boolean;
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
      TLogging.Obj.ApplicationMessage(AType, 'ExecQuery',
        Format('Query=%s,RowsAffected=%d,ExecSec=%0.2f', [Self.Name, Self.RowsAffected,
        TDSCommon.CalcExecTime(ExecTime)]));
      Result := True;
    except
      on E: Exception do
        TLogging.Obj.ApplicationMessage(msError, 'ExecQuery', 'Query=%s,E=%s',
          [Self.Name, E.Message]);
    end;
  finally
    AConn.Free;
  end;
end;

procedure TFDQueryHelper.FieldByJSONObject(const AJSON: String);
begin
  TFDDataSet(Self).FieldByJSONObject(AJSON);
end;

procedure TFDQueryHelper.LoadFromDSStream(const AStream: TStream);
begin
  TFDDataSet(Self).LoadFromDSStream(AStream);
end;

function TFDQueryHelper.OpenInstantQuery(const AConn: TFDConnection; const AParams: TJSONObject;
  const AType: TMessageType): TStream;
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

function TFDQueryHelper.OpenInstantToMemTab(const AConn: TFDConnection; const AParams: TJSONObject;
  const AType: TMessageType): TFDMemTable;
var
  MyQuery: TFDQuery;
begin
  MyQuery := Self.Clone;
  try
    Result := MyQuery.OpenToMemTab(AConn, AParams, AType);
  finally
    MyQuery.Free;
  end;
end;

function TFDQueryHelper.OpenQuery(const AConn: TFDConnection; const AParams: TJSONObject;
  const AType: TMessageType): TStream;
begin
  Result := OpenTo<TStream>(AConn, AParams, AType,
    function(const AQuery: TFDQuery): TStream
    begin
      Result := AQuery.ToStream;
    end);
end;

function TFDQueryHelper.OpenTo<T>(const AConn: TFDConnection; const AParams: TJSONObject;
const AType: TMessageType; const AProc: TOpenToFunc<T>): T;
var
  ExecTime: TDateTime;
begin
  try
    if Assigned(AConn) then
      Self.Connection := AConn;
    try
      if Assigned(AParams) then
        ParamByJSONObject(AParams);

      ExecTime := Now;
      try
        Result := AProc(Self);
      except
        on E: Exception do
          raise Exception.Create(Format('ExecQuery,E=%s', [E.Message]));
      end;

      try
        TLogging.Obj.ApplicationMessage(AType, 'OpenTo',
          Format('Query=%s,RecordCount=%d,ExecSec=%0.2f', [Self.Name, Self.RecordCount,
          TDSCommon.CalcExecTime(ExecTime)]));
      except
        on E: Exception do
          raise Exception.Create(Format('RecordCount,E=%s', [E.Message]));
      end;
    except
      on E: Exception do
        raise Exception.Create(Format('OpenTo,Query=%s,E=%s', [Self.Name, E.Message]));
    end;
  finally
    if Assigned(AConn) then
      FreeAndNil(AConn);
  end;
end;

function TFDQueryHelper.OpenToMemTab(const AConn: TFDConnection; const AParams: TJSONObject;
const AType: TMessageType): TFDMemTable;
begin
  Result := OpenTo<TFDMemTable>(AConn, AParams, AType,
    function(const AQuery: TFDQuery): TFDMemTable
    begin
      Result := AQuery.ToMemTable;
    end);
end;

procedure TFDQueryHelper.ParamByJsonValue(const AParam: TFDParam; const AValue: TJSONValue);
var
  Msg: String;
begin
  if not Assigned(AParam) then
    Exit;

  Msg := Format('DataSet=%s,ParamName=%s,DataType=%s,Value=%s',
    [Self.Name, AParam.Name, GetFieldTypeName(AParam.DataType), AValue.Value]);

  // TLogging.Obj.ApplicationMessage(msSystem, 'SQLParameter', Msg);
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

    ftMemo, ftWideMemo:
      if Self.Params.ArraySize = 1 then
        AParam.AsWideMemo := (AValue as TJSONString).Value
      else
        AParam.AsWideMemos[Self.Tag] := (AValue as TJSONString).Value;
  else
    raise Exception.Create(Format('DataSet=%s,ParamName=%s,UnsurportDataType=%s',
      [Self.Name, AParam.Name, GetFieldTypeName(AParam.DataType)]));
  end;

end;

procedure TFDQueryHelper.ParamByJSONArray(const AValue: TJSONArray; const AName: String);
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

procedure TFDQueryHelper.ParamByJSONObject(const AObject: TJSONObject);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    ParamByJsonValue(MyElem.JsonValue, MyElem.JsonString.Value);
end;

procedure TFDQueryHelper.ParamByJsonValue(const AValue: TJSONValue; const AName: String);
var
  Msg: string;
begin
  if AValue is TJSONObject then
    ParamByJSONObject(AValue as TJSONObject)
  else if AValue is TJSONArray then
  begin
    ParamByJSONArray(AValue as TJSONArray, AName)
  end
  else
  begin
    try
      ParamByJsonValue(Self.FindParam(AName), AValue);
    except
      on E: Exception do
      begin
        Msg := Format('%s=%s->%s', [AName, AValue.Value, E.Message]);
        raise Exception.Create(Msg);
      end;
    end;
  end;
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

function TFDQueryHelper.ToRecordArray<T>(const AName: String): TArray<T>;
begin
  Result := TFDDataSet(Self).ToRecordArray<T>(AName);
end;

function TFDQueryHelper.ToJSON: TJSONObject;
begin
  Result := TFDDataSet(Self).ToJSON;
end;

function TFDQueryHelper.ToMemTable: TFDMemTable;
begin
  Result := TFDMemTable.Create(nil);
  Self.Close;
  Self.Open;
  Result.CopyDataSet(Self, [coStructure, coRestart, coAppend]);
end;

function TFDQueryHelper.ToRecord<T>(const AName: String): T;
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

procedure TFDMemTableHelper.FieldByJSONObject(const AObject: TJSONObject);
begin
  TFDDataSet(Self).FieldByJSONObject(AObject);
end;

procedure TFDMemTableHelper.FieldByJSONObject(const AJSON: String);
begin
  TFDDataSet(Self).FieldByJSONObject(AJSON);
end;

procedure TFDMemTableHelper.LoadFromDSStream(const AStream: TStream);
begin
  TFDDataSet(Self).LoadFromDSStream(AStream);
end;

function TFDMemTableHelper.ToRecordArray<T>(const AName: String): TArray<T>;
begin
  Result := TFDDataSet(Self).ToRecordArray<T>(AName);
end;

function TFDMemTableHelper.ToJSON: TJSONObject;
begin
  Result := TFDDataSet(Self).ToJSON;
end;

function TFDMemTableHelper.ToRecord<T>(const AName: String): T;
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

procedure TFDDataSetHelper.FieldByJsonValue(const AField: TField; const AValue: TJSONValue);
var
  Msg: String;
begin
  if not Assigned(AField) then
    Exit;

  Msg := Format('DataSet=%s,FieldName=%s,DataType=%s,Value=%s',
    [Self.Name, AField.Name, GetFieldTypeName(AField.DataType), AValue.Value]);

  // TLogging.Obj.ApplicationMessage(msSystem, 'SQLParameter', Msg);
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

procedure TFDDataSetHelper.FieldByJsonValue(const AValue: TJSONValue; const AName: String);
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

function TFDDataSetHelper.FieldToJSONVlaue(const AField: TField): TJSONValue;
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
    ftWideMemo:
      Result := TJSONString.Create(AField.AsWideString);
  else
    raise Exception.Create(Format('DataSet=%s,FieldName=%s,UnsurportDataType=%s',
      [Self.Name, AField.FieldName, GetFieldTypeName(AField.DataType)]));
  end;

  // TLogging.Obj.ApplicationMessage(msSystem, 'FieldToJSONVlaue',
  // 'DataSet=%s,FieldName=%s,DataType=%s,Value=%s', [Self.Name, AField.FieldName,
  // GetFieldTypeName(AField.DataType), Result.Value]);
end;

procedure TFDDataSetHelper.FieldByJSONArray(const AValue: TJSONArray; const AName: String);
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

procedure TFDDataSetHelper.FieldByJSONObject(const AObject: TJSONObject);
var
  MyElem: TJSONPair;
begin
  for MyElem in AObject do
    FieldByJsonValue(MyElem.JsonValue, MyElem.JsonString.Value);
end;

procedure TFDDataSetHelper.FieldByJSONObject(const AJSON: String);
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

function TFDDataSetHelper.GetJSONArray(const AValue: TJSONArray; const AName: String): TJSONArray;
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

function TFDDataSetHelper.GetJSONObject(const AValue: TJSONObject; const AName: String)
  : TJSONObject;
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

function TFDDataSetHelper.GetNameValue(const AValue: TJSONValue; const AName: String): TJSONValue;
begin
  if AValue is TJSONObject then
    Result := GetJSONObject(AValue as TJSONObject, AName)
  else if AValue is TJSONArray then
    Result := GetJSONArray(AValue as TJSONArray, AName)
  else
    Result := FieldToJSONVlaue(Self.Fields.FindField(AName));
end;

procedure TFDDataSetHelper.LoadFromDSStream(const AStream: TStream);
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

function TFDDataSetHelper.ToRecordArray<T>(const AName: String): TArray<T>;
var
  _record: T;
  Index: Integer;
begin
  SetLength(Result, Self.RecordCount);

  Index := 0;
  Self.First;
  while not Self.Eof do
  begin
    _record := Self.ToRecord<T>(AName);
    if Length(Result) > Index then
      Result[Index] := _record
    else
      Result := Result + [_record];
    Index := Index + 1;
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

function TFDDataSetHelper.ToRecord<T>(const AName: String): T;
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
