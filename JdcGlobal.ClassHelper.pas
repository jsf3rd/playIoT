// *******************************************************
//
// DACO Class Helper
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@e-daco.net
//
//
// *******************************************************

unit JdcGlobal.ClassHelper;

interface

uses
  Classes, SysUtils, REST.JSON, Winapi.Windows,
  XSuperObject, System.IOUtils, System.Generics.Collections, System.DateUtils, Data.SqlTimSt,
  IdContext, JdcGlobal, IdGlobal, IdExceptionCore, IdIOHandler, JdcLogging, Vcl.StdCtrls

{$IF CompilerVersion  > 26} // upper XE5
    , System.JSON
{$ELSE}
    , Data.DBXJSON, Data.DBXPlatform
{$ENDIF}
{$IFDEF MSWINDOWS}
    , Vcl.ExtCtrls
{$ENDIF}
    ;

type
{$IFDEF MSWINDOWS}
  TTimerHelper = class helper for TTimer
  public
    procedure Reset;
  end;
{$ENDIF}

  TIdContextHelper = class helper for TIdContext
  private
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(const Value: Integer);
  public
    function IOHandler: TIdIOHandler;
    function ReadByte: Byte;
    procedure ReadBytes(var VBuffer: TIdBytes; AByteCount: Integer; AAppend: Boolean = True);
    procedure Write(const ABuffer: TIdBytes; const ALength: Integer = -1; const AOffset: Integer = 0);

    function PeerIP: string;
    function PeerPort: word;
    function PeerInfo: string;

    procedure FlushBuffer;

    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  end;

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetValueEx(const Name: string): TJSONValue;
    function GetString(const Name: string; const default: string = ''): String;
    function GetInt(const Name: string): Integer;
    function GetIntDef(const Name: string; const default: Integer = -9999): Integer;
    function GetDouble(const Name: string): double;
    function GetDoubleDef(const Name: string; const default: double = -9999): double;
    function GetJSONArray(const Name: string): TJSONArray;
    function GetJSONObject(const Name: string): TJSONObject;

    function AddPair(const Str: string; const Val: Integer): TJSONObject; overload;
    function AddPair(const Str: string; const Val: double): TJSONObject; overload;

    function ToRecord<T: record >: T;
    function ToObject<T: class>: T;

    class function ParseFile(FileName: String): TJSONValue;

    procedure Clear;
  end;

  TJSONArrayHelper = class helper for TJSONArray
  public

  end;

  TJSONHelper = class helper for REST.JSON.TJSON
  public
    class function ObjectToJsonObjectEx(AObject: TObject): TJSONObject;

    class function ObjectToJsonStringEx(AObject: TObject): String;
    class function JsonToObjectEx<T: class>(AJsonObject: TJSONObject): T; overload;
    class function JsonToObjectEx<T: class>(const AJson: String): T; overload;
    class function FileToObject<T: class>(const FileName: String): T;

    class function RecordToJsonObject<T: record >(ARecord: T): TJSONObject;
    class function RecordToJsonString<T: record >(ARecord: T): String;
    class function JsonToRecord<T: record >(const AJsonObject: TJSONObject): T; overload;
    class function JsonToRecord<T: record >(const AJson: String): T; overload;
    class function FileToRecord<T: record >(const FileName: String): T;
    class function ConvertRecord<T1, T2: record >(ARecord: T1): T2;

    class function RecordArrayToJsonArray<T: record >(RecordArray: TArray<T>): TJSONArray;
    class function JsonArrayToRecordArray<T: record >(JsonArray: TJSONArray): TArray<T>;
  end;

  TTimeHelper = record helper for TTime
  public
    function ToString: String;
  end;

  TDateTimeHelper = record helper for TDateTime
  public
    function ToString: String;

    function ToISO8601: String;
    function FormatWithMSec: String;
    function FormatWithoutMSec: String;
    function RecodeTenMinute: TDateTime;
    function SQLTimeStamp: TSQLTimeStamp;
    function Date: string;
    function Time: string;
  end;

  TCustomComboboxHelper = class helper for TCustomCombobox
  public
    procedure RightAlignment;
  end;

function ReplaceStringNumber(const AInput: string): string;

implementation

{$IFDEF MSWINDOWS}

uses JdcGlobal.DSCommon;

function ReplaceStringNumber(const AInput: string): string;
begin
  result := AInput.Replace('NAN', '0', [rfReplaceAll]);
  result := result.Replace('INF', '0', [rfReplaceAll]);
end;

{ TTimerHelper }

procedure TTimerHelper.Reset;
begin
  Self.Enabled := False;
  Self.Enabled := True;
end;
{$ENDIF}

{ TJSONObjectHelper }
function TJSONObjectHelper.AddPair(const Str: string; const Val: Integer): TJSONObject;
begin
  if not Str.IsEmpty then
    AddPair(TJSONPair.Create(Str, TJSONNumber.Create(Val)));
  result := Self;
end;

function TJSONObjectHelper.GetJSONArray(const Name: string): TJSONArray;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    result := JSONValue as TJSONArray;
  except
    on E: Exception do
      raise Exception.Create(SysUtils.Format('JSON name [%s] can not cast to TJSONArray. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetJSONObject(const Name: string): TJSONObject;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    result := JSONValue as TJSONObject;
  except
    on E: Exception do
      raise Exception.Create(SysUtils.Format('JSON name [%s] can not cast to TJSONObject. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.AddPair(const Str: string; const Val: double): TJSONObject;
begin
  if not Str.IsEmpty then
    AddPair(TJSONPair.Create(Str, TJSONNumber.Create(Val)));
  result := Self;
end;

procedure TJSONObjectHelper.Clear;
begin
  TDSCommon.ClearJSONObject(Self);
end;

function TJSONObjectHelper.GetDouble(const Name: string): double;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);
  try
    result := (JSONValue as TJSONNumber).AsDouble;
  except
    on E: Exception do
      raise Exception.Create(SysUtils.Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetDoubleDef(const Name: string; const default: double): double;
var
  JSONValue: TJSONValue;
begin
  try
    JSONValue := GetValueEx(Name);
    result := (JSONValue as TJSONNumber).AsDouble;
  except
    result := default;
  end;
end;

function TJSONObjectHelper.GetInt(const Name: string): Integer;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    result := (JSONValue as TJSONNumber).AsInt;
  except
    on E: Exception do
      raise Exception.Create(SysUtils.Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetIntDef(const Name: string; const default: Integer): Integer;
var
  JSONValue: TJSONValue;
begin
  try
    JSONValue := GetValueEx(Name);
    result := (JSONValue as TJSONNumber).AsInt;
  except
    on E: Exception do
      result := default;
  end;
end;

function TJSONObjectHelper.GetString(const Name: string; const default: string): String;
begin
  try
    result := GetValueEx(Name).Value;
  except
    on E: Exception do
      result := default;
  end;
end;

function TJSONObjectHelper.GetValueEx(const Name: string): TJSONValue;
var
  MyElem: TJSONPair;
  Names: String;
begin
  result := GetValue(Name);

  if Assigned(result) then
    Exit;

  Names := '';
  for MyElem in Self do
    Names := Names + MyElem.JsonString.Value + ', ';

  raise Exception.Create(SysUtils.Format('JSON name [%s] is not exist. Other name list [%s],caller=%s',
    [Name, Names, GetProcByLevel(2)]));
end;

class function TJSONObjectHelper.ParseFile(FileName: String): TJSONValue;
var
  JsonString: string;
begin
  JsonString := ReplaceStringNumber(TFile.ReadAllText(FileName));
  result := TJSONObject.ParseJSONValue(JsonString);
end;

function TJSONObjectHelper.ToObject<T>: T;
begin
  result := REST.JSON.TJSON.JsonToObjectEx<T>(Self);
end;

function TJSONObjectHelper.ToRecord<T>: T;
begin
  result := REST.JSON.TJSON.JsonToRecord<T>(Self);
end;

{ TJSONHelper }

class function TJSONHelper.JsonToObjectEx<T>(AJsonObject: TJSONObject): T;
begin
  result := JsonToObjectEx<T>(AJsonObject.ToString);
end;

class function TJSONHelper.ConvertRecord<T1, T2>(ARecord: T1): T2;
var
  JSONObject: TJSONObject;
begin
  JSONObject := RecordToJsonObject<T1>(ARecord);
  result := JsonToRecord<T2>(JSONObject);
  JSONObject.Free;
end;

class function TJSONHelper.FileToObject<T>(const FileName: String): T;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  result := REST.JSON.TJSON.JsonToObjectEx<T>(JsonString);
end;

class function TJSONHelper.FileToRecord<T>(const FileName: String): T;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  result := REST.JSON.TJSON.JsonToRecord<T>(JsonString);
end;

class function TJSONHelper.JsonArrayToRecordArray<T>(JsonArray: TJSONArray): TArray<T>;
var
  MyValue: TJSONValue;
  I: Integer;
begin
  SetLength(result, JsonArray.Count);

  I := 0;
  for MyValue in JsonArray do
  begin
    try
      result[I] := JsonToRecord<T>(MyValue as TJSONObject);
    except
      on E: Exception do
        raise Exception.Create('JsonArrayToRecordArray - ' + E.Message);
    end;

    Inc(I);
  end;
end;

class function TJSONHelper.JsonToObjectEx<T>(const AJson: String): T;
begin
  result := TSuperObject.Create(AJson).AsType<T>;
end;

class function TJSONHelper.JsonToRecord<T>(const AJsonObject: TJSONObject): T;
begin
  result := JsonToRecord<T>(AJsonObject.ToString);
end;

class function TJSONHelper.JsonToRecord<T>(const AJson: String): T;
var
  JsonString: string;
begin
  JsonString := ReplaceStringNumber(AJson);
  result := TSuperRecord<T>.FromJSON(JsonString);
end;

class function TJSONHelper.ObjectToJsonObjectEx(AObject: TObject): TJSONObject;
var
  JSONStr: String;
begin
  JSONStr := ReplaceStringNumber(AObject.AsJSON);
  result := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
end;

class function TJSONHelper.ObjectToJsonStringEx(AObject: TObject): String;
begin
  result := AObject.AsJSON;
end;

class function TJSONHelper.RecordArrayToJsonArray<T>(RecordArray: TArray<T>): TJSONArray;
var
  MyElem: T;
begin
  result := TJSONArray.Create;

  for MyElem in RecordArray do
  begin
    result.Add(RecordToJsonObject(MyElem));
  end;
end;

class function TJSONHelper.RecordToJsonObject<T>(ARecord: T): TJSONObject;
var
  JsonString: string;
begin
  try
    JsonString := ReplaceStringNumber(RecordToJsonString<T>(ARecord));
    result := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  except
    on E: Exception do
      raise Exception.Create('RecordToJsonObject,' + E.Message);
  end;
end;

class function TJSONHelper.RecordToJsonString<T>(ARecord: T): String;
begin
  result := TSuperRecord<T>.AsJSON(ARecord);
end;

{ TDateTimeHelper }

function TDateTimeHelper.Date: string;
begin
  result := FormatDateTime('YYYY-MM-DD', Self);
end;

function TDateTimeHelper.FormatWithMSec: String;
begin
  result := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', Self);
end;

function TDateTimeHelper.FormatWithoutMSec: String;
begin
  result := FormatDateTime('YYYY-MM-DD HH:NN:SS', Self);
end;

function TDateTimeHelper.RecodeTenMinute: TDateTime;
var
  Min: Integer;
begin
  Min := MinuteOf(Self);
  Min := (Min div 10) * 10;

  result := RecodeMilliSecond(Self, 0);
  result := RecodeSecond(result, 0);
  result := RecodeMinute(result, Min);
end;

function TDateTimeHelper.SQLTimeStamp: TSQLTimeStamp;
begin
  result := DateTimeToSQLTimeStamp(Self);
end;

function TDateTimeHelper.Time: string;
begin
  result := FormatDateTime('HH:NN:SS', Self);
end;

function TDateTimeHelper.ToISO8601: String;
begin
  result := FormatDateTime('YYYY-MM-DD"T"HH:NN:SS.zzz', Self);
end;

function TDateTimeHelper.ToString: String;
begin
  result := Self.FormatWithMSec;
end;

{ TIdContextHelper }

procedure TIdContextHelper.FlushBuffer;
var
  buff: TIdBytes;
  OldTimeout: Integer;
begin
  OldTimeout := Self.Connection.IOHandler.ReadTimeout;

  SetLength(buff, 0);
  Self.Connection.IOHandler.ReadTimeout := 10;
  try
    try
      while True do
      begin
        AppendByte(buff, Self.Connection.IOHandler.ReadByte);
        if Length(buff) > 1000 then
          break;
      end;
    except
      // Ignore timeout.
      on E: EIdReadTimeout do;

      on E: Exception do
        raise;
    end;

    if Length(buff) = 0 then
      Exit;

    TLogging.Obj.ApplicationMessage(msDebug, 'FlushBuffer', Format('Port=%d,Len=%d,Msg=%s',
      [Self.PeerPort, Length(buff), IdBytesToHex(buff)]));
  finally
    Self.Connection.IOHandler.ReadTimeout := OldTimeout;
  end;
end;

function TIdContextHelper.GetReadTimeout: Integer;
begin
  result := IOHandler.ReadTimeout;
end;

function TIdContextHelper.IOHandler: TIdIOHandler;
begin
  result := Self.Connection.IOHandler;
end;

function TIdContextHelper.PeerInfo: string;
begin
  result := Format('IP=%s,Port=%d', [Self.PeerIP, Self.PeerPort]);
end;

function TIdContextHelper.PeerIP: string;
begin
  result := Self.Connection.Socket.Binding.PeerIP;
end;

function TIdContextHelper.PeerPort: word;
begin
  result := Self.Connection.Socket.Binding.PeerPort;
end;

function TIdContextHelper.ReadByte: Byte;
begin
  result := IOHandler.ReadByte;
end;

procedure TIdContextHelper.ReadBytes(var VBuffer: TIdBytes; AByteCount: Integer; AAppend: Boolean);
begin
  IOHandler.ReadBytes(VBuffer, AByteCount, AAppend);
end;

procedure TIdContextHelper.SetReadTimeout(const Value: Integer);
begin
  IOHandler.ReadTimeout := Value;
end;

procedure TIdContextHelper.Write(const ABuffer: TIdBytes; const ALength, AOffset: Integer);
begin
  IOHandler.Write(ABuffer, ALength, AOffset);
end;

{ TTimeHelper }

function TTimeHelper.ToString: String;
begin
  result := FormatDateTime('HH:NN:SS', Self);
end;

{ TCustomComboboxHelper }

procedure TCustomComboboxHelper.RightAlignment;
var
  Info: tagCOMBOBOXINFO;
  old_style, new_style: NativeInt;
  rtl: LongBool;
begin
  ZeroMemory(@Info, SizeOf(Info));
  Info.cbSize := SizeOf(Info);

  if not GetComboBoxInfo(Self.Handle, Info) then
    raise Exception.Create('GetComboBoxInfo Error Code - ' + GetLastERror.ToString);

  old_style := GetWindowLong(Info.hwndItem, GWL_STYLE);
  new_style := old_style or ES_RIGHT;
  SetWindowLong(Info.hwndItem, GWL_STYLE, new_style);

  old_style := GetWindowLong(Info.hwndList, GWL_EXSTYLE);
  new_style := old_style or WS_EX_RIGHT;
  SetWindowLong(Info.hwndList, GWL_EXSTYLE, new_style);

  Refresh;
end;

end.
