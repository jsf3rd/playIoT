// *******************************************************
//
// playIoT Class Helper
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
//
// *******************************************************

unit JdcGlobal.ClassHelper;

interface

uses
  Classes, SysUtils, REST.JSON,
  XSuperObject, System.IOUtils, System.Generics.Collections, System.DateUtils, Data.SqlTimSt,
  IdContext

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
  public
    function PeerIP: string;
    function PeerPort: word;
    function PeerInfo: string;
  end;

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetValueEx(const Name: string): TJSONValue;
    function GetString(const Name: string): String;
    function GetInt(const Name: string): Integer;
    function GetDouble(const Name: string): double;
    function GetJSONArray(const Name: string): TJSONArray;
    function GetJSONObject(const Name: string): TJSONObject;

    function AddPair(const Str: string; const Val: Integer): TJSONObject; overload;
    function AddPair(const Str: string; const Val: double): TJSONObject; overload;

    function ToRecord<T: record >: T;
    function ToObject<T: class>: T;

    class function ParseFile(FileName: String): TJSONValue;

    procedure Clear;
  end;

  TJSONHelper = class helper for REST.JSON.TJSON
  public
    class function ObjectToJsonObjectEx(AObject: TObject): TJSONObject;
    class function ObjectToJsonStringEx(AObject: TObject): String;
    class function JsonToObjectEx<T: class>(AJsonObject: TJSONObject): T; overload;
    class function JsonToObjectEx<T: class>(AJson: String): T; overload;
    class function FileToObject<T: class>(FileName: String): T;

    class function RecordToJsonObject<T: record >(ARecord: T): TJSONObject;
    class function RecordToJsonString<T: record >(ARecord: T): String;
    class function JsonToRecord<T: record >(AJsonObject: TJSONObject): T; overload;
    class function JsonToRecord<T: record >(AJson: String): T; overload;
    class function FileToRecord<T: record >(FileName: String): T;
    class function ConvertRecord<T1, T2: record >(ARecord: T1): T2;
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

implementation

{$IFDEF MSWINDOWS}

uses JdcGlobal, JdcGlobal.DSCommon;

{ TTimerHelper }

procedure TTimerHelper.Reset;
begin
  Self.Enabled := False;
  Self.Enabled := true;
end;
{$ENDIF}

{ TJSONObjectHelper }
function TJSONObjectHelper.AddPair(const Str: string; const Val: Integer): TJSONObject;
begin
  if not Str.IsEmpty then
    AddPair(TJSONPair.Create(Str, TJSONNumber.Create(Val)));
  Result := Self;
end;

function TJSONObjectHelper.GetJSONArray(const Name: string): TJSONArray;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    Result := JSONValue as TJSONArray;
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
    Result := JSONValue as TJSONObject;
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
  Result := Self;
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
    Result := (JSONValue as TJSONNumber).AsDouble;
  except
    on E: Exception do
      raise Exception.Create(SysUtils.Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetInt(const Name: string): Integer;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    Result := (JSONValue as TJSONNumber).AsInt;
  except
    on E: Exception do
      raise Exception.Create(SysUtils.Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetString(const Name: string): String;
begin
  Result := GetValueEx(Name).Value;
end;

function TJSONObjectHelper.GetValueEx(const Name: string): TJSONValue;
var
  MyElem: TJSONPair;
  Names: String;
begin
  Result := GetValue(Name);

  if Assigned(Result) then
    Exit;

  Names := '';
  for MyElem in Self do
    Names := Names + MyElem.JsonString.Value + ', ';

  raise Exception.Create(SysUtils.Format('JSON name [%s] is not exist. Other name list [%s]',
    [Name, Names]));
end;

class function TJSONObjectHelper.ParseFile(FileName: String): TJSONValue;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  Result := TJSONObject.ParseJSONValue(JsonString);
end;

function TJSONObjectHelper.ToObject<T>: T;
begin
  Result := REST.JSON.TJSON.JsonToObjectEx<T>(Self);
end;

function TJSONObjectHelper.ToRecord<T>: T;
begin
  Result := REST.JSON.TJSON.JsonToRecord<T>(Self);
end;

{ TJSONHelper }

class function TJSONHelper.JsonToObjectEx<T>(AJsonObject: TJSONObject): T;
begin
  Result := JsonToObjectEx<T>(AJsonObject.ToString);
end;

class function TJSONHelper.ConvertRecord<T1, T2>(ARecord: T1): T2;
var
  JSONObject: TJSONObject;
begin
  JSONObject := RecordToJsonObject<T1>(ARecord);
  Result := JsonToRecord<T2>(JSONObject);
  JSONObject.Free;
end;

class function TJSONHelper.FileToObject<T>(FileName: String): T;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  Result := REST.JSON.TJSON.JsonToObjectEx<T>(JsonString);
end;

class function TJSONHelper.FileToRecord<T>(FileName: String): T;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  Result := REST.JSON.TJSON.JsonToRecord<T>(JsonString);
end;

class function TJSONHelper.JsonToObjectEx<T>(AJson: String): T;
begin
  Result := TSuperObject.Create(AJson).AsType<T>;
end;

class function TJSONHelper.JsonToRecord<T>(AJsonObject: TJSONObject): T;
begin
  Result := JsonToRecord<T>(AJsonObject.ToString);
end;

class function TJSONHelper.JsonToRecord<T>(AJson: String): T;
var
  tmp: string;
begin
  tmp := AJson.Replace('NAN', '0', [rfReplaceAll, rfIgnoreCase]);
  Result := TSuperRecord<T>.FromJSON(tmp);
end;

class function TJSONHelper.ObjectToJsonObjectEx(AObject: TObject): TJSONObject;
var
  JSONStr: String;
begin
  JSONStr := AObject.AsJSON;
  Result := TJSONObject.ParseJSONValue(JSONStr) as TJSONObject;
end;

class function TJSONHelper.ObjectToJsonStringEx(AObject: TObject): String;
begin
  Result := AObject.AsJSON;
end;

class function TJSONHelper.RecordToJsonObject<T>(ARecord: T): TJSONObject;
var
  JsonString: string;
begin
  try
    JsonString := RecordToJsonString<T>(ARecord);
    // JsonString := JsonString.Replace('NAN', '0', [rfReplaceAll, rfIgnoreCase]);
    // JsonString := JsonString.Replace('-NAN', '0', [rfReplaceAll, rfIgnoreCase]);
    Result := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
  except
    on E: Exception do
      raise Exception.Create('RecordToJsonObject,' + E.Message);
  end;
end;

class function TJSONHelper.RecordToJsonString<T>(ARecord: T): String;
begin
  Result := TSuperRecord<T>.AsJSON(ARecord);
end;

{ TDateTimeHelper }

function TDateTimeHelper.Date: string;
begin
  Result := FormatDateTime('YYYY-MM-DD', Self);
end;

function TDateTimeHelper.FormatWithMSec: String;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', Self);
end;

function TDateTimeHelper.FormatWithoutMSec: String;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS', Self);
end;

function TDateTimeHelper.RecodeTenMinute: TDateTime;
var
  Min: Integer;
begin
  Min := MinuteOf(Self);
  Min := (Min div 10) * 10;

  Result := RecodeMilliSecond(Self, 0);
  Result := RecodeSecond(Result, 0);
  Result := RecodeMinute(Result, Min);
end;

function TDateTimeHelper.SQLTimeStamp: TSQLTimeStamp;
begin
  Result := DateTimeToSQLTimeStamp(Self);
end;

function TDateTimeHelper.Time: string;
begin
  Result := FormatDateTime('HH:NN:SS', Self);
end;

function TDateTimeHelper.ToISO8601: String;
begin
  Result := FormatDateTime('YYYY-MM-DD"T"HH:NN:SS.zzz', Self);
end;

function TDateTimeHelper.ToString: String;
begin
  Result := Self.FormatWithMSec;
end;

{ TIdContextHelper }

function TIdContextHelper.PeerInfo: string;
begin
  Result := Format('IP=%s,Port=%d', [Self.PeerIP, Self.PeerPort]);
end;

function TIdContextHelper.PeerIP: string;
begin
  Result := Self.Connection.Socket.Binding.PeerIP;
end;

function TIdContextHelper.PeerPort: word;
begin
  Result := Self.Connection.Socket.Binding.PeerPort;
end;

{ TTimeHelper }

function TTimeHelper.ToString: String;
begin
  Result := FormatDateTime('HH:NN:SS', Self);
end;

end.
