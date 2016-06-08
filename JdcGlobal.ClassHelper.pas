unit JdcGlobal.ClassHelper;

interface

uses
  Classes, SysUtils, REST.JSON,
  XSuperObject, System.IOUtils, System.Generics.Collections

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

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetValueEx(const Name: string): TJSONValue;
    function GetString(const Name: string): String;
    function GetInt(const Name: string): integer;
    function GetDouble(const Name: string): double;
    function GetJSONArray(const Name: string): TJSONArray;
    function GetJSONObject(const Name: string): TJSONObject;

    function AddPair(const Str: string; const Val: integer)
      : TJSONObject; overload;

    class function ParseFile(FileName: String): TJSONValue;
  end;

  TJSONHelper = class helper for REST.JSON.TJSON
  public
    class function ObjectToJsonObjectEx(AObject: TObject): TJSONObject;
    class function ObjectToJsonStringEx(AObject: TObject): String;
    class function JsonToObjectEx<T: class>(AJsonObject: TJSONObject)
      : T; overload;
    class function JsonToObjectEx<T: class>(AJson: String): T; overload;
    class function FileToObject<T: class>(FileName: String): T;

    class function RecordToJsonObject<T: record >(ARecord: T): TJSONObject;
    class function RecordToJsonString<T: record >(ARecord: T): String;
    class function JsonToRecord<T: record >(AJsonObject: TJSONObject)
      : T; overload;
    class function JsonToRecord<T: record >(AJson: String): T; overload;
    class function FileToRecord<T: record >(FileName: String): T;
  end;

  TDateTimeHelper = record helper for TDateTime
  public
    function ToString: String;
    function FormatWithMSec: String;
    function FormatWithoutMSec: String;
  end;

implementation

{$IFDEF MSWINDOWS}
{ TTimerHelper }

procedure TTimerHelper.Reset;
begin
  Self.Enabled := False;
  Self.Enabled := true;
end;
{$ENDIF}

{ TJSONObjectHelper }
function TJSONObjectHelper.AddPair(const Str: string; const Val: integer)
  : TJSONObject;
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
      raise Exception.Create
        (Format('JSON name [%s] can not cast to TJSONArray. \n %s',
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
      raise Exception.Create
        (Format('JSON name [%s] can not cast to TJSONObject. \n %s',
        [Name, JSONValue.ToString]));
  end;
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
      raise Exception.Create
        (Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetInt(const Name: string): integer;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    Result := (JSONValue as TJSONNumber).AsInt;
  except
    on E: Exception do
      raise Exception.Create
        (Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
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

  Names := '';
  for MyElem in Self do
  begin
    Names := Names + MyElem.JsonString.Value + ', ';
  end;

  if not Assigned(Result) then
    raise Exception.Create
      (Format('JSON name [%s] is not exist. Other name list [%s]',
      [Name, Names]));
end;

class function TJSONObjectHelper.ParseFile(FileName: String): TJSONValue;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  Result := TJSONObject.ParseJSONValue(JsonString);
end;

{ TJSONHelper }

class function TJSONHelper.JsonToObjectEx<T>(AJsonObject: TJSONObject): T;
begin
  Result := JsonToObjectEx<T>(AJsonObject.ToString);
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
begin
  Result := TSuperRecord<T>.FromJSON(AJson);
end;

class function TJSONHelper.ObjectToJsonObjectEx(AObject: TObject): TJSONObject;
var
  JSONObject: String;
begin
  JSONObject := AObject.AsJSON;
  Result := TJSONObject.ParseJSONValue(JSONObject) as TJSONObject;
end;

class function TJSONHelper.ObjectToJsonStringEx(AObject: TObject): String;
begin
  Result := AObject.AsJSON;
end;

class function TJSONHelper.RecordToJsonObject<T>(ARecord: T): TJSONObject;
var
  JsonString: string;
begin
  JsonString := RecordToJsonString(ARecord);
  JsonString := JsonString.Replace('NAN', '0', [rfReplaceAll, rfIgnoreCase]);
  Result := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
end;

class function TJSONHelper.RecordToJsonString<T>(ARecord: T): String;
var
  SO: ISuperObject;
begin
  SO := TSuperRecord<T>.AsJSONObject(ARecord);
  Result := SO.AsJSON;
  SO := nil;
end;

{ TDateTimeHelper }

function TDateTimeHelper.FormatWithMSec: String;
begin
  Result := Self.ToString;
end;

function TDateTimeHelper.FormatWithoutMSec: String;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS', Self);
end;

function TDateTimeHelper.ToString: String;
begin
  Result := FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', Self);
end;

end.
