unit JdcGlobal.ClassHelper;

interface

uses
  Classes, SysUtils, Windows, System.JSON, Vcl.ExtCtrls, REST.JSON,
  XSuperObject, System.IOUtils;

type
  TTimerHelper = class helper for TTimer
  public
    procedure Reset;
  end;

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetValueEx(const Name: string): TJSONValue;
    function GetString(const Name: string): String;
    function GetInt(const Name: string): integer;
    function GetDouble(const Name: string): double;
    function GetJSONArray(const Name: string): TJSONArray;
    function GetJSONObject(const Name: string): TJSONObject;

    class function ParseFile(FileName: String): TJSONValue;
  end;

  TJSONHelper = class helper for TJSON
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

implementation

{ TJSONObjectHelper }

function TJSONObjectHelper.GetJSONArray(const Name: string): TJSONArray;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);

  try
    result := JSONValue as TJSONArray;
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
    result := JSONValue as TJSONObject;
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
    result := (JSONValue as TJSONNumber).AsDouble;
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
    result := (JSONValue as TJSONNumber).AsInt;
  except
    on E: Exception do
      raise Exception.Create
        (Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetString(const Name: string): String;
begin
  result := GetValueEx(Name).Value;
end;

function TJSONObjectHelper.GetValueEx(const Name: string): TJSONValue;
var
  MyElem: TJSONPair;
  Names: String;
begin
  result := GetValue(Name);

  Names := '';
  for MyElem in Self do
  begin
    Names := Names + MyElem.JsonString.Value + ', ';
  end;

  if not Assigned(result) then
    raise Exception.Create
      (Format('JSON name [%s] is not exist. Other name list [%s]',
      [Name, Names]));
end;

class function TJSONObjectHelper.ParseFile(FileName: String): TJSONValue;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  result := TJSONObject.ParseJSONValue(JsonString);
end;

{ TTimerHelper }

procedure TTimerHelper.Reset;
begin
  Self.Enabled := False;
  Self.Enabled := true;
end;

{ TJSONHelper }

class function TJSONHelper.JsonToObjectEx<T>(AJsonObject: TJSONObject): T;
begin
  result := JsonToObjectEx<T>(AJsonObject.ToString);
end;

class function TJSONHelper.FileToObject<T>(FileName: String): T;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  result := TJSON.JsonToObjectEx<T>(JsonString);
end;

class function TJSONHelper.FileToRecord<T>(FileName: String): T;
var
  JsonString: string;
begin
  JsonString := TFile.ReadAllText(FileName);
  result := TJSON.JsonToRecord<T>(JsonString);
end;

class function TJSONHelper.JsonToObjectEx<T>(AJson: String): T;
begin
  result := TSuperObject.Create(AJson).AsType<T>;
end;

class function TJSONHelper.JsonToRecord<T>(AJsonObject: TJSONObject): T;
begin
  result := JsonToRecord<T>(AJsonObject.ToString);
end;

class function TJSONHelper.JsonToRecord<T>(AJson: String): T;
begin
  result := TSuperRecord<T>.FromJSON(AJson);
end;

class function TJSONHelper.ObjectToJsonObjectEx(AObject: TObject): TJSONObject;
var
  JSONObject: String;
begin
  JSONObject := AObject.AsJSON;
  result := TJSONObject.ParseJSONValue(JSONObject) as TJSONObject;
end;

class function TJSONHelper.ObjectToJsonStringEx(AObject: TObject): String;
begin
  result := AObject.AsJSON;
end;

class function TJSONHelper.RecordToJsonObject<T>(ARecord: T): TJSONObject;
var
  JsonString: string;
begin
  JsonString := RecordToJsonString(ARecord);
  result := TJSONObject.ParseJSONValue(JsonString) as TJSONObject;
end;

class function TJSONHelper.RecordToJsonString<T>(ARecord: T): String;
begin
  result := TSuperRecord<T>.AsJSONObject(ARecord).AsJSON;
end;

end.
