unit JdcGlobal.ClassHelper;

interface

uses
  Classes, SysUtils, Windows, System.JSON, Vcl.ExtCtrls;

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

{ TTimerHelper }

procedure TTimerHelper.Reset;
begin
  Self.Enabled := False;
  Self.Enabled := true;
end;

end.
