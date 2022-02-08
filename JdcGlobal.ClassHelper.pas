// *******************************************************
//
// Jdc Class Helper
//
// Copyright(c) 2020.
//
// jsf3rd@nate.com
//
//
// *******************************************************

unit JdcGlobal.ClassHelper;

interface

uses
  Classes, SysUtils, REST.JSON, XSuperObject, System.IOUtils, System.Generics.Collections,
  System.DateUtils, Data.SqlTimSt, IdContext, JdcGlobal, IdGlobal, IdExceptionCore, IdIOHandler,
  JdcLogging

{$IF CompilerVersion  > 26} // upper XE5
    , System.JSON
{$ELSE}
    , Data.DBXJSON, Data.DBXPlatform
{$ENDIF}
{$IFDEF MSWINDOWS}
    , Vcl.ExtCtrls, Winapi.Windows, Vcl.StdCtrls, Vcl.WinXCtrls
{$ENDIF}
    ;

type
{$IFDEF MSWINDOWS}
  TTimerHelper = class helper for TTimer
  public
    procedure Reset;
  end;
{$ENDIF}

  TIdIOHandlerHelper = class helper for TIdIOHandler
  private
  public
    procedure FlushBuffer;
  end;

  TIdContextHelper = class helper for TIdContext
  private
    function GetReadTimeout: Integer;
    procedure SetReadTimeout(const Value: Integer);
  public
    function IOHandler: TIdIOHandler;
    function ReadByte: Byte;
    procedure ReadBytes(var VBuffer: TIdBytes; const AByteCount: Integer;
      const AAppend: Boolean = True);
    procedure Write(const ABuffer: TIdBytes; const ALength: Integer = -1;
      const AOffset: Integer = 0);

    function PeerIP: string;
    function PeerPort: word;
    function PeerInfo: string;

    procedure FlushBuffer;

    property ReadTimeout: Integer read GetReadTimeout write SetReadTimeout;
  end;

  TTimeHelper = record helper for TTime
  public
    function ToString: String;
  end;

  TDateTimeHelper = record helper for System.TDateTime
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

{$IFDEF MSWINDOWS}

  TCustomComboboxHelper = class helper for TCustomCombobox
  public
    procedure RightAlignment;
  end;

  TActivityIndicatorHelper = class helper for TActivityIndicator
  public
    procedure Start;
    procedure Stop;
  end;
{$ENDIF}

  TArrayHelper = class helper for TArray
  public
    class function Print(Value: TArray<String>): string; overload;
    class function Print(Value: TArray<Integer>): string; overload;
    class function Print(Value: TArray<Double>): string; overload;
  end;

  TJSONObjectHelper = class helper for TJSONObject
  public
    function GetValueEx(const Name: string): TJSONValue;
    function GetString(const Name: string; const default: string = ''): String;
    function GetInt(const Name: string): Integer;
    function GetIntDef(const Name: string; const default: Integer = -9999): Integer;
    function GetDouble(const Name: string): Double;
    function GetDoubleDef(const Name: string; const default: Double = -9999): Double;
    function GetJSONArray(const Name: string): TJSONArray;
    function GetJSONObject(const Name: string): TJSONObject;

    function AddPair(const Str: string; const Val: Integer): TJSONObject; overload;
    function AddPair(const Str: string; const Val: Double): TJSONObject; overload;

    function ToRecord<T: record >: T;
    function ToObject<T: class>: T;

    class function ParseFile(const FileName: String): TJSONValue;

    procedure Clear;
  end;

  TJSONArrayHelper = class helper for TJSONArray
  public
  end;

  TJSONHelper = class helper for REST.JSON.TJSON
  public
    class function ObjectToJsonObjectEx(const AObject: TObject): TJSONObject;

    class function ObjectToJsonStringEx(const AObject: TObject): String;
    class function JsonToObjectEx<T: class>(const AJsonObject: TJSONObject): T; overload;
    class function JsonToObjectEx<T: class>(const AJson: String): T; overload;
    class function FileToObject<T: class>(const FileName: String): T;

    class function RecordToJsonObject<T: record >(const ARecord: T): TJSONObject;
    class function RecordToJsonString<T: record >(const ARecord: T): String;
    class procedure RecordToJsonFile<T: record >(const ARecord: T; AName: string;
      const Encoding: TEncoding);
    class function JsonToRecord<T: record >(const AJsonObject: TJSONObject): T; overload;
    class function JsonToRecord<T: record >(const AJson: String): T; overload;
    class function FileToRecord<T: record >(const FileName: String; const Encoding: TEncoding): T;
    class function ConvertRecord<T1, T2: record >(const ARecord: T1): T2;

    class function RecordArrayToJsonArray<T: record >(const RecordArray: TArray<T>): TJSONArray;
    class function JsonArrayToRecordArray<T: record >(const JsonArray: TJSONArray): TArray<T>;
  end;

function ReplaceStringNumber(const AInput: string): string;

implementation

uses JdcGlobal.DSCommon;

function ReplaceStringNumber(const AInput: string): string;
begin
  result := AInput.Replace('NAN', '0', [rfReplaceAll]);
  result := result.Replace('INF', '0', [rfReplaceAll]);
end;

{$IFDEF MSWINDOWS}
{ TTimerHelper }

procedure TTimerHelper.Reset;
begin
  Self.Enabled := False;
  Self.Enabled := True;
end;
{$ENDIF}
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
begin
  Self.IOHandler.FlushBuffer;
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

procedure TIdContextHelper.ReadBytes(var VBuffer: TIdBytes; const AByteCount: Integer;
  const AAppend: Boolean);
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

{$IFDEF MSWINDOWS}
{ TCustomComboboxHelper }

procedure TCustomComboboxHelper.RightAlignment;
var
  Info: tagCOMBOBOXINFO;
  old_style, new_style: NativeInt;
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

{ TActivityIndicatorHelper }

procedure TActivityIndicatorHelper.Start;
begin
  Self.Animate := True;
  Self.Visible := True;
end;

procedure TActivityIndicatorHelper.Stop;
begin
  Self.Visible := False;
  Self.Animate := False;
end;
{$ENDIF}
{ TIdIOHandlerHelper }

procedure TIdIOHandlerHelper.FlushBuffer;
var
  buff: TIdBytes;
  OldTimeout: Integer;
begin
  OldTimeout := Self.ReadTimeout;

  SetLength(buff, 0);
  Self.ReadTimeout := 10;
  try
    try
      while True do
      begin
        AppendByte(buff, Self.ReadByte);
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

    TLogging.Obj.ApplicationMessage(msDebug, 'FlushBuffer', Format('Host=%s:%d,Len=%d,Msg=%s',
      [Self.Host, Self.Port, Length(buff), IdBytesToHex(buff)]));
  finally
    Self.ReadTimeout := OldTimeout;
  end;
end;

{ TArrayHelper }

class function TArrayHelper.Print(Value: TArray<String>): string;
var
  MyElem: String;
begin
  result := '[';
  for MyElem in Value do
    result := result + MyElem + ',';
  result := result + ']';
end;

class function TArrayHelper.Print(Value: TArray<Integer>): string;
var
  MyElem: Integer;
begin
  result := '[';
  for MyElem in Value do
    result := result + MyElem.ToString + ',';
  result := result + ']';
end;

class function TArrayHelper.Print(Value: TArray<Double>): string;
var
  MyElem: Double;
begin
  result := '[';
  for MyElem in Value do
    result := result + MyElem.ToString + ',';
  result := result + ']';
end;

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
      raise Exception.Create
        (System.SysUtils.Format('JSON name [%s] can not cast to TJSONArray. \n %s',
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
        (System.SysUtils.Format('JSON name [%s] can not cast to TJSONObject. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.AddPair(const Str: string; const Val: Double): TJSONObject;
begin
  if not Str.IsEmpty then
    AddPair(TJSONPair.Create(Str, TJSONNumber.Create(Val)));
  result := Self;
end;

procedure TJSONObjectHelper.Clear;
begin
  TDSCommon.ClearJSONObject(Self);
end;

function TJSONObjectHelper.GetDouble(const Name: string): Double;
var
  JSONValue: TJSONValue;
begin
  JSONValue := GetValueEx(Name);
  try
    result := (JSONValue as TJSONNumber).AsDouble;
  except
    on E: Exception do
      raise Exception.Create
        (System.SysUtils.Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
        [Name, JSONValue.ToString]));
  end;
end;

function TJSONObjectHelper.GetDoubleDef(const Name: string; const default: Double): Double;
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
      raise Exception.Create
        (System.SysUtils.Format('JSON name [%s] can not cast to TJSONNumber. \n %s',
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

  raise Exception.Create(System.SysUtils.Format('JSON name [%s] is not exist. Other name list [%s]',
    [Name, Names]));
end;

class function TJSONObjectHelper.ParseFile(const FileName: String): TJSONValue;
var
  JsonString: string;
begin
  JsonString := ReplaceStringNumber(TFile.ReadAllText(FileName, TEncoding.UTF8));
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

class function TJSONHelper.JsonToObjectEx<T>(const AJsonObject: TJSONObject): T;
begin
  result := JsonToObjectEx<T>(AJsonObject.ToString);
end;

class function TJSONHelper.ConvertRecord<T1, T2>(const ARecord: T1): T2;
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

class function TJSONHelper.FileToRecord<T>(const FileName: String; const Encoding: TEncoding): T;
begin
  result := REST.JSON.TJSON.JsonToRecord<T>(TFile.ReadAllText(FileName, Encoding));
end;

class function TJSONHelper.JsonArrayToRecordArray<T>(const JsonArray: TJSONArray): TArray<T>;
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
var
  SuperObj: TSuperObject;
begin
  SuperObj := TSuperObject.Create(AJson);
  try
    result := SuperObj.AsType<T>;
  finally
    SuperObj.Free;
  end;
end;

class function TJSONHelper.JsonToRecord<T>(const AJsonObject: TJSONObject): T;
begin
  result := JsonToRecord<T>(AJsonObject.ToString);
end;

class function TJSONHelper.JsonToRecord<T>(const AJson: String): T;
begin
  result := TSuperRecord<T>.FromJSON(AJson);
end;

class function TJSONHelper.ObjectToJsonObjectEx(const AObject: TObject): TJSONObject;
begin
  result := TJSONObject.ParseJSONValue(ReplaceStringNumber(AObject.AsJSON)) as TJSONObject;
end;

class function TJSONHelper.ObjectToJsonStringEx(const AObject: TObject): String;
begin
  result := AObject.AsJSON;
end;

class function TJSONHelper.RecordArrayToJsonArray<T>(const RecordArray: TArray<T>): TJSONArray;
var
  MyElem: T;
begin
  result := TJSONArray.Create;

  for MyElem in RecordArray do
  begin
    result.Add(RecordToJsonObject(MyElem));
  end;
end;

class procedure TJSONHelper.RecordToJsonFile<T>(const ARecord: T; AName: string;
  const Encoding: TEncoding);
begin
  TFile.WriteAllText(AName, REST.JSON.TJSON.RecordToJsonString<T>(ARecord), Encoding);
end;

class function TJSONHelper.RecordToJsonObject<T>(const ARecord: T): TJSONObject;
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

class function TJSONHelper.RecordToJsonString<T>(const ARecord: T): String;
begin
  result := TSuperRecord<T>.AsJSON(ARecord);
end;

end.
