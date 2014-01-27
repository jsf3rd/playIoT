unit Option;

interface

uses
  Classes, SysUtils, JdcOption, Global;

type
  TOption = class(TOptionAbstract)
  private
    function GetDBInfo: String;
    procedure SetDBInfo(ADBInfo: String);
    function GetDSPort: Integer;
    procedure SetDSPort(const Value: Integer);
    function GetConnStateLog: boolean;
    procedure SetConnStateLog(const Value: boolean);
    function GetMeaDataLog: boolean;
    procedure SetMeaDataLog(const Value: boolean);
    function GetEventLog: boolean;
    procedure SetEventLog(const Value: boolean);
    function GetErrorLog: boolean;
    procedure SetErrorLog(const Value: boolean);

  public
    class function Obj: TOption;

    property DSPort: Integer read GetDSPort write SetDSPort;
    property DBInfo: String read GetDBInfo write SetDBInfo;
    property ConnStateLog: boolean read GetConnStateLog write SetConnStateLog;
    property MeaDataLog: boolean read GetMeaDataLog write SetMeaDataLog;
    property EventLog: boolean read GetEventLog write SetEventLog;
    property ErrorLog: boolean read GetErrorLog write SetErrorLog;

  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetConnStateLog: boolean;
begin
  result := GetBoolValue('ViewLog', 'ConnStateLog', True);
end;

function TOption.GetMeaDataLog: boolean;
begin
  result := GetBoolValue('ViewLog', 'MeaDataLog', False);
end;

function TOption.GetDBInfo: String;
begin
  result := GetStringValue('DB', 'Params', '');
end;

function TOption.GetDSPort: Integer;
begin
  result := GetIntegerValue('DSServer', 'Port', 211);
end;

function TOption.GetErrorLog: boolean;
begin
  result := GetBoolValue('ViewLog', 'ErrorLog', True);
end;

function TOption.GetEventLog: boolean;
begin
  result := GetBoolValue('ViewLog', 'EventLog', False);
end;

procedure TOption.SetConnStateLog(const Value: boolean);
begin
  SetBoolValue('ViewLog', 'ConnStateLog', Value);
end;

procedure TOption.SetMeaDataLog(const Value: boolean);
begin
  SetBoolValue('ViewLog', 'MeaDataLog', Value);
end;

procedure TOption.SetDBInfo(ADBInfo: String);
begin
  SetStringValue('DB', 'Params', ADBInfo);
end;

procedure TOption.SetDSPort(const Value: Integer);
begin
  SetIntegerValue('DSServer', 'Port', Value);
end;

procedure TOption.SetErrorLog(const Value: boolean);
begin
  SetBoolValue('ViewLog', 'ErrorLog', Value);
end;

procedure TOption.SetEventLog(const Value: boolean);
begin
  SetBoolValue('ViewLog', 'EventLog', Value);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create(nil);
    MyObj.FExeName := TGlobal.Obj.ExeName;
  end;
  result := MyObj;
end;

end.
