unit Option;

interface

uses
  Classes, SysUtils, JdcOption, Global;

type
  TOption = class(TOptionAbstract)
  private
    function GetYear: Integer;
    procedure SetYear(const Value: Integer);
    function GetDay: Integer;
    function GetMonth: Integer;
    procedure SetDay(const Value: Integer);
    procedure SetMonth(const Value: Integer);
    function GetHour: Integer;
    procedure SetHour(const Value: Integer);
    function GetMinute: Integer;
    procedure SetMinute(const Value: Integer);
    function GetSecond: Integer;
    procedure SetSecond(const Value: Integer);
    function GetBackup: string;
    procedure SetBackup(const Value: string);
    function GetDebug: boolean;
    procedure SetDebug(const Value: boolean);
    function GetSearchPattern: string;
    procedure SetSearchPattern(const Value: string);
    function GetSubDir: boolean;
    procedure SetSubDir(const Value: boolean);
  public
    class function Obj: TOption;

    property Year: Integer read GetYear write SetYear;
    property Month: Integer read GetMonth write SetMonth;
    property Day: Integer read GetDay write SetDay;
    property Hour: Integer read GetHour write SetHour;
    property Minute: Integer read GetMinute write SetMinute;
    property Second: Integer read GetSecond write SetSecond;
    property Backup: string read GetBackup write SetBackup;
    property SubDir: boolean read GetSubDir write SetSubDir;
    property Debug: boolean read GetDebug write SetDebug;
    property SearchPattern: string read GetSearchPattern write SetSearchPattern;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetBackup: string;
begin
  Result := GetStringValue('Option', 'Backup', '');
end;

function TOption.GetDay: Integer;
begin
  Result := GetIntegerValue('Option', 'Day', 0);
end;

function TOption.GetDebug: boolean;
begin
  Result := GetBoolValue('Option', 'Debug', False);
end;

function TOption.GetHour: Integer;
begin
  Result := GetIntegerValue('Option', 'Hour', 0);
end;

function TOption.GetMonth: Integer;
begin
  Result := GetIntegerValue('Option', 'Month', 0);
end;

function TOption.GetSearchPattern: string;
begin
  Result := GetStringValue('Option', 'SearchOption', '*.dat');
end;

function TOption.GetSecond: Integer;
begin
  Result := GetIntegerValue('Option', 'Second', 0);
end;

function TOption.GetSubDir: boolean;
begin
  Result := GetBoolValue('Option', 'SubDir', True);
end;

function TOption.GetMinute: Integer;
begin
  Result := GetIntegerValue('Option', 'Minute', 0);
end;

function TOption.GetYear: Integer;
begin
  Result := GetIntegerValue('Option', 'Year', 0);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create(nil);
    MyObj.FExeName := TGlobal.Obj.ExeName;
  end;
  Result := MyObj;
end;

procedure TOption.SetBackup(const Value: string);
begin
  SetStringValue('Option', 'Backup', Value);
end;

procedure TOption.SetDay(const Value: Integer);
begin
  SetIntegerValue('Option', 'Day', Value);
end;

procedure TOption.SetDebug(const Value: boolean);
begin
  SetBoolValue('Option', 'Debug', Value);
end;

procedure TOption.SetHour(const Value: Integer);
begin
  SetIntegerValue('Option', 'Hour', Value);
end;

procedure TOption.SetMonth(const Value: Integer);
begin
  SetIntegerValue('Option', 'Month', Value);
end;

procedure TOption.SetSearchPattern(const Value: string);
begin
  SetStringValue('Option', 'SearchOption', Value);
end;

procedure TOption.SetSecond(const Value: Integer);
begin
  SetIntegerValue('Option', 'Second', Value);
end;

procedure TOption.SetSubDir(const Value: boolean);
begin
  SetBoolValue('Option', 'SubDir', Value);
end;

procedure TOption.SetMinute(const Value: Integer);
begin
  SetIntegerValue('Option', 'Minute', Value);
end;

procedure TOption.SetYear(const Value: Integer);
begin
  SetIntegerValue('Option', 'Year', Value);
end;

end.
