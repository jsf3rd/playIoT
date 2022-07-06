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
    function GetFTPServer: TFTPServer;
    procedure SetFTPServer(const Value: TFTPServer);
    function GetDeleteExtension: string;
    procedure SetDeleteExtension(const Value: string);
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
    property FTPServer: TFTPServer read GetFTPServer write SetFTPServer;
    property DeleteExtension: string read GetDeleteExtension write SetDeleteExtension;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetBackup: string;
begin
  Result := FIniFile.ReadString('Option', 'Backup', '');
end;

function TOption.GetDay: Integer;
begin
  Result := FIniFile.ReadInteger('Option', 'Day', 0);
end;

function TOption.GetDebug: boolean;
begin
  Result := FIniFile.ReadBool('Option', 'Debug', False);
end;

function TOption.GetDeleteExtension: string;
begin
  Result := FIniFile.ReadString('Option', 'DeleteExtension', ' ');
end;

function TOption.GetFTPServer: TFTPServer;
begin
  Result.User := FIniFile.ReadString('FTPServer', 'User', '');
  Result.Password := FIniFile.ReadString('FTPServer', 'Password', '');
  Result.Address := FIniFile.ReadString('FTPServer', 'Address', '');
  Result.Port := FIniFile.ReadString('FTPServer', 'Port', '');
end;

function TOption.GetHour: Integer;
begin
  Result := FIniFile.ReadInteger('Option', 'Hour', 0);
end;

function TOption.GetMonth: Integer;
begin
  Result := FIniFile.ReadInteger('Option', 'Month', 0);
end;

function TOption.GetSearchPattern: string;
begin
  Result := FIniFile.ReadString('Option', 'SearchOption', '');
end;

function TOption.GetSecond: Integer;
begin
  Result := FIniFile.ReadInteger('Option', 'Second', 0);
end;

function TOption.GetSubDir: boolean;
begin
  Result := FIniFile.ReadBool('Option', 'SubDir', True);
end;

function TOption.GetMinute: Integer;
begin
  Result := FIniFile.ReadInteger('Option', 'Minute', 0);
end;

function TOption.GetYear: Integer;
begin
  Result := FIniFile.ReadInteger('Option', 'Year', 0);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
    MyObj := TOption.Create;
  Result := MyObj;
end;

procedure TOption.SetBackup(const Value: string);
begin
  FIniFile.WriteString('Option', 'Backup', Value);
end;

procedure TOption.SetDay(const Value: Integer);
begin
  FIniFile.WriteInteger('Option', 'Day', Value);
end;

procedure TOption.SetDebug(const Value: boolean);
begin
  FIniFile.WriteBool('Option', 'Debug', Value);
end;

procedure TOption.SetDeleteExtension(const Value: string);
begin
  FIniFile.WriteString('Option', 'DeleteExtension', Value);
end;

procedure TOption.SetFTPServer(const Value: TFTPServer);
begin
  FIniFile.WriteString('FTPServer', 'User', Value.User);
  FIniFile.WriteString('FTPServer', 'Password', Value.Password);
  FIniFile.WriteString('FTPServer', 'Address', Value.Address);
  FIniFile.WriteString('FTPServer', 'Port', Value.Port);
end;

procedure TOption.SetHour(const Value: Integer);
begin
  FIniFile.WriteInteger('Option', 'Hour', Value);
end;

procedure TOption.SetMonth(const Value: Integer);
begin
  FIniFile.WriteInteger('Option', 'Month', Value);
end;

procedure TOption.SetSearchPattern(const Value: string);
begin
  FIniFile.WriteString('Option', 'SearchOption', Value);
end;

procedure TOption.SetSecond(const Value: Integer);
begin
  FIniFile.WriteInteger('Option', 'Second', Value);
end;

procedure TOption.SetSubDir(const Value: boolean);
begin
  FIniFile.WriteBool('Option', 'SubDir', Value);
end;

procedure TOption.SetMinute(const Value: Integer);
begin
  FIniFile.WriteInteger('Option', 'Minute', Value);
end;

procedure TOption.SetYear(const Value: Integer);
begin
  FIniFile.WriteInteger('Option', 'Year', Value);
end;

end.
