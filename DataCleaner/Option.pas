unit Option;

interface

uses
  Classes, SysUtils, JdcOption, Global, System.IniFiles;

type
  TOption = class
  private
    FIniFile: TCustomIniFile;
    constructor Create;

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
    destructor Destroy; override;
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

constructor TOption.Create;
var
  FileName: string;
begin
  // IniFile...
  FileName := ChangeFileExt(TGlobal.Obj.ExeName, '.ini');
  FIniFile := TIniFile.Create(FileName);

  // FIniFile := TMemIniFile.Create(FileName);

  // Registry...
  // FileName := 'SOFTWARE\PlayIoT\' + PROJECT_CODE;
  // FIniFile := TRegistryIniFile.Create(FileName);
  // TRegistryIniFile(FIniFile).RegIniFile.RootKey := HKEY_LOCAL_MACHINE;
  // TRegistryIniFile(FIniFile).RegIniFile.OpenKey(FIniFile.FileName, True);
end;

destructor TOption.Destroy;
begin
  if Assigned(FIniFile) then
    FIniFile.Free;

  inherited;
end;

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
  Result := FIniFile.ReadString('Option', 'SearchOption', '*.dat');
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
  begin
    MyObj := TOption.Create;
  end;
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
