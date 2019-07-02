unit MyOption;

interface

uses
  Classes, SysUtils, System.IniFiles, Registry, Winapi.Windows, JdcGlobal, MyGlobal;

type
  TOption = class
  private
    FIniFile: TCustomIniFile;
    constructor Create;

  private
    function GetInterval: Integer;
    procedure SetInterval(const Value: Integer);
    function GetUseCloudLog: boolean;
    procedure SetUseCloudLog(const Value: boolean);
    function GetAppCode: string;
    function GetProjectCode: string;
    procedure SetAppCode(const Value: string);
    procedure SetProjectCode(const Value: string);
    function GetLogServer: TConnInfo;
    procedure SetLogServer(const Value: TConnInfo);
    function GetUseDebug: boolean;
    procedure SetUseDebug(const Value: boolean);

  public
    class function Obj: TOption;
    destructor Destroy; override;

    property Interval: Integer read GetInterval write SetInterval;

    property AppCode: string read GetAppCode write SetAppCode;
    property LogServer: TConnInfo read GetLogServer write SetLogServer;
    property ProjectCode: string read GetProjectCode write SetProjectCode;
    property UseCloudLog: boolean read GetUseCloudLog write SetUseCloudLog;
    property UseDebug: boolean read GetUseDebug write SetUseDebug;
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

  // FIniFile := TMemIniFile.Create(FileName,TEncoding.UTF8);

  // Registry...
  // FileName:= 'SOFTWARE\PlayIoT\' + PROJECT_CODE;
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

function TOption.GetAppCode: string;
begin
  result := FIniFile.ReadString('Config', 'AppCode', SERVICE_CODE);
end;

function TOption.GetInterval: Integer;
begin
  result := FIniFile.ReadInteger('Config', 'Inverval', 1000);
end;

function TOption.GetLogServer: TConnInfo;
begin
  result.StringValue := FIniFile.ReadString('CloudLog', 'IP', '');
  result.IntegerValue := FIniFile.ReadInteger('CloudLog', 'Port', 8094);
end;

function TOption.GetProjectCode: string;
begin
  result := FIniFile.ReadString('Config', 'ProjectCode', PROJECT_CODE);
end;

function TOption.GetUseCloudLog: boolean;
begin
  result := FIniFile.ReadBool('CloudLog', 'Enable', False);
end;

function TOption.GetUseDebug: boolean;
begin
  result := FIniFile.ReadBool('Config', 'UseDebug', False);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create;
  end;
  result := MyObj;
end;

procedure TOption.SetAppCode(const Value: string);
begin
  FIniFile.WriteString('Config', 'AppCode', Value);
end;

procedure TOption.SetInterval(const Value: Integer);
begin
  FIniFile.WriteInteger('Config', 'Inverval', Value);
end;

procedure TOption.SetLogServer(const Value: TConnInfo);
begin
  FIniFile.WriteString('CloudLog', 'IP', Value.StringValue);
  FIniFile.WriteInteger('CloudLog', 'Port', Value.IntegerValue);
end;

procedure TOption.SetProjectCode(const Value: string);
begin
  FIniFile.WriteString('Config', 'ProjectCode', Value);
end;

procedure TOption.SetUseCloudLog(const Value: boolean);
begin
  FIniFile.WriteBool('CloudLog', 'Enable', Value);
end;

procedure TOption.SetUseDebug(const Value: boolean);
begin
  FIniFile.WriteBool('Config', 'UseDebug', Value);
end;

end.
