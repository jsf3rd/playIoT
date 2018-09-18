unit MyOption;

interface

uses
  Classes, SysUtils, MyGlobal, System.IniFiles, Registry, Winapi.Windows, JdcGlobal;

type
  TOption = class
  private
    FIniFile: TCustomIniFile;
    constructor Create;
    function GetAppName: string;
    procedure SetAppName(const Value: string);
    function GetUseCloudLog: boolean;
    procedure SetUseCloudLog(const Value: boolean);
    function GetAppCode: string;
    function GetProjectCode: string;
    procedure SetAppCode(const Value: string);
    procedure SetProjectCode(const Value: string);
    function GetLogServer: TConnInfo;
    procedure SetLogServer(const Value: TConnInfo);
  public
    class function Obj: TOption;

    destructor Destroy; override;

    property IniFile: TCustomIniFile read FIniFile;
    property AppName: string read GetAppName write SetAppName;
    property AppCode: string read GetAppCode write SetAppCode;
    property LogServer: TConnInfo read GetLogServer write SetLogServer;
    property ProjectCode: string read GetProjectCode write SetProjectCode;
    property UseCloudLog: boolean read GetUseCloudLog write SetUseCloudLog;
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
  // FileName:= ''SOFTWARE\PlayIoT\' + PROJECT_CODE;
  // FIniFile := TRegistryIniFile.Create(FileName);
  // TRegistryIniFile(FIniFile).RegIniFile.RootKey := HKEY_CURRENT_USER;
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
  result := FIniFile.ReadString('Config', 'AppCode', APPLICATION_CODE);
end;

function TOption.GetAppName: string;
begin
  result := FIniFile.ReadString('Config', 'AppName', APPLICATION_TITLE);
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

procedure TOption.SetAppName(const Value: string);
begin
  FIniFile.WriteString('Config', 'AppName', Value);
end;

procedure TOption.SetLogServer(const Value: TConnInfo);
begin
  FIniFile.WriteString('CloudLog', 'IP', Value.StringValue);
  FIniFile.WriteInteger('CloudLog', 'Port', Value.IntegerValue);
end;

procedure TOption.SetProjectCode(const Value: string);
begin
  FIniFile.WriteString('Config', 'ProjectCode', ProjectCode);
end;

procedure TOption.SetUseCloudLog(const Value: boolean);
begin
  FIniFile.WriteBool('CloudLog', 'Enable', Value);
end;

end.
