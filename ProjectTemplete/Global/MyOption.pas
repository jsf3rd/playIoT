unit MyOption;

interface

uses
  Classes, SysUtils, MyGlobal, System.IniFiles, Registry,
  Winapi.Windows;

type
  TOption = class
  private
    FIniFile: TCustomIniFile;
    constructor Create;
    function GetAppName: string;
    procedure SetAppName(const Value: string);
  public
    class function Obj: TOption;

    destructor Destroy; override;

    property IniFile: TCustomIniFile read FIniFile;
    property AppName: string read GetAppName write SetAppName;
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
  FileName := ChangeFileExt(TGlobal.Obj.LogName, '.ini');
  FIniFile := TIniFile.Create(FileName);

  // FIniFile := TMemIniFile.Create(FileName);

  // Registry...
  // FileName:= ''SOFTWARE\PlayIoT\' + PROJECT_CODE;
  // FIniFile := TRegistryIniFile.Create(FileName);
  // TRegistryIniFile(FIniFile).RegIniFile.RootKey := HKEY_CURRENT_USER;
end;

destructor TOption.Destroy;
begin
  if Assigned(FIniFile) then
    FIniFile.Free;

  inherited;
end;

function TOption.GetAppName: string;
begin
  result := FIniFile.ReadString('Config', 'AppName', 'MyApp');
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create;
  end;
  result := MyObj;
end;

procedure TOption.SetAppName(const Value: string);
begin
  FIniFile.WriteString('Config', 'AppName', Value);
end;

end.
