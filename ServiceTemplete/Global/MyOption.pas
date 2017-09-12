unit MyOption;

interface

uses
  Classes, SysUtils, System.IniFiles, Registry,
  Winapi.Windows, MyGlobal;

type
  TOption = class
  private
    FIniFile: TCustomIniFile;
    constructor Create;

  private
    function GetInterval: Integer;
    procedure SetInterval(const Value: Integer);

  public
    class function Obj: TOption;
    destructor Destroy; override;

    property Interval: Integer read GetInterval write SetInterval;
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

function TOption.GetInterval: Integer;
begin
  result := FIniFile.ReadInteger('Config', 'Inverval', 1000);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create;
  end;
  result := MyObj;
end;

procedure TOption.SetInterval(const Value: Integer);
begin
  FIniFile.WriteInteger('Config', 'Inverval', Value);
end;

end.
