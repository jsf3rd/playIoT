unit MyOption;

interface

uses
  Classes, SysUtils, MyGlobal, System.IniFiles, Registry, Winapi.Windows, JdcGlobal, JdcOption;

type
  TOption = class(TOptionAbstract)
  private
    function GetAppName: string;
    procedure SetAppName(const Value: string);
    function GetAppCode: string;
  public
    class function Obj: TOption;
    property AppName: string read GetAppName write SetAppName;
    property AppCode: string read GetAppCode;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetAppCode: string;
begin
  result := FIniFile.ReadString('Config', 'AppCode', APPLICATION_CODE);
end;

function TOption.GetAppName: string;
begin
  result := FIniFile.ReadString('Config', 'AppName', APPLICATION_TITLE);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
    MyObj := TOption.Create;
  result := MyObj;
end;

procedure TOption.SetAppName(const Value: string);
begin
  FIniFile.WriteString('Config', 'AppName', Value);
end;

initialization

TOption.Obj;

finalization

FreeAndNilEx(MyObj);

end.
