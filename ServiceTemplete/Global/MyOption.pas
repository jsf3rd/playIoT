unit MyOption;

interface

uses
  Classes, SysUtils, System.IniFiles, Registry, Winapi.Windows, JdcGlobal, JdcOption, MyGlobal;

type
  TOption = class(TOptionAbstract)
  private
    function GetInterval: Integer;
    procedure SetInterval(const Value: Integer);
    function GetAppCode: string;
  public
    class function Obj: TOption;

    property Interval: Integer read GetInterval write SetInterval;
    property AppCode: string read GetAppCode;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetAppCode: string;
begin
  result := FIniFile.ReadString('Config', 'AppCode', SERVICE_CODE);
end;

function TOption.GetInterval: Integer;
begin
  result := FIniFile.ReadInteger('Config', 'Inverval', 1000);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
    MyObj := TOption.Create;
  result := MyObj;
end;

procedure TOption.SetInterval(const Value: Integer);
begin
  FIniFile.WriteInteger('Config', 'Inverval', Value);
end;

initialization

MyObj := TOption.Create;

finalization

MyObj.Free;

end.
