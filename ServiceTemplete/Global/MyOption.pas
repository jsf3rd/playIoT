unit MyOption;

interface

uses
  Classes, SysUtils, JdcOption, MyGlobal;

type
  TOption = class(TOptionRegistry)
  private
    function GetInterval: Integer;
    procedure SetInterval(const Value: Integer);

  public

    property Interval: Integer read GetInterval write SetInterval;

    class function Obj: TOption;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetInterval: Integer;
begin
  result := GetIntegerValue('Config', 'Inverval', 1000);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create(nil);
    MyObj.Path := '\ENBGROUP\MyProject';
  end;
  result := MyObj;
end;

procedure TOption.SetInterval(const Value: Integer);
begin
  SetIntegerValue('Config', 'Inverval', Value);
end;

end.
