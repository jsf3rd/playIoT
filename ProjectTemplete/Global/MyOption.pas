unit MyOption;

interface

uses
  Classes, SysUtils, JdcOption, MyGlobal;

type
  // TOption = class(TOptionIniFiles)
  TOption = class(TOptionRegistry)
  private
  public
    class function Obj: TOption;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create(nil);
    // MyObj.Path := ChangeFileExt(TGlobal.Obj.LogName, '.ini'); // ini
    MyObj.Path := '\ENBGROUP\MyProject'; // registry
  end;
  result := MyObj;
end;

end.
