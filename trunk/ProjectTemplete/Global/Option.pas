unit Option;

interface

uses
  Classes, SysUtils, JdcOption, Global;

type
  TOption = class(TOptionAbstract)
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
    MyObj.IniName := ChangeFileExt(TGlobal.Obj.LogName, '.ini');
  end;
  result := MyObj;
end;

end.
