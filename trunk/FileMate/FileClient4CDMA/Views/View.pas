unit View;

interface

uses JdcView;

type
  TView = class(TViewAbstract)
  public
    class function Obj: TView;
  end;

implementation

uses Global;

var
  MyObj: TView = nil;

  { TView }

class function TView.Obj: TView;
begin
  if MyObj = nil then
    MyObj := TView.Create(nil);
  Result := MyObj;
end;

end.
