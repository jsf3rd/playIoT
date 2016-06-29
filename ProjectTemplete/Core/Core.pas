unit Core;

interface

uses Classes, SysUtils, System.IOUtils, Generics.Collections, Generics.Defaults,
  MyGlobal;

type
  TCore = class
  private
    FIsInitialized: boolean;
    FIsfinalized: boolean;

    constructor Create;
  public
    class function Obj: TCore;

    procedure Initialize;
    /// TCore에서 사용하는 객체들에 대한 초기화.
    procedure Finalize;
    /// TCore에서 사용하는 객체들에 대한 종료 처리.

  end;

implementation

uses MyOption, JdcView2, MyCommon;

var
  MyObj: TCore = nil;

  { TCore }
constructor TCore.Create;
begin
  // TODO : Init Core..
  FIsInitialized := false;
  FIsfinalized := false;
end;

procedure TCore.Finalize;
begin
  TGlobal.Obj.Finalize;

  if FIsfinalized then
    Exit;
  FIsfinalized := true;
end;

procedure TCore.Initialize;
begin
  TGlobal.Obj.Initialize;

  if FIsfinalized then
    Exit;

  if FIsInitialized then
    Exit;
  FIsInitialized := true;

  TView.Obj.sp_SyncMessage('Init');
end;

class function TCore.Obj: TCore;
begin
  if MyObj = nil then
    MyObj := TCore.Create;
  result := MyObj;
end;

end.
