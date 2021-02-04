unit Core;

interface

uses Classes, SysUtils, System.IOUtils, JdcGlobal;

type
  TCore = class
  private
    FIsInitialized: boolean;
    FIsfinalized: boolean;

    constructor Create;
  public
    class function Obj: TCore;

    /// TCore에서 사용하는 객체들에 대한 초기화.
    procedure Initialize;

    /// TCore에서 사용하는 객체들에 대한 종료 처리.
    procedure Finalize;
  end;

implementation

uses MyGlobal, MyOption, JdcView, MyCommon;

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
  if FIsfinalized then
    Exit;
  FIsfinalized := true;

  TView.Obj.sp_Terminate;

  TGlobal.Obj.Finalize;
end;

procedure TCore.Initialize;
begin
  if FIsfinalized then
    Exit;
  if FIsInitialized then
    Exit;
  FIsInitialized := true;

  TGlobal.Obj.Initialize;

  TView.Obj.sp_SyncMessage('Init');
end;

class function TCore.Obj: TCore;
begin
  if MyObj = nil then
    MyObj := TCore.Create;
  result := MyObj;
end;

initialization

TCore.Obj;

finalization

FreeAndNilEx(MyObj);

end.
