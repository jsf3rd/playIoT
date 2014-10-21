unit Core;

interface

uses Classes, SysUtils, System.IOUtils, Generics.Collections, Generics.Defaults,
  Global;

type
  TCore = class
  private
    FIsInitialized: boolean;
    FIsfinalized: boolean;

    constructor Create;
  public
    class function Obj: TCore;

    procedure Initialize;
    /// TCore���� ����ϴ� ��ü�鿡 ���� �ʱ�ȭ.
    procedure Finalize;
    /// TCore���� ����ϴ� ��ü�鿡 ���� ���� ó��.

  end;

implementation

uses Option, JdcView2, Common;

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

end;

procedure TCore.Initialize;
begin
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
