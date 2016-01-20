unit Core;

interface

uses Classes, SysUtils, System.IOUtils, Generics.Collections, Generics.Defaults,
  MyGlobal, System.Threading;

type
  TCore = class
  private
    FIsInitialized: boolean;
    FIsfinalized: boolean;

    FThread1: TThread;

    constructor Create;
  public
    class function Obj: TCore;

    procedure Initialize; // TCore에서 사용하는 객체들에 대한 초기화.
    procedure Finalize; // TCore에서 사용하는 객체들에 대한 종료 처리.

    procedure Start; // 작업 시작.
  end;

implementation

uses MyOption, JdcView2, MyCommon, JdcGlobal.ClassHelper;

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

  // Terminate Threads...
  FThread1.Terminate;
  FThread1.WaitFor;
  FreeAndNil(FThread1);
end;

procedure TCore.Initialize;
begin
  if FIsfinalized then
    Exit;

  if FIsInitialized then
    Exit;
  FIsInitialized := true;

  // Create Threads...
  FThread1 := TThread.CreateAnonymousThread(
    procedure
    begin
      while not TThread.CurrentThread.CheckTerminated do
      begin
        TView.Obj.sp_DebugMessage(Now.FormatWithoutMSec);
        Sleep(TOption.Obj.Interval);
      end;
    end);
  FThread1.FreeOnTerminate := false;

  TView.Obj.sp_SyncMessage('Init');
end;

class function TCore.Obj: TCore;
begin
  if MyObj = nil then
    MyObj := TCore.Create;
  result := MyObj;
end;

procedure TCore.Start;
begin
  // Start Thread
  FThread1.Start;
end;

end.
