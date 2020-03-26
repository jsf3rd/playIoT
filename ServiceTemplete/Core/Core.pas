unit Core;

interface

uses Classes, SysUtils, System.IOUtils, Generics.Collections, Generics.Defaults,
  System.Threading, System.DateUtils;

type
  TCore = class
  private
    FIsInitialized: boolean;
    FIsfinalized: boolean;

    FMyTask: TThread;

    constructor Create;
  public
    class function Obj: TCore;

    procedure Initialize; // TCore에서 사용하는 객체들에 대한 초기화.
    procedure Finalize; // TCore에서 사용하는 객체들에 대한 종료 처리.

    procedure Start; // 작업 시작.
  end;

implementation

uses MyGlobal, MyOption, MyCommon, JdcGlobal.ClassHelper, JdcGlobal;

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
  if Assigned(FMyTask) then
  begin
    FMyTask.Terminate;
    FMyTask.WaitFor;
    FreeAndNil(FMyTask);
  end;

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

  // Create Threads...
  FMyTask := TThread.CreateAnonymousThread(
    procedure
    begin
      while not TThread.CurrentThread.CheckTerminated do
      begin
        TGlobal.Obj.ApplicationMessage(msDebug, 'TimeStamp', Now.FormatWithoutMSec);
        Sleep(TOption.Obj.Interval);

        if SecondOf(Now) = 0 then
        begin
          // raise Exception.Create('Task - 0 Seconds Error');
        end;

      end;
    end);
  FMyTask.FreeOnTerminate := false;
end;

class function TCore.Obj: TCore;
begin
  if MyObj = nil then
    MyObj := TCore.Create;
  result := MyObj;
end;

procedure TCore.Start;
begin
  FMyTask.Start;
end;

initialization

MyObj := TCore.Create;

end.
