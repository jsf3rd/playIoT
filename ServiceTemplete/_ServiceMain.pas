unit _ServiceMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Registry, Vcl.ExtCtrls,
  Vcl.AppEvnts, ValueList;

type
  TServiceMain = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
  private
    function GetExeName: String;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  ServiceMain: TServiceMain;

implementation

{$R *.dfm}

uses JdcGlobal, MyGlobal, Core, MyOption;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServiceMain.Controller(CtrlCode);
end;

function TServiceMain.GetExeName: String;
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Name, false) then
    begin
      result := Reg.ReadString('ImagePath');
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

function TServiceMain.GetServiceController: TServiceController;
begin
  result := ServiceController;
end;

procedure TServiceMain.ServiceAfterInstall(Sender: TService);
var
  Reg: TRegistry;
begin
  Reg := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Self.Name, false)
    then
    begin
      Reg.WriteString('Description', SERVICE_DESCRIPTION);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;
end;

procedure TServiceMain.ServiceCreate(Sender: TObject);
begin
  Self.Name := SERVICE_CODE;
  Self.DisplayName := SERVICE_NAME;
end;

procedure TServiceMain.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    // Main Process Code

    Sleep(1);
    ServiceThread.ProcessRequests(false);
  end;
end;

procedure TServiceMain.ServiceShutdown(Sender: TService);
begin
  TCore.Obj.Finalize;
end;

procedure TServiceMain.ServiceStart(Sender: TService; var Started: Boolean);
begin
  TGlobal.Obj.ExeName := GetExeName;
  TCore.Obj.Initialize;
  TCore.Obj.Start;
end;

procedure TServiceMain.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  TCore.Obj.Finalize;
end;

end.
