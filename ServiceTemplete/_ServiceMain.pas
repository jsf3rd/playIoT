unit _ServiceMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Registry, Vcl.ExtCtrls,
  Vcl.AppEvnts;

type
  TServiceMain = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterUninstall(Sender: TService);
  private
    procedure ServiceEnd;
  public
    function GetServiceController: TServiceController; override;
  end;

var
  ServiceMain: TServiceMain;

implementation

{$R *.dfm}

uses JdcGlobal, MyGlobal, MyOption, Core;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServiceMain.Controller(CtrlCode);
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
    if Reg.OpenKey('\SYSTEM\CurrentControlSet\Services\' + Self.Name, false) then
    begin
      Reg.WriteString('Description', SERVICE_DESCRIPTION);
      Reg.CloseKey;
    end;
  finally
    Reg.Free;
  end;

  TGlobal.Obj.ExeName := ParamStr(0);
  TGlobal.Obj.ApplicationMessage(msWarning, 'Installed', SERVICE_NAME);
end;

procedure TServiceMain.ServiceAfterUninstall(Sender: TService);
begin
  TGlobal.Obj.ApplicationMessage(msWarning, 'Uninstalled', SERVICE_NAME);
end;

procedure TServiceMain.ServiceCreate(Sender: TObject);
begin
  Self.Name := SERVICE_CODE;
  Self.DisplayName := SERVICE_NAME;
  TGlobal.Obj.ExeName := ParamStr(0);
end;

procedure TServiceMain.ServiceEnd;
begin
  TCore.Obj.Finalize;
end;

procedure TServiceMain.ServiceExecute(Sender: TService);
begin
  if TOption.Obj.LogServer.StringValue.IsEmpty then
  begin
    TGlobal.Obj.ApplicationMessage(msError, 'Log', 'License Expired');
    DoStop;
  end;

  while not Terminated do
  begin
    // Main Process Code

    Sleep(31);
    ServiceThread.ProcessRequests(false);
  end;
end;

procedure TServiceMain.ServiceShutdown(Sender: TService);
begin
  TGlobal.Obj.ApplicationMessage(msInfo, 'ServiceShutdown');
  ServiceEnd;
end;

procedure TServiceMain.ServiceStart(Sender: TService; var Started: Boolean);
begin
  if TOption.Obj.LogServer.StringValue.IsEmpty then
    Exit;

  TCore.Obj.Initialize;
  TCore.Obj.Start;
end;

procedure TServiceMain.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  ServiceEnd;
end;

end.
