unit _JudicoService;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.SvcMgr, Vcl.Dialogs, Registry, Vcl.ExtCtrls,
  Vcl.AppEvnts, ValueList;

type
  TJudicoService = class(TService)
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
  private
    function GetExeName: String;
  public
    function GetServiceController: TServiceController; override;
  published
    procedure rp_ErrorMessage(APacket: TValueList);
    procedure rp_LogMessage(APacket: TValueList);
  end;

var
  JudicoService: TJudicoService;

implementation

{$R *.dfm}

uses JdcGlobal, MyGlobal, JdcView2, Core, MyOption;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  JudicoService.Controller(CtrlCode);
end;

procedure TJudicoService.ApplicationEventsException(Sender: TObject;
  E: Exception);
begin
  PrintDebug('SYSTEM_ERROR ' + E.Message);
end;

function TJudicoService.GetExeName: String;
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

function TJudicoService.GetServiceController: TServiceController;
begin
  result := ServiceController;
end;

procedure TJudicoService.rp_ErrorMessage(APacket: TValueList);
begin
  LogMessage(APacket.Values['Msg'] + ', ' + APacket.Values['ErrorMsg']);
  // PrintLog(TGlobal.Obj.LogName, '<ERR> ' + APacket.Values['Msg'] + ', ' +
  // APacket.Values['ErrorMsg']);
end;

procedure TJudicoService.rp_LogMessage(APacket: TValueList);
begin
  PrintDebug('::LOG:: ' + APacket.Values['Msg']);
  // PrintLog(TGlobal.Obj.LogName, '<LOG> ' + APacket.Values['Msg']);
end;

procedure TJudicoService.ServiceAfterInstall(Sender: TService);
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

procedure TJudicoService.ServiceCreate(Sender: TObject);
begin
  Self.Name := SERVICE_CODE;
  Self.DisplayName := SERVICE_NAME;
end;

procedure TJudicoService.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    // Main Process Code

    Sleep(1);
    ServiceThread.ProcessRequests(false);
  end;
end;

procedure TJudicoService.ServiceShutdown(Sender: TService);
begin
  TCore.Obj.Finalize;
  TView.Obj.Remove(Self);
end;

procedure TJudicoService.ServiceStart(Sender: TService; var Started: Boolean);
begin
  TGlobal.Obj.ExeName := GetExeName;
  TView.Obj.Add(Self);
  TCore.Obj.Initialize;
  TCore.Obj.Start;
end;

procedure TJudicoService.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  TCore.Obj.Finalize;
  TView.Obj.Remove(Self);
end;

end.
