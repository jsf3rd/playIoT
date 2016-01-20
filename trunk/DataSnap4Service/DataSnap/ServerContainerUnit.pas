unit ServerContainerUnit;

interface

uses System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Datasnap.DSTCPServerTransport,
  Datasnap.DSHTTPCommon, Datasnap.DSHTTP,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  Datasnap.DSAuth, IPPeerServer, Registry, ValueList, Winapi.Windows;

type
  TDataSnapSvc = class(TService)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    DSHTTPService: TDSHTTPService;
    dscDataProvider: TDSServerClass;
    procedure dscDataProviderGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
  private
    function GetExeName: String;
  protected
    function DoStop: Boolean; override;
    function DoPause: Boolean; override;
    function DoContinue: Boolean; override;
    procedure DoInterrogate; override;
  public
    function GetServiceController: TServiceController; override;
  published
    procedure rp_ErrorMessage(APacket: TValueList);
    procedure rp_LogMessage(APacket: TValueList);
    procedure rp_DebugMessage(APacket: TValueList);
  end;

var
  DataSnapSvc: TDataSnapSvc;

implementation

{$R *.dfm}

uses _smDataProvider, JdcGlobal, JdcView2, MyGlobal;

procedure TDataSnapSvc.dscDataProviderGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := _smDataProvider.TsmDataProvider;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  DataSnapSvc.Controller(CtrlCode);
end;

function TDataSnapSvc.GetExeName: String;
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

function TDataSnapSvc.GetServiceController: TServiceController;
begin
  result := ServiceController;
end;

procedure TDataSnapSvc.rp_DebugMessage(APacket: TValueList);
begin
  PrintDebug('::DEBUG:: ' + APacket.Values['Msg']);
end;

procedure TDataSnapSvc.rp_ErrorMessage(APacket: TValueList);
begin
  LogMessage(APacket.Values['Msg'] + ', ' + APacket.Values['ErrorMsg']);
  // PrintLog(TGlobal.Obj.LogName, '<ERR> ' + APacket.Values['Msg'] + ', ' +
  // APacket.Values['ErrorMsg']);
end;

procedure TDataSnapSvc.rp_LogMessage(APacket: TValueList);
begin
  PrintDebug('::LOG:: ' + APacket.Values['Msg']);
  // PrintLog(TGlobal.Obj.LogName, '<LOG> ' + APacket.Values['Msg']);
end;

function TDataSnapSvc.DoContinue: Boolean;
begin
  result := inherited;
  DSServer.Start;
end;

procedure TDataSnapSvc.DoInterrogate;
begin
  inherited;
end;

function TDataSnapSvc.DoPause: Boolean;
begin
  DSServer.Stop;
  result := inherited;
end;

function TDataSnapSvc.DoStop: Boolean;
begin
  DSServer.Stop;
  result := inherited;
end;

procedure TDataSnapSvc.ServiceAfterInstall(Sender: TService);
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

procedure TDataSnapSvc.ServiceCreate(Sender: TObject);
begin
  Self.Name := SERVICE_CODE;
  Self.DisplayName := SERVICE_NAME;
end;

procedure TDataSnapSvc.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    // Main Process Code

    Sleep(1);
    ServiceThread.ProcessRequests(false);
  end;
end;

procedure TDataSnapSvc.ServiceShutdown(Sender: TService);
begin
  TView.Obj.Remove(Self);
end;

procedure TDataSnapSvc.ServiceStart(Sender: TService; var Started: Boolean);
begin
  TGlobal.Obj.ExeName := GetExeName;
  TView.Obj.Add(Self);
  DSServer.Start;
end;

procedure TDataSnapSvc.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  TView.Obj.Remove(Self);
end;

end.
