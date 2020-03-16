unit _ServerContainer;

interface

uses System.SysUtils, System.Classes, Vcl.SvcMgr, Datasnap.DSTCPServerTransport, Datasnap.DSHTTPCommon,
  Datasnap.DSHTTP,
  Datasnap.DSServer, Datasnap.DSCommonServer, Datasnap.DSAuth, IPPeerServer, Registry, Winapi.Windows,
  JdcConnectionPool, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf,
  FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, System.JSON, FireDAC.Stan.Param, System.StrUtils, System.Generics.Collections;

type
  TServerContainer = class(TService)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    DSHTTPService: TDSHTTPService;
    dscDataProvider: TDSServerClass;
    procedure dscDataProviderGetClass(DSServerClass: TDSServerClass; var PersistentClass: TPersistentClass);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
  private
    FConnectionPool: TJdcConnectionPool;

    procedure InitCode;
    procedure ServiceEnd;
    procedure _RaiseException(Msg: String; E: Exception);
  protected
    function DoStop: Boolean; override;
    function DoPause: Boolean; override;
    function DoContinue: Boolean; override;
    procedure DoInterrogate; override;
  public
    procedure CreateDBPool;
    function GetIdleConnection: TFDConnection;
    function GetServiceController: TServiceController; override;
  end;

var
  ServerContainer: TServerContainer;

implementation

{$R *.dfm}

uses _smDataProvider, JdcGlobal, MyGlobal, MyOption, JdcGlobal.DSCommon;

procedure TServerContainer.dscDataProviderGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := _smDataProvider.TsmDataProvider;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServerContainer.Controller(CtrlCode);
end;

function TServerContainer.GetIdleConnection: TFDConnection;
begin
  if not Assigned(FConnectionPool) then
    CreateDBPool;

  try
    result := FConnectionPool.GetIdleConnection;
  except
    on E: Exception do
      raise Exception.Create('GetIdleConnection - ' + E.Message);
  end;
end;

function TServerContainer.GetServiceController: TServiceController;
begin
  result := ServiceController;
end;

procedure TServerContainer.InitCode;
  procedure FDQueryToFDMemTab(AQuery: TFDQuery; AMemTab: TFDMemTable);
  var
    Conn: TFDConnection;
  begin
    Conn := FConnectionPool.GetIdleConnection;
    try
      AQuery.Connection := Conn;
      AQuery.Open;

      AMemTab.LoadFromDSStream(AQuery.ToStream);
      AQuery.Close;

      AMemTab.Open;
      TGlobal.Obj.ApplicationMessage(msInfo, AMemTab.Name + ' Record Count', AMemTab.RecordCount.ToString);
    finally
      Conn.Free;
    end;
  end;

begin
  // FDQueryToFDMemTab(qryLogger, mtLogger);
end;

procedure TServerContainer.CreateDBPool;
var
  DefName: string;
  DriverName: string;
begin
  if Assigned(FConnectionPool) then
    Exit;

  DefName := 'DataSnap';
  DriverName := 'PG';
  try
    FConnectionPool := TJdcConnectionPool.Create(TOption.Obj.DBInfo, DefName, DriverName);
    TGlobal.Obj.ApplicationMessage(msDebug, 'CreateDBPool', TOption.Obj.DBInfo);
  except
    on E: Exception do
      raise Exception.Create('CreateDBPool-' + E.Message);
  end;
end;

function TServerContainer.DoContinue: Boolean;
begin
  result := inherited;
  DSServer.Start;
end;

procedure TServerContainer.DoInterrogate;
begin
  inherited;
end;

function TServerContainer.DoPause: Boolean;
begin
  DSServer.Stop;
  result := inherited;
end;

function TServerContainer.DoStop: Boolean;
begin
  DSServer.Stop;
  result := inherited;
end;

procedure TServerContainer.ServiceAfterInstall(Sender: TService);
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

procedure TServerContainer.ServiceAfterUninstall(Sender: TService);
begin
  TGlobal.Obj.ApplicationMessage(msWarning, 'Uninstalled', SERVICE_NAME);
end;

procedure TServerContainer.ServiceCreate(Sender: TObject);
begin
  Self.Name := SERVICE_CODE;
  Self.DisplayName := SERVICE_NAME;
  TGlobal.Obj.ExeName := ParamStr(0);
end;

procedure TServerContainer.ServiceEnd;
begin
  TGlobal.Obj.Finalize;
  if Assigned(FConnectionPool) then
    FreeAndNil(FConnectionPool);
end;

procedure TServerContainer.ServiceExecute(Sender: TService);
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

procedure TServerContainer.ServiceShutdown(Sender: TService);
begin
  ServiceEnd;
end;

procedure TServerContainer.ServiceStart(Sender: TService; var Started: Boolean);
begin
  if TOption.Obj.LogServer.StringValue.IsEmpty then
    Exit;

  TGlobal.Obj.Initialize;
  CreateDBPool;

  DSTCPServerTransport.Port := TOption.Obj.TcpPort;
  DSHTTPService.HttpPort := TOption.Obj.HttpPort;
  try
    DSServer.Start;
  except
    on E: Exception do
      _RaiseException(Format('TCP=%d,HTTP=%d,E=%s', [DSTCPServerTransport.Port, DSHTTPService.HttpPort,
        E.Message]), E);
  end;

  TGlobal.Obj.ApplicationMessage(msInfo, 'DSServer', 'TCP=%d,HTTP=%d',
    [DSTCPServerTransport.Port, DSHTTPService.HttpPort]);

  if Assigned(FConnectionPool) then
    try
      InitCode;
    except
      on E: Exception do
        _RaiseException('DB Test Failed.', E);
    end;
end;

procedure TServerContainer.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  ServiceEnd;
end;

procedure TServerContainer._RaiseException(Msg: String; E: Exception);
begin
  TGlobal.Obj.ApplicationMessage(msError, Msg, E.Message);
  raise Exception.Create(Msg + #13#10 + E.Message);
end;

end.
