unit _ServerContainer;

interface

uses System.SysUtils, System.Classes,
  Vcl.SvcMgr,
  Datasnap.DSTCPServerTransport,
  Datasnap.DSHTTPCommon, Datasnap.DSHTTP,
  Datasnap.DSServer, Datasnap.DSCommonServer,
  Datasnap.DSAuth, IPPeerServer, Registry, Winapi.Windows, MyOption,
  JdcConnectionPool, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Error,
  FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Stan.Async, FireDAC.Phys, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, System.JSON;

type
  TServerContainer = class(TService)
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
    procedure ServiceAfterUninstall(Sender: TService);
  private
    FConnectionPool: TJdcConnectionPool;

    procedure _RaiseException(Msg: String; E: Exception);

    procedure ServiceEnd;
    function GetExeName: String;
  protected
    function DoStop: Boolean; override;
    function DoPause: Boolean; override;
    function DoContinue: Boolean; override;
    procedure DoInterrogate; override;
  public
    procedure CreateDBPool;
    function GetIdleConnection: TFDConnection;
    function GetServiceController: TServiceController; override;

    function OpenQuery(AQuery: TFDQuery; AParams: TJSONObject): TStream;
    function ExecQuery(AQuery: TFDQuery; AParams: TJSONObject;
      AProcName: String): Boolean;
    procedure ApplyUpdate(AQuery: TFDQuery; AStream: TStream);
  end;

var
  ServerContainer: TServerContainer;

implementation

{$R *.dfm}

uses _smDataProvider, JdcGlobal, MyGlobal, JdcGlobal.DSCommon;

procedure TServerContainer.dscDataProviderGetClass(DSServerClass
  : TDSServerClass; var PersistentClass: TPersistentClass);
begin
  PersistentClass := _smDataProvider.TsmDataProvider;
end;

function TServerContainer.ExecQuery(AQuery: TFDQuery; AParams: TJSONObject;
  AProcName: String): Boolean;
var
  Conn: TFDConnection;
begin
  result := false;

  Conn := GetIdleConnection;
  try
    AQuery.Connection := Conn;
    try
      AQuery.ParamByJSONObject(AParams, TGlobal.Obj.ApplicationMessage);
      AQuery.ExecSQL;
      result := true;
      TGlobal.Obj.ApplicationMessage(mtLog, AProcName, 'Query=%s,SQL=%s',
        [AQuery.Name, AQuery.SQL.Text]);
    except
      on E: Exception do
      begin
        TGlobal.Obj.ApplicationMessage(mtError, AProcName, E.Message);
        CreateDBPool;
      end;
    end;

  finally
    Conn.Free;
  end;
end;

procedure ServiceController(CtrlCode: DWord); stdcall;
begin
  ServerContainer.Controller(CtrlCode);
end;

function TServerContainer.GetExeName: String;
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

function TServerContainer.GetIdleConnection: TFDConnection;
begin
  if not Assigned(FConnectionPool) then
    raise Exception.Create('DB Connection Failed.');

  result := FConnectionPool.GetIdleConnection;
end;

function TServerContainer.GetServiceController: TServiceController;
begin
  result := ServiceController;
end;

procedure TServerContainer.ApplyUpdate(AQuery: TFDQuery; AStream: TStream);
var
  Conn: TFDConnection;
  rlt: integer;
begin
  Conn := GetIdleConnection;
  try
    AQuery.Connection := Conn;
    AQuery.LoadFromDSStream(AStream);

    if AQuery.IsEmpty then
      Exit;

    rlt := AQuery.ApplyUpdates;
    TGlobal.Obj.ApplicationMessage(mtLog, 'ApplyUpdates',
      'Query=%s,UpdateCount=%d', [AQuery.Name, rlt]);
  finally
    Conn.Free;
  end;
end;

function TServerContainer.OpenQuery(AQuery: TFDQuery;
  AParams: TJSONObject): TStream;
var
  Conn: TFDConnection;
begin
  Conn := GetIdleConnection;
  try
    AQuery.Connection := Conn;

    if Assigned(AParams) then
      AQuery.ParamByJSONObject(AParams, TGlobal.Obj.ApplicationMessage);

    try
      result := AQuery.ToStream;
    except
      on E: Exception do
      begin
        CreateDBPool;
        raise E;
      end;
    end;

    TGlobal.Obj.ApplicationMessage(mtDebug, 'OpenQuery', 'Query=%s,SQL=%s',
      [AQuery.Name, AQuery.SQL.Text]);
  finally
    Conn.Free;
  end;
end;

procedure TServerContainer.CreateDBPool;
var
  DefName: string;
  DriverName: string;
begin
  DefName := 'DataSnap';
  DriverName := 'PG';

  if Assigned(FConnectionPool) then
    FreeAndNil(FConnectionPool);

  try
    FConnectionPool := TJdcConnectionPool.Create(TOption.Obj.DBInfo, DefName,
      DriverName);
    TGlobal.Obj.ApplicationMessage(mtDebug, 'CreateDBPool', TOption.Obj.DBInfo);
  except
    on E: Exception do
      _RaiseException('DB Connection Failed.' + TOption.Obj.DBInfo, E);
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

  TGlobal.Obj.ApplicationMessage(mtLog, 'Stop');
  result := inherited;
end;

function TServerContainer.DoStop: Boolean;
begin
  DSServer.Stop;

  TGlobal.Obj.ApplicationMessage(mtLog, 'Stop');
  result := inherited;
end;

procedure TServerContainer.ServiceAfterInstall(Sender: TService);
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
  TGlobal.Obj.ApplicationMessage(mtLog, 'Installed', SERVICE_NAME);
end;

procedure TServerContainer.ServiceAfterUninstall(Sender: TService);
begin
  TGlobal.Obj.ApplicationMessage(mtLog, 'Uninstalled', SERVICE_NAME);
end;

procedure TServerContainer.ServiceCreate(Sender: TObject);
begin
  Self.Name := SERVICE_CODE;
  Self.DisplayName := SERVICE_NAME;
end;

procedure TServerContainer.ServiceEnd;
begin
  TGlobal.Obj.Finalize;
  if Assigned(FConnectionPool) then
    FreeAndNil(FConnectionPool);
end;

procedure TServerContainer.ServiceExecute(Sender: TService);
begin
  while not Terminated do
  begin
    // Main Process Code

    Sleep(1);
    ServiceThread.ProcessRequests(false);
  end;
end;

procedure TServerContainer.ServiceShutdown(Sender: TService);
begin
  ServiceEnd;
end;

procedure TServerContainer.ServiceStart(Sender: TService; var Started: Boolean);
begin
  TGlobal.Obj.ExeName := GetExeName;

  CreateDBPool;
  TGlobal.Obj.Initialize;

  DSTCPServerTransport.Port := TOption.Obj.TcpPort;
  DSHTTPService.HttpPort := TOption.Obj.HttpPort;
  DSServer.Start;

  TGlobal.Obj.ApplicationMessage(mtLog, 'Start', 'TCP=%d,HTTP=%d',
    [DSTCPServerTransport.Port, DSHTTPService.HttpPort]);
end;

procedure TServerContainer.ServiceStop(Sender: TService; var Stopped: Boolean);
begin
  ServiceEnd;
end;

procedure TServerContainer._RaiseException(Msg: String; E: Exception);
begin
  TGlobal.Obj.ApplicationMessage(mtError, Msg, E.Message);
  raise Exception.Create(Msg + #13#10 + E.Message);
end;

end.
