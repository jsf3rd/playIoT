unit ServerContainerUnit;

interface

uses System.SysUtils, System.Classes, IPPeerServer, Data.DB,
  Datasnap.DSCommonServer,
  Datasnap.DSServer, Datasnap.DSTCPServerTransport, System.Generics.Collections,
  Datasnap.DSSession, Data.DBXJSON, Datasnap.DSHTTP, JdcConnectionPool,
  FireDAC.Comp.Client;

type
  TServerContainer = class(TDataModule)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    dscDataLoader: TDSServerClass;
    dscDataProvier: TDSServerClass;
    DSHTTPService: TDSHTTPService;
    procedure dscDataLoaderGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure dscDataProvierGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DSServerConnect(DSConnectEventObject: TDSConnectEventObject);
  private
    FConnectionPool: TJdcConnectionPool;
    procedure OnChannelStateChanged(Sender: TObject;
      const EventItem: TDSCallbackTunnelEventItem);
    procedure CreateDBPool;
    procedure _RaiseException(Msg: String; E: Exception);
  public
    function GetIdleConenction: TFDConnection;
  end;

var
  ServerContainer: TServerContainer;

implementation

uses Winapi.Windows, _smDataLoader, _smDataProvider, MyGlobal, MyOption,
  JdcGlobal;

{$R *.dfm}

var
  SessionEvent: TDSSessionEvent;

procedure TServerContainer.CreateDBPool;
var

  ConnStr: string;
  DefName: string;
  DriverName: string;
begin
  ConnStr := TOption.Obj.DBInfo;
  DefName := 'Test_Pooled'; // Unique Name
  DriverName := 'MSSQL';

  if Assigned(FConnectionPool) then
    FreeAndNil(FConnectionPool);

  try
    FConnectionPool := TJdcConnectionPool.Create(TOption.Obj.DBInfo, DefName, DriverName);
    TGlobal.Obj.ApplicationMessage(msDebug, 'CreateDBPool', TOption.Obj.DBInfo);
  except
    on E: Exception do
      _RaiseException('DB Connection Failed.' + TOption.Obj.DBInfo, E);
  end;
end;

procedure TServerContainer._RaiseException(Msg: String; E: Exception);
begin
  TGlobal.Obj.ApplicationMessage(msError, Msg, E.Message);
  raise Exception.Create(Msg + #13#10 + E.Message);
end;

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  DSServer.ChannelResponseTimeout := 5000;

  TDSCallbackTunnelManager.Instance.AddTunnelEvent(OnChannelStateChanged);
  TDSSessionManager.Instance.AddSessionEvent(SessionEvent);

  DSTCPServerTransport.Port := TOption.Obj.TcpPort;
  DSHTTPService.HttpPort := TOption.Obj.HttpPort;
  DSServer.Start;

  TGlobal.Obj.ApplicationMessage(msInfo, 'DSServer', 'Start,TCP:%d, HTTP:%d',
    [DSTCPServerTransport.Port, DSHTTPService.HttpPort]);

  CreateDBPool;
end;

procedure TServerContainer.DataModuleDestroy(Sender: TObject);
begin
  TDSSessionManager.Instance.RemoveSessionEvent(SessionEvent);
  TDSCallbackTunnelManager.Instance.RemoveTunnelEvent(OnChannelStateChanged);

  DSTCPServerTransport.CloseConnections;
  DSServer.Free;

  FConnectionPool.Free;
end;

procedure TServerContainer.dscDataLoaderGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := TsmDataLoader;
end;

procedure TServerContainer.dscDataProvierGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := TsmDataProvider;
end;

procedure TServerContainer.DSServerConnect(DSConnectEventObject: TDSConnectEventObject);
var
  UserIP, SessionID: string;
begin
  if DSConnectEventObject.ChannelInfo = nil then
    UserIP := 'WebUser'
  else
    UserIP := DSConnectEventObject.ChannelInfo.ClientInfo.IpAddress;

  SessionID := IntToStr(TDSSessionManager.Instance.GetThreadSession.Id);
  TDSSessionManager.Instance.GetThreadSession.PutData('IP', UserIP);

  TGlobal.Obj.ApplicationMessage(msInfo, 'UserConnect', 'IP:%s,SessionID:%s',
    [UserIP, SessionID]);
end;

function TServerContainer.GetIdleConenction: TFDConnection;
begin
  if not Assigned(FConnectionPool) then
    raise Exception.Create('DB Connection Failed.');

  try
    result := FConnectionPool.GetIdleConnection;
  except
    on E: Exception do
    begin
      CreateDBPool;
      raise Exception.Create('GetIdleConnection - ' + E.Message);
    end;
  end;
end;

procedure TServerContainer.OnChannelStateChanged(Sender: TObject;
  const EventItem: TDSCallbackTunnelEventItem);
begin
  TGlobal.Obj.ApplicationMessage(msInfo, 'ChannelStateChanged', 'UserCount:%d',
    [DSServer.GetAllChannelClientId(CHANNEL_DEFAULT).Count]);
end;

initialization

SessionEvent :=
    procedure(Sender: TObject; const EventType: TDSSessionEventType; const Session: TDSSession)
  var
    UserIP, SessionID: string;
  begin
    if not Assigned(Session) then
      Exit;

    UserIP := Session.GetData('IP');
    SessionID := ' (' + IntToStr(Session.Id) + ')';

    case EventType of
      SessionCreate:
        // TODO :
        ;
      SessionClose:
        begin
          TGlobal.Obj.ApplicationMessage(msInfo, 'UserDisconnect', 'IP:%s,SessionID:%s',
            [UserIP, SessionID]);
        end;
    end;

  end;

end.
