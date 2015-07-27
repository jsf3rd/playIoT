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
    procedure InitConnecitonPool;
  public
    function GetIdleConenction: TFDConnection;
    procedure ReleaseConnection(AConn: TFDConnection);
  end;

var
  ServerContainer: TServerContainer;

implementation

uses Winapi.Windows, _smDataLoader, _smDataProvider, JdcView2,
  MyGlobal, MyOption;

{$R *.dfm}

var
  SessionEvent: TDSSessionEvent;

procedure TServerContainer.InitConnecitonPool;
var

  ConnStr: string;
  DefName: string;
  DriverName: string;
begin
  ConnStr := TOption.Obj.DBInfo;
  DefName := 'Test_Pooled'; // Unique Name
  DriverName := 'MSSQL';

  FConnectionPool := TJdcConnectionPool.Create(ConnStr, DefName, DriverName);
end;

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  DSServer.ChannelResponseTimeout := 5000;

  TDSCallbackTunnelManager.Instance.AddTunnelEvent(OnChannelStateChanged);
  TDSSessionManager.Instance.AddSessionEvent(SessionEvent);

  DSTCPServerTransport.Port := TOption.Obj.TcpPort;
  DSHTTPService.HttpPort := TOption.Obj.HttpPort;
  DSServer.Start;

  TView.Obj.sp_SyncMessage('StartDSServer',
    IntToStr(DSTCPServerTransport.Port));

  InitConnecitonPool;
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

procedure TServerContainer.DSServerConnect(DSConnectEventObject
  : TDSConnectEventObject);
var
  UserIP, SessionID: string;
begin
  if DSConnectEventObject.ChannelInfo = nil then
    UserIP := 'WebUser'
  else
    UserIP := DSConnectEventObject.ChannelInfo.ClientInfo.IpAddress;

  SessionID := IntToStr(TDSSessionManager.Instance.GetThreadSession.Id);
  TDSSessionManager.Instance.GetThreadSession.PutData('IP', UserIP);

  TView.Obj.sp_SyncMessage('UserConnected', UserIP + ' (' + SessionID + ')');
end;

function TServerContainer.GetIdleConenction: TFDConnection;
begin
  result := FConnectionPool.GetIdleConnection;
end;

procedure TServerContainer.OnChannelStateChanged(Sender: TObject;
  const EventItem: TDSCallbackTunnelEventItem);
begin
  TView.Obj.sp_SyncMessage('ChannelStateChanged',
    IntToStr(DSServer.GetAllChannelClientId(CHANNEL_DEFAULT).Count));
end;

procedure TServerContainer.ReleaseConnection(AConn: TFDConnection);
begin
  FConnectionPool.ReleaseConnection(AConn);
end;

initialization

SessionEvent :=
    procedure(Sender: TObject; const EventType: TDSSessionEventType;
    const Session: TDSSession)
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
          TView.Obj.sp_SyncMessage('UserDisconnected', UserIP + SessionID);
        end;
    end;

  end;

end.
