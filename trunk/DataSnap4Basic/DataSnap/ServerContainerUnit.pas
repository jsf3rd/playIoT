unit ServerContainerUnit;

interface

uses System.SysUtils, System.Classes, IPPeerServer, Data.DB,
  Datasnap.DSCommonServer,
  Datasnap.DSServer, Datasnap.DSTCPServerTransport, System.Generics.Collections,
  Datasnap.DSSession, Data.DBXJSON, Datasnap.DSHTTP;

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
    procedure OnChannelStateChanged(Sender: TObject;
      const EventItem: TDSCallbackTunnelEventItem);
  public
  end;

var
  ServerContainer: TServerContainer;

implementation

uses Winapi.Windows, _smDataLoader, _smDataProvider, JdcView2,
  Global, Option;

{$R *.dfm}

var
  SessionEvent: TDSSessionEvent;

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  TDSCallbackTunnelManager.Instance.AddTunnelEvent(OnChannelStateChanged);
  TDSSessionManager.Instance.AddSessionEvent(SessionEvent);

  DSTCPServerTransport.Port := TOption.Obj.TcpPort;
  DSHTTPService.HttpPort := TOption.Obj.HttpPort;
  DSServer.Start;

  TView.Obj.sp_SyncMessage('StartDSServer',
    IntToStr(DSTCPServerTransport.Port));
end;

procedure TServerContainer.DataModuleDestroy(Sender: TObject);
begin
  TDSSessionManager.Instance.RemoveSessionEvent(SessionEvent);
  TDSCallbackTunnelManager.Instance.RemoveTunnelEvent(OnChannelStateChanged);
  DSServer.Stop;
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

procedure TServerContainer.OnChannelStateChanged(Sender: TObject;
  const EventItem: TDSCallbackTunnelEventItem);
begin
  TView.Obj.sp_SyncMessage('ChannelStateChanged',
    IntToStr(DSServer.GetAllChannelClientId(CHANNEL_DEFAULT).Count));
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
