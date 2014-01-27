unit ServerContainerUnit;

interface

uses System.SysUtils, System.Classes, IPPeerServer, Data.DB,
  Datasnap.DSCommonServer,
  Datasnap.DSServer, Datasnap.DSTCPServerTransport, System.Generics.Collections,
  Datasnap.DSSession, Data.DBXJSON;

type
  TServerContainer = class(TDataModule)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    dscDataLoader: TDSServerClass;
    dscDataProvier: TDSServerClass;
    procedure dscDataLoaderGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure dscDataProvierGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
    procedure DSServerConnect(DSConnectEventObject: TDSConnectEventObject);
  private
    FSessions: TList<String>;
    procedure OnChannelStateChanged(Sender: TObject;
      const EventItem: TDSCallbackTunnelEventItem);
  public
  end;

var
  ServerContainer: TServerContainer;

implementation

uses Winapi.Windows, _smDataLoader, _smDataProvider, JdcView,
  Global, Option;

{$R *.dfm}

procedure TServerContainer.DataModuleCreate(Sender: TObject);
begin
  FSessions := TList<String>.Create;

  DSTCPServerTransport.Port := TOption.Obj.DSPort;
  DSServer.Start;

  TView.Obj.sp_SyncMessage('StartDSServer',
    IntToStr(DSTCPServerTransport.Port));

  TDSCallbackTunnelManager.Instance.AddTunnelEvent(OnChannelStateChanged);

  TDSSessionManager.Instance.AddSessionEvent(
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

            // Check Callback Channels..
            ServerContainer.DSServer.BroadcastMessage(CHANNEL_DEFAULT,
              TJSONNull.Create);
          end;
      end;

    end);

end;

procedure TServerContainer.DataModuleDestroy(Sender: TObject);
var
  MyElem: String;
begin
  for MyElem in FSessions do
  begin
    TDSSessionManager.Instance.CloseSession(MyElem);
  end;
  DSTCPServerTransport.Stop;
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
  UserIP := DSConnectEventObject.ChannelInfo.ClientInfo.IpAddress;
  SessionID := IntToStr(TDSSessionManager.Instance.GetThreadSession.Id);
  TDSSessionManager.Instance.GetThreadSession.PutData('IP', UserIP);

  FSessions.Add(SessionID);

  TView.Obj.sp_SyncMessage('UserConnected', UserIP + ' (' + SessionID + ')');

end;

procedure TServerContainer.OnChannelStateChanged(Sender: TObject;
const EventItem: TDSCallbackTunnelEventItem);
begin
  TView.Obj.sp_SyncMessage('ChannelStateChanged',
    IntToStr(DSServer.GetAllChannelClientId(CHANNEL_DEFAULT).Count));
end;

end.
