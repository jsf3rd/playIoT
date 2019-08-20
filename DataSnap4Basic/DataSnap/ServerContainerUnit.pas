unit ServerContainerUnit;

interface

uses System.SysUtils, System.Classes, IPPeerServer, Data.DB,
  Datasnap.DSCommonServer, System.JSON, REST.JSON, JdcGlobal.ClassHelper, JdcGlobal.DSCommon,
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
    function GetIdleConnection: TFDConnection;

    // OpenQuery
    function OpenInstantQuery(AQuery: TFDQuery; AParams: TJSONObject;
      AProcName: String = ''): TStream;
    function OpenQuery(AQuery: TFDQuery; AParams: TJSONObject; AProcName: String = '')
      : TStream;

    // ExecQuery
    function ExecInstantQuery(AQuery: TFDQuery; AParams: TJSONObject; AProcName: String = '')
      : Boolean; overload;
    function ExecQuery(AQuery: TFDQuery; AParams: TJSONObject; AProcName: String = '')
      : Boolean; overload;

    // Array DML
    function ExecInstantQuery(AQuery: TFDQuery; AParams: TJSONArray; AProcName: String = '')
      : Boolean; overload;
    function ExecQuery(AQuery: TFDQuery; AParams: TJSONArray; AProcName: String = '')
      : Boolean; overload;

    // ApplyUpdate
    function ApplyInstantUpdate(AQuery: TFDQuery; AStream: TStream;
      AProcName: String = ''): Boolean;
    function ApplyUpdate(AQuery: TFDQuery; AStream: TStream; AProcName: String = ''): Boolean;
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

function TServerContainer.GetIdleConnection: TFDConnection;
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

function TServerContainer.ApplyInstantUpdate(AQuery: TFDQuery; AStream: TStream;
  AProcName: String): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := AQuery.Clone;
  try
    result := ServerContainer.ApplyUpdate(MyQuery, AStream, AProcName);
  finally
    MyQuery.Free;
  end;
end;

function TServerContainer.ApplyUpdate(AQuery: TFDQuery; AStream: TStream;
  AProcName: String): Boolean;
var
  Conn: TFDConnection;
  ExecTime: TDateTime;
  Errors: integer;
begin
  Conn := GetIdleConnection;
  try
    try
      AQuery.Connection := Conn;
      AQuery.LoadFromDSStream(AStream);

      ExecTime := Now;
      Errors := AQuery.ApplyUpdates;
      TGlobal.Obj.ApplicationMessage(msInfo, StrDefault(AProcName, 'ApplyUpdates'),
        'Query=%s,ChangeCount=%d,Errors=%d,ExecTime=%s', [AQuery.Name, AQuery.ChangeCount,
        Errors, FormatDateTime('NN:SS.zzz', Now - ExecTime)]);

      result := Errors = 0;
    except
      on E: Exception do
      begin
        TGlobal.Obj.ApplicationMessage(msError, AQuery.Name, E.Message);
        result := false;
      end;
    end;
  finally
    Conn.Free;
  end;
end;

function TServerContainer.OpenInstantQuery(AQuery: TFDQuery; AParams: TJSONObject;
  AProcName: String): TStream;
var
  MyQuery: TFDQuery;
begin
  MyQuery := AQuery.Clone;
  try
    result := ServerContainer.OpenQuery(MyQuery, AParams, AProcName);
  finally
    MyQuery.Free;
  end;
end;

function TServerContainer.OpenQuery(AQuery: TFDQuery; AParams: TJSONObject;
  AProcName: String): TStream;
var
  Conn: TFDConnection;
  ExecTime: TDateTime;
begin
  Conn := GetIdleConnection;
  try
    AQuery.Connection := Conn;
    try
      if Assigned(AParams) then
      begin
        if TGlobal.Obj.UseDebug then
          AQuery.ParamByJSONObject(AParams, TGlobal.Obj.ApplicationMessage)
        else
          AQuery.ParamByJSONObject(AParams);
      end;

      ExecTime := Now;
      result := AQuery.ToStream;
      TGlobal.Obj.ApplicationMessage(msDebug, StrDefault(AProcName, 'OpenQuery'),
        'Query=%s,RecordCount=%d,ExecTime=%s', [AQuery.Name, AQuery.RecordCount,
        FormatDateTime('NN:SS.zzz', Now - ExecTime)]);
    except
      on E: Exception do
      begin
        result := TStream.Create;
        raise Exception.Create('OpenQuery - ' + AQuery.Name + ', ' + E.Message);
      end;
    end;
  finally
    Conn.Free;
  end;
end;

function TServerContainer.ExecInstantQuery(AQuery: TFDQuery; AParams: TJSONObject;
  AProcName: String): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := AQuery.Clone;
  try
    result := ServerContainer.ExecQuery(MyQuery, AParams, AProcName);
  finally
    MyQuery.Free;
  end;
end;

function TServerContainer.ExecInstantQuery(AQuery: TFDQuery; AParams: TJSONArray;
  AProcName: String): Boolean;
var
  MyQuery: TFDQuery;
begin
  MyQuery := AQuery.Clone;
  try
    result := ServerContainer.ExecQuery(MyQuery, AParams, AProcName);
  finally
    MyQuery.Free;
  end;
end;

function TServerContainer.ExecQuery(AQuery: TFDQuery; AParams: TJSONArray;
  AProcName: String): Boolean;
var
  Conn: TFDConnection;
  ExecTime: TDateTime;
  I: integer;
  Params: TJSONObject;
begin
  result := false;
  Conn := GetIdleConnection;
  Conn.StartTransaction;
  try
    AQuery.Connection := Conn;
    try
      AQuery.Params.ArraySize := AParams.Count;
      for I := 0 to AParams.Count - 1 do
      begin
        Params := AParams.Items[I] as TJSONObject;
        AQuery.Tag := I;
        AQuery.ParamByJSONObject(Params);
      end;

      ExecTime := Now;
      AQuery.Execute(AParams.Count);
      TGlobal.Obj.ApplicationMessage(msInfo, StrDefault(AProcName, 'ExecQuery'),
        'Query=%s,Requested=%d,RowsAffected=%d,ExecTime=%s',
        [AQuery.Name, AParams.Count, AQuery.RowsAffected, FormatDateTime('NN:SS.zzz',
        Now - ExecTime)]);
      result := AQuery.RowsAffected = AParams.Count;

      if result then
        Conn.Commit
      else
        Conn.Rollback;
    except
      on E: Exception do
      begin
        Conn.Rollback;
        TGlobal.Obj.ApplicationMessage(msError, AProcName, E.Message);
      end;
    end;
  finally
    Conn.Free;
  end;
end;

function TServerContainer.ExecQuery(AQuery: TFDQuery; AParams: TJSONObject;
  AProcName: String): Boolean;
var
  Conn: TFDConnection;
  ExecTime: TDateTime;
begin
  result := false;
  Conn := GetIdleConnection;
  try
    AQuery.Connection := Conn;
    try
      if Assigned(AParams) then
      begin
        if TGlobal.Obj.UseDebug then
          AQuery.ParamByJSONObject(AParams, TGlobal.Obj.ApplicationMessage)
        else
          AQuery.ParamByJSONObject(AParams);
      end;

      ExecTime := Now;
      AQuery.ExecSQL;
      TGlobal.Obj.ApplicationMessage(msInfo, StrDefault(AProcName, 'ExecQuery'),
        'Query=%s,RowsAffected=%d,ExecTime=%s', [AQuery.Name, AQuery.RowsAffected,
        FormatDateTime('NN:SS.zzz', Now - ExecTime)]);
      result := true;
    except
      on E: Exception do
        TGlobal.Obj.ApplicationMessage(msError, AProcName, E.Message);
    end;
  finally
    Conn.Free;
  end;
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
