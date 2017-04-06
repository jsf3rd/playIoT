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
  FireDAC.Comp.Client, System.JSON, FireDAC.Stan.Param, System.StrUtils;

type
  TServerContainer = class(TService)
    DSServer: TDSServer;
    DSTCPServerTransport: TDSTCPServerTransport;
    DSHTTPService: TDSHTTPService;
    dscDataProvider: TDSServerClass;
    DSAuthenticationManager: TDSAuthenticationManager;
    procedure dscDataProviderGetClass(DSServerClass: TDSServerClass;
      var PersistentClass: TPersistentClass);
    procedure ServiceStart(Sender: TService; var Started: Boolean);
    procedure ServiceCreate(Sender: TObject);
    procedure ServiceExecute(Sender: TService);
    procedure ServiceShutdown(Sender: TService);
    procedure ServiceStop(Sender: TService; var Stopped: Boolean);
    procedure ServiceAfterInstall(Sender: TService);
    procedure ServiceAfterUninstall(Sender: TService);
    procedure DSAuthenticationManagerUserAuthenticate(Sender: TObject;
      const Protocol, Context, User, Password: string; var valid: Boolean;
      UserRoles: TStrings);
  private
    FConnectionPool: TJdcConnectionPool;

    procedure InitCode;
    procedure ServiceEnd;
    function GetExeName: String;
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

    function OpenInstantQuery(AQuery: TFDQuery; AParams: TJSONObject;
      AProcName: String = ''): TStream;
    function OpenQuery(AQuery: TFDQuery; AParams: TJSONObject; AProcName: String = '')
      : TStream;
    function ExecInstantQuery(AQuery: TFDQuery; AParams: TJSONObject; AProcName: String = '')
      : Boolean; overload;
    function ExecInstantQuery(AQuery: TFDQuery; AParams: TJSONArray; AProcName: String = '')
      : Boolean; overload;
    function ExecQuery(AQuery: TFDQuery; AParams: TJSONObject; AProcName: String = '')
      : Boolean; overload;
    function ExecQuery(AQuery: TFDQuery; AParams: TJSONArray; AProcName: String = '')
      : Boolean; overload;
    function ApplyUpdate(AQuery: TFDQuery; AStream: TStream; AProcName: String = ''): Boolean;
  end;

var
  ServerContainer: TServerContainer;

implementation

{$R *.dfm}

uses _smDataProvider, JdcGlobal, MyGlobal, JdcGlobal.DSCommon;

procedure TServerContainer.dscDataProviderGetClass(DSServerClass: TDSServerClass;
  var PersistentClass: TPersistentClass);
begin
  PersistentClass := _smDataProvider.TsmDataProvider;
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
      TGlobal.Obj.ApplicationMessage(mtInfo, StrDefault(AProcName, 'ExecQuery'),
        'Query=%s,RowsAffected=%d,ExecTime=%s', [AQuery.Name, AQuery.RowsAffected,
        FormatDateTime('NN:SS.zzz', Now - ExecTime)]);
      result := AQuery.RowsAffected = AParams.Count;
    except
      on E: Exception do
        TGlobal.Obj.ApplicationMessage(mtError, AProcName, E.Message);
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
      AQuery.ParamByJSONObject(AParams);

      ExecTime := Now;
      AQuery.ExecSQL;
      TGlobal.Obj.ApplicationMessage(mtInfo, StrDefault(AProcName, 'ExecQuery'),
        'Query=%s,RowsAffected=%d,ExecTime=%s', [AQuery.Name, AQuery.RowsAffected,
        FormatDateTime('NN:SS.zzz', Now - ExecTime)]);
      result := true;
    except
      on E: Exception do
        TGlobal.Obj.ApplicationMessage(mtError, AProcName, E.Message);
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
      TGlobal.Obj.ApplicationMessage(mtInfo, AMemTab.Name + ' Record Count',
        AMemTab.RecordCount.ToString);
    finally
      Conn.Free;
    end;
  end;

begin
  // FDQueryToFDMemTab(qryLogger, mtLogger);
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
      TGlobal.Obj.ApplicationMessage(mtInfo, StrDefault(AProcName, 'ApplyUpdates'),
        'Query=%s,ChangeCount=%d,Errors=%d,ExecTime=%s', [AQuery.Name, AQuery.ChangeCount,
        Errors, FormatDateTime('NN:SS.zzz', Now - ExecTime)]);

      result := Errors = 0;
    except
      on E: Exception do
      begin
        TGlobal.Obj.ApplicationMessage(mtError, AQuery.Name, E.Message);
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

    if Assigned(AParams) then
      AQuery.ParamByJSONObject(AParams, TGlobal.Obj.ApplicationMessage);

    try
      ExecTime := Now;
      result := AQuery.ToStream;
      TGlobal.Obj.ApplicationMessage(mtDebug, StrDefault(AProcName, 'OpenQuery'),
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
    TGlobal.Obj.ApplicationMessage(mtDebug, 'CreateDBPool', TOption.Obj.DBInfo);
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

procedure TServerContainer.DSAuthenticationManagerUserAuthenticate(Sender: TObject;
  const Protocol, Context, User, Password: string; var valid: Boolean; UserRoles: TStrings);

const
  ID = 'palyIoT';
  PW = 'play345';
begin
  valid := (User = ID) and (Password = PW);
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
  TGlobal.Obj.ApplicationMessage(mtWarning, 'Installed', SERVICE_NAME);
end;

procedure TServerContainer.ServiceAfterUninstall(Sender: TService);
begin
  TGlobal.Obj.ApplicationMessage(mtWarning, 'Uninstalled', SERVICE_NAME);
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
  try
    DSServer.Start;
  except
    on E: Exception do
      _RaiseException(Format('TCP=%d,HTTP=%d,E=%s', [DSTCPServerTransport.Port,
        DSHTTPService.HttpPort, E.Message]), E);
  end;

  TGlobal.Obj.ApplicationMessage(mtInfo, 'DSServer', 'TCP=%d,HTTP=%d',
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
  TGlobal.Obj.ApplicationMessage(mtError, Msg, E.Message);
  raise Exception.Create(Msg + #13#10 + E.Message);
end;

end.
