unit JdcDSRest;

interface

uses System.Classes, System.SysUtils, Datasnap.DSClientRest, JdcGlobal, Datasnap.DSProxyRest;

type
  TRestClientClass = class of TDSAdminRestClient;

  TJdcDSRest<T: class> = class
  private
    FConn: TDSRestConnection;
    FClient: TDSAdminRestClient;
    function GetClient: T;
  public
    constructor Create(AInfo: TConnInfo; AClass: TRestClientClass);
    destructor Destroy; override;

    property Client: T read GetClient;
  end;

implementation

{ TJdcDSRest<T> }

constructor TJdcDSRest<T>.Create(AInfo: TConnInfo; AClass: TRestClientClass);
const
  HTTP_CONNECT_TIMEOUT = 3000;
  HTTP_READ_TIMEOUT = 30000;
begin
  FConn := TDSRestConnection.Create(nil);
  FConn.PreserveSessionID := false;
  FConn.Host := AInfo.StringValue;
  FConn.Port := AInfo.IntegerValue;
  FConn.HTTP.ConnectTimeout := HTTP_CONNECT_TIMEOUT;
  FConn.HTTP.ReadTimeout := HTTP_READ_TIMEOUT;
  FClient := AClass.Create(FConn);
end;

destructor TJdcDSRest<T>.Destroy;
begin
  try
    FreeAndNilEx(FClient);
    FreeAndNilEx(FConn);
  except
    on E: Exception do
      raise Exception.Create('JdcDSRest,' + E.Message);
  end;

  inherited;
end;

function TJdcDSRest<T>.GetClient: T;
begin
  result := T(FClient);
end;

end.
