// *******************************************************
//
// playIoT FireDac Connection Pool
//
// Copyright(c) 2016 playIoT.
//
// jsf3rd@playiot.biz
//
// Update 2016. 04. 22
//
// *******************************************************

unit JdcConnectionPool;

interface

uses System.Classes, System.SysUtils, System.Generics.Collections,
  System.Generics.Defaults, System.SyncObjs, FireDAC.Comp.Client,
  FireDAC.Stan.Consts, JdcLogging, JdcGlobal;

const
  DEFAULT_MAX_ITEMS = 50;
  DEFAULT_CLEANUP_TIMEOUT = 30000;
  DEFAULT_EXPIRE_TIMEOUT = 90000;
  DEFAULT_LOGIN_TIMEOUT = 5;

type
  TJdcConnectionPool = class
  private
    FDManager: TFDManager;
    FMaxItems: Integer;
    FDefName: string;
  public
    constructor Create(CommaText, DefName, DriverID: String; MaximumItems: Integer = DEFAULT_MAX_ITEMS;
      CleanupTimeout: Integer = DEFAULT_CLEANUP_TIMEOUT; ExpireTimeout: Integer = DEFAULT_EXPIRE_TIMEOUT;
      LoginTimeout: Integer = DEFAULT_LOGIN_TIMEOUT);
    destructor Destroy; override;

    function GetIdleConnection: TFDConnection;
    function GetIdleConnectionCount: Integer;
  end;

implementation

{ TJdcConnectionPool }

constructor TJdcConnectionPool.Create(CommaText, DefName, DriverID: String;
  MaximumItems, CleanupTimeout, ExpireTimeout: Integer; LoginTimeout: Integer);
{$IF CompilerVersion  = 26} // XE5
const
  S_FD_ConnParam_Common_Pool_CleanupTimeout = 'POOL_CleanupTimeout';
  S_FD_ConnParam_Common_Pool_ExpireTimeout = 'POOL_ExpireTimeout';
  S_FD_ConnParam_Common_Pool_MaximumItems = 'POOL_MaximumItems';
{$ENDIF}
var
  List: TStringList;
begin
  FDManager := TFDManager.Create(nil);
  FMaxItems := MaximumItems;
  FDefName := DefName;

  List := TStringList.Create;
  try
    List.CommaText := CommaText;
    List.Values[S_FD_ConnParam_Common_Pooled] := 'True';
    List.Values[S_FD_ConnParam_Common_Pool_MaximumItems] := FMaxItems.ToString;
    List.Values[S_FD_ConnParam_Common_Pool_CleanupTimeout] := CleanupTimeout.ToString;
    List.Values[S_FD_ConnParam_Common_Pool_ExpireTimeout] := ExpireTimeout.ToString;
    List.Values[S_FD_ConnParam_Common_LoginTimeout] := LoginTimeout.ToString;

    FDManager.AddConnectionDef(FDefName, DriverID, List);
  finally
    List.Free;
  end;
  FDManager.Open;
end;

destructor TJdcConnectionPool.Destroy;
begin
  FDManager.Free;
end;

function TJdcConnectionPool.GetIdleConnection: TFDConnection;
begin
  result := TFDConnection.Create(nil);
  result.ConnectionDefName := FDefName;
  result.Connected := true;
end;

function TJdcConnectionPool.GetIdleConnectionCount: Integer;
begin
  result := FMaxItems - FDManager.ConnectionCount;
end;

end.
