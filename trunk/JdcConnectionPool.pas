// *******************************************************
//
// Judico DataSnap Common
//
// Copyright(c) 2014 ENBGroup.
//
// jsf3rd@enbgroup.co.kr
//
// Update 2014. 12. 04
//
// *******************************************************

unit JdcConnectionPool;

interface

uses System.Classes, System.SysUtils, System.Generics.Collections,
  System.Generics.Defaults, System.SyncObjs, FireDAC.Comp.Client,
  FireDAC.Stan.Consts;

const
  DEFAULT_MAX_ITEMS = 50;
  DEFAULT_CLEANUP_TIMEOUT = 30000;
  DEFAULT_EXPIRE_TIMEOUT = 90000;

type
  TJdcConnectionPool = class
  private
    FDManager: TFDManager;
    FMaxItems: Integer;
    FDefName: string;
  public
    constructor Create(CommaText, DefName, DriverID: String;
      MaximumItems: Integer = DEFAULT_MAX_ITEMS;
      CleanupTimeout: Integer = DEFAULT_CLEANUP_TIMEOUT;
      ExpireTimeout: Integer = DEFAULT_EXPIRE_TIMEOUT);
    destructor Destroy; override;

    function GetIdleConnection: TFDConnection;
    function GetIdleConnectionCount: Integer;
    procedure ReleaseConnection(AConnection: TFDConnection);
  end;

implementation

uses JdcView2;

{ TJdcConnectionPool }

constructor TJdcConnectionPool.Create(CommaText, DefName, DriverID: String;
  MaximumItems, CleanupTimeout, ExpireTimeout: Integer);
var
  List: TStringList;
begin
  FDManager := TFDManager.Create(nil);
  FMaxItems := MaximumItems;
  FDefName := DefName;

  List := TStringList.Create;
  List.CommaText := CommaText;
  List.Values[S_FD_ConnParam_Common_Pooled] := 'True';
  List.Values[S_FD_ConnParam_Common_Pool_MaximumItems] := FMaxItems.ToString;
  List.Values[S_FD_ConnParam_Common_Pool_CleanupTimeout] :=
    CleanupTimeout.ToString;
  List.Values[S_FD_ConnParam_Common_Pool_ExpireTimeout] :=
    ExpireTimeout.ToString;

  FDManager.AddConnectionDef(FDefName, DriverID, List);
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

procedure TJdcConnectionPool.ReleaseConnection(AConnection: TFDConnection);
begin
  if Assigned(AConnection) then
    AConnection.Free;
end;

end.
