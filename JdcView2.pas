unit JdcView2;

interface

uses
  JdcView2.ObserverListEx, JsonData, Classes, SysUtils;

type
  TView = class(TObserverListEx)
  protected
    constructor Create(AOwner: TComponent); reintroduce;
  public
    class function Obj: TView;

    procedure sp_SyncPacket(const APacket: String);
    procedure sp_ASyncPacket(const APacket: String);

    procedure sp_SyncMessage(const ACode: String; AMsg: String = '');
    procedure sp_AsyncMessage(const ACode: String; AMsg: String = '');
    destructor Destroy; override;

    procedure sp_ErrorMessage(const ErrorName: string;
      const ErrorMsg: String = '');
      deprecated 'Use TGlobal.Obj.ApplicationMessage';
    procedure sp_DebugMessage(const Msg: String); overload;
      deprecated 'Use TGlobal.Obj.ApplicationMessage';
    procedure sp_LogMessage(const LogName: String; const LogMsg: string = '');
      deprecated 'Use TGlobal.Obj.ApplicationMessage';

    procedure sp_ShowMessage(const Msg: String);
    procedure sp_Terminate(const Msg: string);
  end;

implementation

uses JdcGlobal;

var
  MyObj: TView = nil;

  { TView }

constructor TView.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TView.Destroy;
begin
  inherited;
end;

procedure TView.sp_AsyncMessage(const ACode: String; AMsg: String = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := ACode;
    JsonData.Values['Msg'] := AMsg;
    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_ASyncPacket(const APacket: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Text := APacket;
    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_DebugMessage(const Msg: String);
begin
  PrintDebug(Msg);
  sp_SyncMessage('DebugMessage', Msg);
end;

procedure TView.sp_ErrorMessage(const ErrorName: string;
  const ErrorMsg: String = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'ErrorMessage';
    JsonData.Values['Msg'] := ErrorName + ', ' + ErrorMsg; // old version.
    JsonData.Values['ErrorName'] := ErrorName;
    JsonData.Values['ErrorMsg'] := ErrorMsg;
    Broadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_LogMessage(const LogName: String; const LogMsg: string = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'LogMessage';
    JsonData.Values['Msg'] := LogName + ', ' + LogMsg; // old version.
    JsonData.Values['LogName'] := LogName;
    JsonData.Values['LogMsg'] := LogMsg;
    Broadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_ShowMessage(const Msg: String);
begin
  sp_SyncMessage('ShowMessage', Msg);
end;

procedure TView.sp_SyncPacket(const APacket: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Text := APacket;
    Broadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_SyncMessage(const ACode: String; AMsg: String = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := ACode;
    JsonData.Values['Msg'] := AMsg;

    Broadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_Terminate(const Msg: string);
begin
  sp_SyncMessage('Terminate', Msg);
end;

class function TView.Obj: TView;
begin
  if MyObj = nil then
    MyObj := TView.Create(nil);
  Result := MyObj;
end;

initialization

MyObj := TView.Create(nil);

end.
