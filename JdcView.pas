unit JdcView;

interface

uses
  ObserverList, JsonData,
  Classes, SysUtils;

type
  TView = class(TObserverList)
  protected
    constructor Create(AOwner: TComponent); reintroduce;
  public
    class function Obj: TView;

    procedure sp_SyncPacket(const APacket: String);
    procedure sp_ASyncPacket(const APacket: String);

    procedure sp_ErrorMessage(const AName: String; AMsg: String = '');
    procedure sp_WarnMessage(const AName: String; AMsg: String = '');
    procedure sp_LogMessage(const AName: String; AMsg: String = '');
    procedure sp_InfoMessage(const AName: String; AMsg: String = '');
    procedure sp_DebugMessage(const AName: String; AMsg: String = '');

    procedure sp_SyncMessage(const ACode: String; AMsg: String = ''; ARemark: String = '');
    procedure sp_AsyncMessage(const ACode: String; AMsg: String = ''; ARemark: String = '');
    destructor Destroy; override;

    procedure sp_Terminate(const Msg: string = '');
  end;

implementation

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

procedure TView.sp_AsyncMessage(const ACode: String; AMsg: String = ''; ARemark: String = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := ACode;
    JsonData.Values['Msg'] := AMsg;
    JsonData.Values['Remark'] := ARemark;

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

procedure TView.sp_DebugMessage(const AName: String; AMsg: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'DebugMessage';
    JsonData.Values['Name'] := AName;
    JsonData.Values['Msg'] := AMsg;

    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_ErrorMessage(const AName: String; AMsg: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'ErrorMessage';
    JsonData.Values['Name'] := AName;
    JsonData.Values['Msg'] := AMsg;

    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_InfoMessage(const AName: String; AMsg: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'InfoMessage';
    JsonData.Values['Name'] := AName;
    JsonData.Values['Msg'] := AMsg;

    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_LogMessage(const AName: String; AMsg: String = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'LogMessage';
    JsonData.Values['Name'] := AName;
    JsonData.Values['Msg'] := AMsg;

    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_SyncPacket(const APacket: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Text := APacket;
    BroadCast(JsonData);
  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_SyncMessage(const ACode: String; AMsg: String = ''; ARemark: String = '');
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := ACode;
    JsonData.Values['Msg'] := AMsg;
    JsonData.Values['Remark'] := ARemark;

    BroadCast(JsonData);

  finally
    JsonData.Free;
  end;
end;

procedure TView.sp_Terminate(const Msg: string);
begin
  sp_SyncMessage('Terminate', Msg);
end;

procedure TView.sp_WarnMessage(const AName: String; AMsg: String);
var
  JsonData: TJsonData;
begin
  JsonData := TJsonData.Create;
  try
    JsonData.Values['Code'] := 'WarnMessage';
    JsonData.Values['Name'] := AName;
    JsonData.Values['Msg'] := AMsg;

    AsyncBroadcast(JsonData);
  finally
    JsonData.Free;
  end;
end;

class function TView.Obj: TView;
begin
  if MyObj = nil then
    MyObj := TView.Create(nil);
  Result := MyObj;
end;

initialization

if MyObj = nil then
  MyObj := TView.Create(nil);

finalization

if Assigned(MyObj) then
  FreeAndNil(MyObj);

end.
