unit JdcView;

interface

uses
  ObserverList, ValueList,
  Classes, SysUtils;

type
  TView = class(TObserverList)
  protected
    constructor Create(AOwner: TComponent); reintroduce;
  public
    class function Obj: TView;

    procedure sp_SyncPacket(const APacket: String);
    procedure sp_ASyncPacket(const APacket: String);

    procedure sp_SyncMessage(const ACode: String; AMsg: String = '');
    procedure sp_AsyncMessage(const ACode: String; AMsg: String = '');
    destructor Destroy; override;

    procedure sp_ShowMessage(const Msg: String);
    procedure sp_Terminate(const Msg: string);
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

procedure TView.sp_AsyncMessage(const ACode: String; AMsg: String = '');
var
  ValueList: TValueList;
begin
  ValueList := TValueList.Create;
  try
    ValueList.Values['Code'] := ACode;
    ValueList.Values['Msg'] := AMsg;
    AsyncBroadcast(ValueList);
  finally
    ValueList.Free;
  end;
end;

procedure TView.sp_ASyncPacket(const APacket: String);
var
  ValueList: TValueList;
begin
  ValueList := TValueList.Create;
  try
    ValueList.Text := APacket;
    AsyncBroadcast(ValueList);
  finally
    ValueList.Free;
  end;
end;

procedure TView.sp_ShowMessage(const Msg: String);
begin
  sp_AsyncMessage('ShowMessage', Msg);
end;

procedure TView.sp_SyncPacket(const APacket: String);
var
  ValueList: TValueList;
begin
  ValueList := TValueList.Create;
  try
    ValueList.Text := APacket;
    BroadCast(ValueList);
  finally
    ValueList.Free;
  end;
end;

procedure TView.sp_SyncMessage(const ACode: String; AMsg: String = '');
var
  ValueList: TValueList;
begin
  ValueList := TValueList.Create;
  try
    ValueList.Values['Code'] := ACode;
    ValueList.Values['Msg'] := AMsg;

    BroadCast(ValueList);

  finally
    ValueList.Free;
  end;
end;

procedure TView.sp_Terminate(const Msg: string);
begin
  sp_SyncMessage('Terminate', Msg);
end;

{ TView }

class function TView.Obj: TView;
begin
  if MyObj = nil then
    MyObj := TView.Create(nil);
  Result := MyObj;
end;

end.
