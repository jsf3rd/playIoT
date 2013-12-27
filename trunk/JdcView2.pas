unit JdcView2;

interface

uses
  JdcView2.ObserverListEx, ValueList,
  Classes, SysUtils;

type
  TView = class(TObserverListEx)
  protected
    constructor Create(AOwner: TComponent); reintroduce;
  public
    class function Obj: TView;

    procedure sp_SyncPacket(APacket: String);
    procedure sp_ASyncPacket(APacket: String);

    procedure sp_SyncMessage(ACode: String; AMsg: String = '');
    procedure sp_AsyncMessage(ACode: String; AMsg: String = '');
    destructor Destroy; override;

    procedure sp_ErrorMessage(Msg: String);
    procedure sp_ShowMessage(Msg: String);
    procedure sp_Terminate(Msg: string);
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

procedure TView.sp_AsyncMessage(ACode, AMsg: String);
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

procedure TView.sp_ASyncPacket(APacket: String);
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

procedure TView.sp_ErrorMessage(Msg: String);
begin
  sp_AsyncMessage('ErrorMessage', Msg);
end;

procedure TView.sp_ShowMessage(Msg: String);
begin
  sp_AsyncMessage('ShowMessage', Msg);
end;

procedure TView.sp_SyncPacket(APacket: String);
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

procedure TView.sp_SyncMessage(ACode, AMsg: String);
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

procedure TView.sp_Terminate(Msg: string);
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
