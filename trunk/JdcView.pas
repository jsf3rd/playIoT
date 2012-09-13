unit JdcView;

interface

uses
  ObserverList, ValueList,
  Classes, SysUtils, MessageRepeater;

const
  MAX_MESSAGE = 10;

type
  TView = class(TObserverList)
  protected
    FMessageQueue: TMessageRepeater<String>;
    constructor Create(AOwner: TComponent); reintroduce;
    procedure OnExcute(const AData: String);

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
  FMessageQueue := TMessageRepeater<String>.Create(MAX_MESSAGE);
  FMessageQueue.OnExcute := OnExcute;
end;

destructor TView.Destroy;
begin
  FMessageQueue.Free;
  inherited;
end;

procedure TView.OnExcute(const AData: String);
begin
  Packet.Text := AData;
  BroadCast;
end;

procedure TView.sp_AsyncMessage(ACode, AMsg: String);
begin
  Packet.Clear;
  Packet.Values['Code'] := ACode;
  Packet.Values['Msg'] := AMsg;
  AsyncBroadcast;
end;

procedure TView.sp_ASyncPacket(APacket: String);
begin
  Packet.Clear;
  Packet.Text := APacket;
  AsyncBroadcast;
end;

procedure TView.sp_ErrorMessage(Msg: String);
begin
  // sp_SyncMessage('ErrorMessage', Msg + ' : ' + ProcByLevel(1) + '(' +IntToStr(LineByLevel(1)) + ')');
  sp_SyncMessage('ErrorMessage', Msg);
end;

procedure TView.sp_ShowMessage(Msg: String);
begin
  sp_SyncMessage('ShowMessage', Msg);
end;

procedure TView.sp_SyncPacket(APacket: String);
var
  _packet: TValueList;
begin
  _packet := TValueList.Create;
  try
    _packet.Text := APacket;
    FMessageQueue.Enqueue(_packet.Text);
  finally
    _packet.Free;
  end;

  FMessageQueue.Excute;
end;

procedure TView.sp_SyncMessage(ACode, AMsg: String);
var
  _packet: TValueList;
begin
  _packet := TValueList.Create;
  try
    _packet.Values['Code'] := ACode;
    _packet.Values['Msg'] := AMsg;
    FMessageQueue.Enqueue(_packet.Text);
  finally
    _packet.Free;
  end;

  FMessageQueue.Excute;
end;

procedure TView.sp_Terminate(Msg: string);
begin
  Packet.Clear;
  Packet.Values['Code'] := 'Terminate';
  Packet.Values['Msg'] := Msg;
  BroadCast;
end;

{ TView }

class function TView.Obj: TView;
begin
  if MyObj = nil then
    MyObj := TView.Create(nil);
  Result := MyObj;
end;

end.
