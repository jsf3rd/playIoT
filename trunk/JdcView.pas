unit JdcView;

interface

uses
  ObserverList, ValueList,
  Classes, SysUtils, StringListRepeater;

const
  MAX_MESSAGE = 10;

type
  TViewAbstract = class(TObserverList)
  protected
    FMessageQueue: TStringListRepeater;
    constructor Create(AOwner: TComponent); reintroduce;
    procedure OnExcute(const AData: String);

  public
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

{ TViewAbstract }

constructor TViewAbstract.Create(AOwner: TComponent);
begin
  inherited;
  FMessageQueue := TStringListRepeater.Create(MAX_MESSAGE);
  FMessageQueue.OnExcute := OnExcute;

end;

destructor TViewAbstract.Destroy;
begin
  FMessageQueue.Free;
  inherited;
end;

procedure TViewAbstract.OnExcute(const AData: String);
begin
  Packet.Text := AData;
  BroadCast;
end;

procedure TViewAbstract.sp_AsyncMessage(ACode, AMsg: String);
begin
  Packet.Clear;
  Packet.Values['Code'] := ACode;
  Packet.Values['Msg'] := AMsg;
  AsyncBroadcast;
end;

procedure TViewAbstract.sp_ASyncPacket(APacket: String);
begin
  Packet.Clear;
  Packet.Text := APacket;
  AsyncBroadcast;
end;

procedure TViewAbstract.sp_ErrorMessage(Msg: String);
begin
  // sp_SyncMessage('ErrorMessage', Msg + ' : ' + ProcByLevel(1) + '(' +IntToStr(LineByLevel(1)) + ')');
  sp_SyncMessage('ErrorMessage', Msg);
end;

procedure TViewAbstract.sp_ShowMessage(Msg: String);
begin
  sp_SyncMessage('ShowMessage', Msg);
end;

procedure TViewAbstract.sp_SyncPacket(APacket: String);
var
  _packet: TValueList;
begin
  _packet := TValueList.Create;
  try
    _packet.Text := APacket;
    FMessageQueue.Add(_packet.Text);
  finally
    _packet.Free;
  end;

  FMessageQueue.Excute;
end;

procedure TViewAbstract.sp_SyncMessage(ACode, AMsg: String);
var
  _packet: TValueList;
begin
  _packet := TValueList.Create;
  try
    _packet.Values['Code'] := ACode;
    _packet.Values['Msg'] := AMsg;
    FMessageQueue.Add(_packet.Text);
  finally
    _packet.Free;
  end;

  FMessageQueue.Excute;
end;

procedure TViewAbstract.sp_Terminate(Msg: string);
begin
  Packet.Clear;
  Packet.Values['Code'] := 'Terminate';
  Packet.Values['Msg'] := Msg;
  BroadCast;
end;

end.
