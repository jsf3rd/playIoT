/// ObserverListEx unit
///
/// Remove Critical Section.
/// Add TThread.Synchronize, TThread.Queue.

unit JdcView2.ObserverListEx;

interface

uses
  ValueList, HandleComponent,
  Windows, Messages, Classes, SysUtils, SyncObjs, Types;

const
  WM_ASYNC_BROADCAST = WM_USER + 1;

type
  { *
    Observer ������ �����Ͽ� ���� �� Ŭ�����̴�.
    �޽��� ���� ȣ�� �� �޼ҵ��� �̸��� �Է��ϵ��� �Ǿ� �ִ�.
    �̸� ���� �̸� �˷����� ���� �޼ҵ带 ȣ�� �� ���� �ִ�.
    �޽����� ���� ���� ������(Observer)��
    �ڽſ��� �޽��� ���� �ִ� �̸��� �޼ҵ尡 ������ �����ϰ� ������ �����Ѵ�.
    - Reference: http://ryulib.tistory.com/85, http://ryulib.tistory.com/245
  }
  TObserverListEx = class(THandleComponent)
  private
    FList: TList;
    FActive: boolean;
    procedure do_Notify(Observer: TObject; Packet: TValueList);
    procedure do_WM_ASYNC_BROADCAST(var Msg: TMessage);
      message WM_ASYNC_BROADCAST;
    procedure do_RemoveItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    /// Unregister all observers
    procedure Add(Observer: TObject);
    /// Register an observer.
    procedure Remove(Observer: TObject);
    /// Unregister an observer.

    procedure BroadCast(APacket: TValueList); overload;
    /// Send synchronous message.
    procedure BroadCast(AText: string); overload;
    /// Send synchronous message.

    procedure BroadCastToOther(Sender: TObject; APacket: TValueList); overload;
    /// Send synchronous message to other observers but except Sender.
    procedure BroadCastToOther(Sender: TObject; AText: string); overload;
    /// Send synchronous message to other observers but except Sender.

    procedure AsyncBroadcast(APacket: TValueList); overload;
    /// Send asynchronous message.
    procedure AsyncBroadcast(AText: string); overload;
    /// Send asynchronous message.

    procedure Notify(Observer: TObject; APacket: TValueList); overload;
    /// Send synchronous message to Observer.
    procedure Notify(Observer: TObject; AText: string); overload;
    /// Send synchronous message to Observer.
  published
    property Active: boolean read FActive write FActive;
    /// Message won't be sent when Active is false.
  end;

implementation

{ TObserverListEx }

procedure TObserverListEx.Add(Observer: TObject);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FList.Add(Observer);
    end);
end;

procedure TObserverListEx.BroadCast(APacket: TValueList);
begin
  if not Active then
    Exit;

  TThread.Synchronize(nil,
    procedure
    var
      Loop: Integer;
      Packet: TValueList;
    begin
      Packet := TValueList.Create;
      try
        Packet.Text := APacket.Text;
        for Loop := FList.Count - 1 downto 0 do
          do_Notify(FList[Loop], Packet);
      finally
        Packet.Free;
      end;
    end);
end;

procedure TObserverListEx.AsyncBroadcast(APacket: TValueList);
var
  PacketText: string;
begin
  if not Active then
    Exit;

  PacketText := APacket.Text;

  TThread.Queue(nil,
    procedure
    var
      Loop: Integer;
      Packet: TValueList;
    begin
      Packet := TValueList.Create;
      try
        Packet.Text := PacketText;
        for Loop := FList.Count - 1 downto 0 do
          do_Notify(FList[Loop], Packet);
      finally
        Packet.Free;
      end;
    end);

end;

procedure TObserverListEx.AsyncBroadcast(AText: string);
var
  Packet: TValueList;
begin
  if not Active then
    Exit;

  Packet := TValueList.Create;
  try
    Packet.Text := AText;
    AsyncBroadcast(Packet);
  finally
    Packet.Free;
  end;

end;

procedure TObserverListEx.BroadCast(AText: string);
var
  Packet: TValueList;
begin
  if not Active then
    Exit;

  Packet := TValueList.Create;
  try
    Packet.Text := AText;
    BroadCast(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TObserverListEx.BroadCastToOther(Sender: TObject;
APacket: TValueList);
begin
  if not Active then
    Exit;

  TThread.Synchronize(nil,
    procedure
    var
      Loop: Integer;
    begin
      for Loop := FList.Count - 1 downto 0 do
        if Sender <> FList[Loop] then
          do_Notify(FList[Loop], APacket);
    end);
end;

procedure TObserverListEx.BroadCastToOther(Sender: TObject; AText: string);
var
  Packet: TValueList;
begin
  if not Active then
    Exit;

  Packet := TValueList.Create;
  try
    Packet.Text := AText;
    BroadCastToOther(Sender, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TObserverListEx.Clear;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FList.Clear;
    end);
end;

constructor TObserverListEx.Create(AOwner: TComponent);
begin
  inherited;

  FActive := true;
  FList := TList.Create;
end;

destructor TObserverListEx.Destroy;
begin
  do_RemoveItems;

  FList.Free;

  inherited;
end;

procedure TObserverListEx.do_Notify(Observer: TObject; Packet: TValueList);
var
  Proc: procedure(Packet: TValueList) of object;
begin
  // Notify ���߿� �ٽ� Notify�� �ߺ����� �ʵ��� ����, ���ȣ��
  // �ش� Observer�� �̹� �����Ǿ��µ���, Remove ���� �ʴ� ��� ����
  TMethod(Proc).Data := Observer;
  TMethod(Proc).Code := TObject(Observer)
    .MethodAddress('rp_' + Packet.Values['Code']);
  if Assigned(Proc) then
    Proc(Packet);
end;

procedure TObserverListEx.do_RemoveItems;
var
  i: Integer;
begin
  for i := FList.Count - 1 downto 0 do
    TObject(FList.Items[i]).Free;
  FList.Clear;
end;

procedure TObserverListEx.do_WM_ASYNC_BROADCAST(var Msg: TMessage);
var
  Packet: TValueList;
begin
  Packet := Pointer(Msg.WParam);
  try
    BroadCast(Packet);
  finally
    Packet.Free;
  end;
end;

procedure TObserverListEx.Notify(Observer: TObject; AText: string);
var
  Packet: TValueList;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := AText;
    Notify(Observer, Packet);
  finally
    Packet.Free;
  end;
end;

procedure TObserverListEx.Notify(Observer: TObject; APacket: TValueList);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      do_Notify(Observer, APacket);
    end);
end;

procedure TObserverListEx.Remove(Observer: TObject);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FList.Remove(Observer);
    end);
end;

end.
