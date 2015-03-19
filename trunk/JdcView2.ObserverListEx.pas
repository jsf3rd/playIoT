/// ObserverList unit
unit JdcView2.ObserverListEx;

interface

uses
  DebugTools, ValueList, HandleComponent, SyncValues,
  Windows, Messages, Classes, SysUtils, Types, SyncObjs, System.Threading,
  Vcl.Forms;

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
    FCS: TCriticalSection;
    procedure do_Notify(Observer: TObject; Packet: TValueList);
    procedure do_WM_ASYNC_BROADCAST(var Msg: TMessage);
      message WM_ASYNC_BROADCAST;
    procedure do_RemoveItems;
    procedure _BroadCast(const PacketText: string);
  private
    FLastCommand: TSyncString;
    procedure set_LastCommand(const AValue: string);
  private
    FActive: boolean;
    FLockCount: integer;
    function GetLastCommand: string;
    procedure SendMessage(PackdetText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    /// Unregister all observers
    procedure Clear;

    /// Register an observer.
    procedure Add(Observer: TObject);

    /// Unregister an observer.
    procedure Remove(Observer: TObject);

    /// Send synchronous message.
    procedure BroadCast(APacket: TValueList); overload;

    /// Send synchronous message.
    procedure BroadCast(const AText: string); overload;

    /// Send synchronous message to other observers but except Sender.
    procedure BroadCastToOther(Sender: TObject; APacket: TValueList); overload;

    /// Send synchronous message to other observers but except Sender.
    procedure BroadCastToOther(Sender: TObject; AText: string); overload;

    /// Send asynchronous message.
    procedure AsyncBroadcast(APacket: TValueList); overload;

    /// Send asynchronous message.
    procedure AsyncBroadcast(const AText: string); overload;

    /// Send synchronous message to Observer.
    procedure Notify(Observer: TObject; APacket: TValueList); overload;

    /// Send synchronous message to Observer.
    procedure Notify(Observer: TObject; AText: string); overload;
  published
    /// Message won't be sent when Active is false.
    property Active: boolean read FActive write FActive;

    { *
      ���� �ֱٿ� ������ View ��ɾ �������� �˷��ش�.
      ����뿡 ���ȴ�.
    }
    property LastCommand: string read GetLastCommand;
  end;

implementation

{ TObserverList }

procedure TObserverListEx.Add(Observer: TObject);
begin
  FCS.Enter;
  try
    FList.Add(Observer);
  finally
    FCS.Leave;
  end;
end;

procedure TObserverListEx.SendMessage(PackdetText: string);
var
  Packet: TValueList;
  Loop: integer;
begin
  Packet := TValueList.Create;
  try
    Packet.Text := PackdetText;
    for Loop := FList.Count - 1 downto 0 do
      do_Notify(FList[Loop], Packet);
  finally
    Packet.Free;
  end;
end;

procedure TObserverListEx.BroadCast(const AText: string);
begin
  if not Active then
    Exit;

  TTask.Run(
    procedure
    begin
      _BroadCast(AText);
    end);
end;

procedure TObserverListEx.BroadCast(APacket: TValueList);
var
  PacketText: String;
begin
  PacketText := APacket.Text;

  TTask.Run(
    procedure
    begin
      _BroadCast(PacketText);
    end);
end;

procedure TObserverListEx.AsyncBroadcast(APacket: TValueList);
begin
  AsyncBroadcast(APacket.Text);
end;

procedure TObserverListEx.AsyncBroadcast(const AText: string);
var
  Packet: TValueList;
begin
  if not Active then
    Exit;

  Packet := TValueList.Create;
  Packet.Text := AText;

  PostMessage(Handle, WM_ASYNC_BROADCAST, integer(Packet), 0);
end;

procedure TObserverListEx.BroadCastToOther(Sender: TObject;
APacket: TValueList);
var
  Loop: integer;
begin
  if not Active then
    Exit;

  set_LastCommand(APacket.Values['Code']);

  FCS.Enter;
  try
    for Loop := FList.Count - 1 downto 0 do
      if Sender <> FList[Loop] then
        do_Notify(FList[Loop], APacket);
  finally
    FCS.Leave;
  end;
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
  FCS.Enter;
  try
    FList.Clear;
  finally
    FCS.Leave;
  end;
end;

constructor TObserverListEx.Create(AOwner: TComponent);
begin
  inherited;

  FActive := true;

  FLockCount := 0;

  FList := TList.Create;
  FCS := TCriticalSection.Create;
  FLastCommand := TSyncString.Create;
end;

destructor TObserverListEx.Destroy;
begin
  do_RemoveItems;

  FCS.Free;
  FList.Free;
  FreeAndNil(FLastCommand);

  inherited;
end;

procedure TObserverListEx.do_Notify(Observer: TObject; Packet: TValueList);
var
  Proc: procedure(Packet: TValueList) of object;
begin
  // Notify ���߿� �ٽ� Notify�� �ߺ����� �ʵ��� ����, ���ȣ��
  // �ش� Observer�� �̹� �����Ǿ��µ���, Remove ���� �ʴ� ��� ����
  try
    TMethod(Proc).Data := Observer;
    TMethod(Proc).Code := TObject(Observer)
      .MethodAddress('rp_' + Packet.Values['Code']);
    if Assigned(Proc) then
      Proc(Packet);
  except
    on E: Exception do
      Trace(Format('TObserverList.do_Notify - %s' + #13#10 + '    - %s, %s',
        [E.Message, Observer.ClassName, Packet.Text]));
  end;
end;

procedure TObserverListEx.do_RemoveItems;
var
  I: integer;
begin
  for I := FList.Count - 1 downto 0 do
    TObject(FList.Items[I]).Free;
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

function TObserverListEx.GetLastCommand: string;
begin

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

procedure TObserverListEx._BroadCast(const PacketText: string);
begin
  InterlockedIncrement(FLockCount);
  try
    Sleep(FLockCount * 10);

    if FLockCount > 1 then
      Trace('Sync Inc LockCount : ' + FLockCount.ToString + ', ' + PacketText);

    Trace('CurrentThread.ThreadID, ' + TThread.CurrentThread.ThreadID.ToString);
    Trace('MainThreadID, ' + MainThreadID.ToString);

    FCS.Acquire;
    try
      SendMessage(PacketText);
    finally
      FCS.Leave;
    end;

    Trace('>>>>>> CurrentThread.ThreadID, ' +
      TThread.CurrentThread.ThreadID.ToString);

  finally
    InterlockedDecrement(FLockCount);
    Trace('Sync Dec LockCount : ' + FLockCount.ToString);
  end;
end;

procedure TObserverListEx.Notify(Observer: TObject; APacket: TValueList);
begin
  set_LastCommand(APacket.Values['Code']);

  FCS.Enter;
  try
    do_Notify(Observer, APacket);
  finally
    FCS.Leave;
  end;
end;

procedure TObserverListEx.Remove(Observer: TObject);
begin
  FCS.Enter;
  try
    FList.Remove(Observer);
  finally
    FCS.Leave;
  end;
end;

procedure TObserverListEx.set_LastCommand(const AValue: string);
begin
  FLastCommand.Lock;
  try
    FLastCommand.Value := AValue;
  finally
    FLastCommand.Unlock;
  end;
end;

end.
