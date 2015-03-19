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
    Observer 패턴을 응용하여 구현 된 클래스이다.
    메시지 내에 호출 될 메소드의 이름을 입력하도록 되어 있다.
    이를 통해 미리 알려지지 않은 메소드를 호출 할 수가 있다.
    메시지는 수신 받은 리스너(Observer)는
    자신에게 메시지 내에 있는 이름의 메소드가 있으면 실행하고 없으면 무시한다.
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
      가장 최근에 전송한 View 명령어가 무엇인지 알려준다.
      디버깅에 사용된다.
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
  // Notify 도중에 다시 Notify가 중복되지 않도록 조심, 재귀호출
  // 해당 Observer가 이미 삭제되었는데도, Remove 되지 않는 경우 조심
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
