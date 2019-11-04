/// ObserverListEx unit
///
/// Remove Critical Section.
/// Add TThread.Synchronize, TThread.Queue.

unit JdcView2.ObserverListEx;

interface

uses
  JsonData, HandleComponent,
  Windows, Messages, Classes, SysUtils, SyncObjs, Types, JdcGlobal;

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
    FActive: boolean;
    procedure do_Notify(Observer: TObject; Packet: TJsonData);
    procedure do_RemoveItems;
  private
    FLockCount: integer;

    procedure _BroadCast(const AText: string);
    procedure _AsyncBroadCast(const AText: string);
    procedure SendMessage(PacketText: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;
    /// Unregister all observers
    procedure Add(Observer: TObject);
    /// Register an observer.
    procedure Remove(Observer: TObject);
    /// Unregister an observer.

    procedure BroadCast(APacket: TJsonData); overload;
    /// Send synchronous message.
    procedure BroadCast(AText: string); overload;
    /// Send synchronous message.

    procedure AsyncBroadcast(APacket: TJsonData); overload;
    /// Send asynchronous message.
    procedure AsyncBroadcast(AText: string); overload;
    /// Send asynchronous message.
  published
    property Active: boolean read FActive write FActive;
    /// Message won't be sent when Active is false.
  end;

implementation

const
  LOCK_COUNT_LIMIT = 100;
  ALARM_COUNT = 10;

  { TObserverListEx }

procedure TObserverListEx.Add(Observer: TObject);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FList.Add(Observer);
    end);
end;

procedure TObserverListEx.BroadCast(APacket: TJsonData);
begin
  BroadCast(APacket.Text);
end;

procedure TObserverListEx.AsyncBroadcast(APacket: TJsonData);
begin
  AsyncBroadcast(APacket.Text);
end;

procedure TObserverListEx.AsyncBroadcast(AText: string);
begin
  if not FActive then
  begin
    PrintDebug('ObserverList inactive. Message discarded : ' + AText);
    Exit;
  end;

  try
    _AsyncBroadCast(AText);
  except
    on E: Exception do
      PrintDebug('Error on _AsyncBroadCast, ' + E.Message);
  end;

end;

procedure TObserverListEx.BroadCast(AText: string);
begin
  if not FActive then
  begin
    PrintDebug('ObserverList inactive. Message discarded : ' + AText);
    Exit;
  end;

  try
    _BroadCast(AText);
  except
    on E: Exception do
      PrintDebug('Error on _BroadCast, ' + E.Message);
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

  FLockCount := 0;
  FActive := true;
  FList := TList.Create;
end;

destructor TObserverListEx.Destroy;
begin
  do_RemoveItems;

  FList.Free;

  inherited;
end;

procedure TObserverListEx.do_Notify(Observer: TObject; Packet: TJsonData);
var
  Proc: procedure(Packet: TJsonData) of object;
begin
  // Notify 도중에 다시 Notify가 중복되지 않도록 조심, 재귀호출
  // 해당 Observer가 이미 삭제되었는데도, Remove 되지 않는 경우 조심
  TMethod(Proc).Data := Observer;
  TMethod(Proc).Code := TObject(Observer)
    .MethodAddress('rp_' + Packet.Values['Code']);
  if Assigned(Proc) then
    Proc(Packet);
end;

procedure TObserverListEx.do_RemoveItems;
var
  i: integer;
begin
  for i := FList.Count - 1 downto 0 do
    TObject(FList.Items[i]).Free;
  FList.Clear;
end;

procedure TObserverListEx.Remove(Observer: TObject);
begin
  TThread.Synchronize(nil,
    procedure
    begin
      FList.Remove(Observer);
    end);
end;

procedure TObserverListEx.SendMessage(PacketText: string);
var
  Packet: TJsonData;
  Loop: integer;
begin
  Packet := TJsonData.Create;
  try
    Packet.Text := PacketText;
    for Loop := FList.Count - 1 downto 0 do
      do_Notify(FList[Loop], Packet);
  finally
    Packet.Free;
  end;
end;

procedure TObserverListEx._AsyncBroadCast(const AText: string);
begin
  if FLockCount > LOCK_COUNT_LIMIT then
  begin
    PrintDebug('Async lock count over, Message discarded : ' + AText);
    Active := False;
    Exit;
  end;

  InterlockedIncrement(FLockCount);
  TThread.Queue(TThread.CurrentThread,
    procedure
    begin
      SendMessage(AText);
      InterlockedDecrement(FLockCount);

      if FLockCount > ALARM_COUNT then
        PrintDebug('Async lock count alarm : ' + FLockCount.ToString +
          ', ' + AText);

      if (not Active) and (FLockCount = 0) then
        Active := true;
    end);

end;

procedure TObserverListEx._BroadCast(const AText: string);
begin
  if FLockCount > LOCK_COUNT_LIMIT then
  begin
    PrintDebug('Sync lock count over, Message discarded : ' + AText);
    Active := False;
    Exit;
  end;

  InterlockedIncrement(FLockCount);

  TThread.Synchronize(TThread.CurrentThread,
    procedure
    begin
      SendMessage(AText);
      InterlockedDecrement(FLockCount);

      if FLockCount > ALARM_COUNT then
        PrintDebug('Sync lock count alarm : ' + FLockCount.ToString +
          ', ' + AText);

      if (not Active) and (FLockCount = 0) then
        Active := true;
    end);
end;

end.
