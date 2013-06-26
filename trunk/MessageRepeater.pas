unit MessageRepeater;

interface

uses SysUtils, Classes, Generics.Defaults, Generics.Collections;

const
  DEFAULT_MESSAGE_COUNT = 10;

type
  TProc<T> = procedure(const AMessage: T) of object;
  TOnCompleteEvent = procedure(const ACount: Integer) of Object;

  TMessageRepeater<T> = class(TQueue<T>)
  private
    FOnBeforeExcute: TNotifyEvent;
    FOnExcute: TProc<T>;
    FOnCompleted: TOnCompleteEvent;
    FExcutionCount: Integer;
    FNormalMessageCount: Integer;
    function GetEmpty: boolean;
    function GetActive: boolean;

    procedure _Excute;
  protected

  public
    constructor Create(ACount: Integer = DEFAULT_MESSAGE_COUNT);
    destructor Destroy; override;

    procedure Excute;

    property Empty: boolean read GetEmpty;
    property OnExcute: TProc<T> read FOnExcute write FOnExcute;
    property OnCompleted: TOnCompleteEvent read FOnCompleted write FOnCompleted;
    property OnBeforeExcute: TNotifyEvent read FOnBeforeExcute
      write FOnBeforeExcute;

    property Active: boolean read GetActive;
  end;

implementation

{ TMessageRepeater<T> }

constructor TMessageRepeater<T>.Create(ACount: Integer);
begin
  FNormalMessageCount := ACount;
  Capacity := ACount;
  FExcutionCount := 0;
end;

destructor TMessageRepeater<T>.Destroy;
begin

  inherited;
end;

procedure TMessageRepeater<T>._Excute;
begin
  if not Empty then
  begin
    FOnExcute(Dequeue);
    Sleep(1);
    _Excute;
  end;
end;

procedure TMessageRepeater<T>.Excute;
var
  tmp: Integer;
begin
  if not Assigned(FOnExcute) then
  begin
    raise Exception.Create('Not assigned OnExcute');
  end;

  // 기본적으로 FExcutionCount > 0 크면 Queue 가 실행 중임으로 exit 한다.
  // FExcutionCount 값이 FNormalMessageCount를 초과했다면 Queue가 실행 중 오류가 발생한 것으로 판단하고
  // 작업을 진행한다.
  if (Active) and (FExcutionCount < FNormalMessageCount) then
  begin
    inc(FExcutionCount);
    exit;
  end;

  if Assigned(FOnBeforeExcute) then
  begin
    FOnBeforeExcute(Self);
  end;

  try
    _Excute;
  finally
    tmp := FExcutionCount;
    FExcutionCount := 0;

    if Assigned(FOnCompleted) then
    begin
      FOnCompleted(tmp);
    end;
  end;
end;

function TMessageRepeater<T>.GetActive: boolean;
begin
  result := FExcutionCount > 0;
end;

function TMessageRepeater<T>.GetEmpty: boolean;
begin
  result := Count = 0;
end;

end.
