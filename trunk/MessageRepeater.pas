unit MessageRepeater;

interface

uses SysUtils, Classes, Generics.Defaults, Generics.Collections, Windows;

const
  DEFAULT_MESSAGE_COUNT = 99;

type
  TProc<T> = procedure(const AMessage: T) of object;
  TOnCompleteEvent = procedure(const ACount: Integer) of Object;

  TMessageRepeater<T> = class(TQueue<T>)
  strict private
    FOnBeforeExcute: TNotifyEvent;
    FOnExcute: TProc<T>;
    FOnCompleted: TOnCompleteEvent;
    FActive: Boolean;
    FExcutionCount: Integer;
    function GetEmpty: Boolean;
    function GetActive: Boolean;
    procedure ExcuteLoop;
  private
  protected

  public
    constructor Create;
    destructor Destroy; override;

    procedure Excute;

    property Empty: Boolean read GetEmpty;
    property Active: Boolean read GetActive;

    property OnExcute: TProc<T> read FOnExcute write FOnExcute;
    property OnCompleted: TOnCompleteEvent read FOnCompleted write FOnCompleted;
    property OnBeforeExcute: TNotifyEvent read FOnBeforeExcute
      write FOnBeforeExcute;

  end;

implementation

{ TMessageRepeater<T> }

constructor TMessageRepeater<T>.Create;
begin
  inherited;
  FActive := False;
  FExcutionCount := 0;
end;

destructor TMessageRepeater<T>.Destroy;
begin
  inherited;
end;

procedure TMessageRepeater<T>.Excute;
begin
  if not Assigned(FOnExcute) then
  begin
    raise Exception.Create('Not assigned OnExcute');
  end;

  if FActive then
    Exit;

  FActive := True;
  if Assigned(FOnBeforeExcute) then
  begin
    FOnBeforeExcute(Self);
  end;

  try
    ExcuteLoop;
  finally
    if Assigned(FOnCompleted) then
    begin
      FOnCompleted(FExcutionCount);
    end;
    FExcutionCount := 0;
    FActive := False;
  end;

end;

procedure TMessageRepeater<T>.ExcuteLoop;
begin
  if Self.Empty then
    Exit;

  FOnExcute(Dequeue);
  inc(FExcutionCount);
  ExcuteLoop;
end;

function TMessageRepeater<T>.GetEmpty: Boolean;
begin
  result := Self.Count = 0;
end;

function TMessageRepeater<T>.GetActive: Boolean;
begin
  result := FActive;
end;

end.
