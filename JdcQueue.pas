unit JdcQueue;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections

{$IFDEF MSWINDOWS}
    , Windows.Winapi
{$ENDIF}
    ;

type
  TCircularQueue<T: class> = class
  private
    FList: TList<T>;
    FPos: Integer;

{$IFDEF MSWINDOWS}
    CritSect: TRTLCriticalSection;
{$ENDIF}
  public
    procedure Enqueue(const AItem: T);
    function Dequeue: T;
    function Count: Integer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCircularQueue<T> }

function TCircularQueue<T>.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TCircularQueue<T>.Create;
begin
{$IFDEF MSWINDOWS}
  InitializeCriticalSection(CritSect);
{$ENDIF}
  FList := TList<T>.Create;
  FPos := 0;
end;

function TCircularQueue<T>.Dequeue: T;
begin
  if FList.Count = 0 then
    raise Exception.Create('TCircularQueue,NoItem');

{$IFDEF MSWINDOWS}
  EnterCriticalSection(CritSect);
{$ELIF}
  TMonitor.Enter(Self);
{$ENDIF}
  try
    Result := FList.Items[FPos];
    Inc(FPos);
    if FPos >= FList.Count then
      FPos := 0;
  finally
{$IFDEF MSWINDOWS}
    LeaveCriticalSection(CritSect);
{$ELIF}
    TMonitor.Exit(Self);
{$ENDIF}
  end;
end;

destructor TCircularQueue<T>.Destroy;
var
  I: Integer;
  Obj: TObject;
begin
  for I := 0 to FList.Count - 1 do
  begin
    Obj := FList.Items[I];
    FreeAndNil(Obj);
  end;

  FList.Free;
{$IFDEF MSWINDOWS}
  DeleteCriticalSection(CritSect);
{$ENDIF}
  inherited;
end;

procedure TCircularQueue<T>.Enqueue(const AItem: T);
begin
  FList.Add(AItem);
end;

end.
