unit JdcQueue;

interface

uses System.SysUtils, System.Classes, System.Generics.Collections, Windows;

type
  TCircularQueue<T: class> = class
  private
    FList: TList<T>;
    FPos: Integer;

  public
    procedure Enqueue(AItem: T);
    function Dequeue: T;

    constructor Create;
    destructor Destroy; override;
  end;

var
  CritSect: TRTLCriticalSection;

implementation

{ TCircularQueue<T> }

constructor TCircularQueue<T>.Create;
begin
  FList := TList<T>.Create;
  FPos := 0;
end;

function TCircularQueue<T>.Dequeue: T;
begin
  EnterCriticalSection(CritSect);
  try
    result := FList.Items[FPos];
    Inc(FPos);
    if FPos >= FList.Count then
      FPos := 0;
  finally
    LeaveCriticalSection(CritSect);
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
  inherited;
end;

procedure TCircularQueue<T>.Enqueue(AItem: T);
begin
  FList.Add(AItem);
end;

initialization

InitializeCriticalSection(CritSect);

finalization

DeleteCriticalSection(CritSect);

end.
