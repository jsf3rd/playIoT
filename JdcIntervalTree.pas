unit JdcIntervalTree;

interface

uses
  SysUtils, Math;

type
  TInterval = record
    StartValue, EndValue: Double;
    constructor Create(const A, B, Margin: Double);
  end;

  PIntervalNode = ^TIntervalNode;

  TIntervalNode = record
    Interval: TInterval;
    MaxEnd: Double;
    Left, Right: PIntervalNode;
  end;

  TIntervalTree = class
  private
    FRoot: PIntervalNode;
    function InsertNode(Root: PIntervalNode; const Interval: TInterval): PIntervalNode;
    function SearchNode(Root: PIntervalNode; const Point: Double): Boolean;
    procedure DestroyNode(Root: PIntervalNode);
    function MergeIntervals(Root: PIntervalNode; const Interval: TInterval): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Insert(const Interval: TInterval);
    function Search(const Point: Double): Boolean;
  end;

implementation

constructor TIntervalTree.Create;
begin
  FRoot := nil;
end;

destructor TIntervalTree.Destroy;
begin
  DestroyNode(FRoot);
  inherited Destroy;
end;

function TIntervalTree.InsertNode(Root: PIntervalNode; const Interval: TInterval): PIntervalNode;
begin
  if Root = nil then
  begin
    New(Result);
    Result^.Interval := Interval;
    Result^.MaxEnd := Interval.EndValue;
    Result^.Left := nil;
    Result^.Right := nil;
    Exit;
  end;

  if Interval.StartValue < Root^.Interval.StartValue then
    Root^.Left := InsertNode(Root^.Left, Interval)
  else
    Root^.Right := InsertNode(Root^.Right, Interval);

  if Root^.MaxEnd < Interval.EndValue then
    Root^.MaxEnd := Interval.EndValue;

  Result := Root;
end;

function TIntervalTree.MergeIntervals(Root: PIntervalNode; const Interval: TInterval): Boolean;
begin
  Result := False;
  if Root = nil then
    Exit;

  if (Root^.Interval.StartValue <= Interval.EndValue) and (Interval.StartValue <= Root^.Interval.EndValue)
  then
  begin
    Root^.Interval.StartValue := Min(Root^.Interval.StartValue, Interval.StartValue);
    Root^.Interval.EndValue := Max(Root^.Interval.EndValue, Interval.EndValue);
    Root^.MaxEnd := Root^.Interval.EndValue;
    Result := True;
  end
  else if Interval.StartValue < Root^.Interval.StartValue then
    Result := MergeIntervals(Root^.Left, Interval)
  else
    Result := MergeIntervals(Root^.Right, Interval);
end;

procedure TIntervalTree.Insert(const Interval: TInterval);
begin
  if not MergeIntervals(FRoot, Interval) then
    FRoot := InsertNode(FRoot, Interval);
end;

function TIntervalTree.SearchNode(Root: PIntervalNode; const Point: Double): Boolean;
begin
  if Root = nil then
    Exit(False);

  if (Root^.Interval.StartValue <= Point) and (Point <= Root^.Interval.EndValue) then
    Exit(True);

  if (Root^.Left <> nil) and (Root^.Left^.MaxEnd >= Point) then
    Exit(SearchNode(Root^.Left, Point));

  Result := SearchNode(Root^.Right, Point);
end;

function TIntervalTree.Search(const Point: Double): Boolean;
begin
  Result := SearchNode(FRoot, Point);
end;

procedure TIntervalTree.DestroyNode(Root: PIntervalNode);
begin
  if Root = nil then
    Exit;

  DestroyNode(Root^.Left);
  DestroyNode(Root^.Right);
  Dispose(Root);
end;

{ TInterval }

constructor TInterval.Create(const A, B, Margin: Double);
begin
  if A < B then
  begin
    Self.StartValue := A - Margin;
    Self.EndValue := B + Margin;
  end
  else
  begin
    Self.StartValue := B - Margin;
    Self.EndValue := A + Margin;
  end;
end;

end.
