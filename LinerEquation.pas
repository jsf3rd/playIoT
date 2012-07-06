unit LinerEquation;

interface

uses System.SysUtils, System.Classes, Math, System.Types;

type

  TLinerEquation = class
  private
    FGradient: double; // 기울기
    FIntercept: double; // 절편
  public
    constructor Create(AGradient: double; APoint: TPointF);
    function CalcX(AY: double): TPointF;
    function CalcY(AX: double): TPointF;
    function CalcPoint(APoint: TPointF; ADistance: double): TPointF;
  end;

implementation

{ TLinerEquation }

function TLinerEquation.CalcPoint(APoint: TPointF; ADistance: double): TPointF;
var
  a, b, c: double;
  tmp : double;
begin
  a := Math.Power((1 + FGradient), 2);
  b := -2*(FGradient * FIntercept + FGradient * APoint.Y + APoint.X);
  c := Math.Power(APoint.X, 2) + Math.Power(APoint.Y, 2) -
    Math.Power(ADistance, 2) + Math.Power(FIntercept, 2) - 2 * APoint.Y *
    FIntercept;

  result.X := -b;
  tmp := Math.Power(b,2);
  tmp := tmp - 4*a*c;
  tmp := Sqrt(round(tmp));
  result.X := result.X + tmp;
  result.X := result.X / 2*a;
  result.Y := CalcY(result.X).Y;
end;

function TLinerEquation.CalcX(AY: double): TPointF;
begin
  result.Y := AY;
  result.X := (AY - FIntercept) / FGradient;
end;

function TLinerEquation.CalcY(AX: double): TPointF;
begin
  result.X := AX;
  result.Y := FGradient * AX + FIntercept;
end;

constructor TLinerEquation.Create(AGradient: double; APoint: TPointF);
begin
  FGradient := AGradient;
  FIntercept := APoint.Y - (FGradient * APoint.X);
end;

end.
