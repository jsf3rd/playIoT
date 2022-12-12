unit JdcRoadMark;

interface

uses System.SysUtils, System.Classes, System.DateUtils, JdcGlobal.DSCommon,
  JdcGlobal.ClassHelper, JdcGlobal, JdcGPS;

type
  TAskBranchName = function(ACode: string): string of object;

  TRoadMarkJudge = class;

  TRoadMark = record
    dist: double;
    road_direction: string;
    road_code: string;
    road_name: string;
    mark_name: string;
    latitude: double;
    longitude: double;
    utm_x: double;
    utm_y: double;
    branch_code: string;
    lane_count: integer;
    pavement: integer;
    function ToString(AName: string): String;
    function mark_value: double;
    function GetRoadDirection: string;
    function GetMarkName: string;
  end;

  TState = class abstract
  private
    FRoadMarkJudge: TRoadMarkJudge;
  public
    constructor Create(Sender: TObject); virtual;

    procedure OnRoadMark(RoadMark: TRoadMark; ADirection: string); virtual;
    procedure NoRoadmark; virtual;
    procedure Undirection(RoadMark: TRoadMark); virtual;
    procedure Nearby(RoadMark: TRoadMark); virtual;
    function MarkCaption: string; virtual;
  end;

  TRoadMarkJudge = class
  strict private
    FState: TState;
    FStateNoRoadMark: TState;
    FStateUndirection: TState;
    FStateNearby: TState;
    FStateDrive: TState;
    FStateKeepDirection: TState;

    FDriveMark: TRoadMark;
    FAskBranchName: TAskBranchName;
  public
    procedure SetState(AState: TState);
    constructor Create;
    procedure OnRoadMarks(Marks: TArray<TRoadMark>);

    procedure SetNoRoadMark;
    procedure NoRoadmark;
    procedure UpdateRoadMark(RoadMark: TRoadMark);
    function CalcDirection(NewMark: TRoadMark): string;
    function DriveRoadDirection: string;
    function DriveRoadCode: string;
    function HasDirection: Boolean;

    function MarkCaption: String;
    function FileName(AIndex: integer; AGPS: TGPS): string;

    property State: TState read FState;
    property StateNoRoadMark: TState read FStateNoRoadMark;
    property StateUndirection: TState read FStateUndirection;
    property StateNearby: TState read FStateNearby;
    property StateDrive: TState read FStateDrive;
    property StateKeepDirection: TState read FStateKeepDirection;

    property DriveMark: TRoadMark read FDriveMark;
    property AskBranchName: TAskBranchName read FAskBranchName write FAskBranchName;
  end;

  TMarkParam = record
    utm_x: double;
    utm_y: double;
    road_direction: string;
    xmin: double;
    xmax: double;
    ymin: double;
    ymax: double;
  end;

  TStateNoRoadMark = class(TState)
  public
    function MarkCaption: string; override;
  end;

  TStateUndirection = class(TState)
  public
    procedure OnRoadMark(RoadMark: TRoadMark; ADirection: string); override;
  end;

  TStateNearby = class(TState)
  public
    function MarkCaption: string; override;
  end;

  TStateDrive = class(TState)
  public
    procedure OnRoadMark(RoadMark: TRoadMark; ADirection: string); override;
  end;

  TStateKeepDirection = class(TState)
  strict private
    FCount: integer;
  public
    procedure Init;
    procedure OnRoadMark(RoadMark: TRoadMark; ADirection: string); override;
  end;

const
  ROADMARK_DIST = 35;
  NO_DIRECTION = 'X'; // 방향 미정
  START_DRIECTION = 'S'; // 시점
  END_DRIECTION = 'E'; // 종점

  NEAR_DIRECTION = 'N'; // 근방

implementation

constructor TState.Create(Sender: TObject);
begin
  FRoadMarkJudge := Sender as TRoadMarkJudge;
end;

procedure TState.NoRoadmark;
begin
  FRoadMarkJudge.SetNoRoadMark;
end;

procedure TState.OnRoadMark(RoadMark: TRoadMark; ADirection: string);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateUndirection);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

procedure TState.Undirection(RoadMark: TRoadMark);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateUndirection);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

function TState.MarkCaption: string;
begin
  result := FRoadMarkJudge.DriveMark.ToString
    (FRoadMarkJudge.AskBranchName(FRoadMarkJudge.DriveMark.branch_code));
end;

procedure TState.Nearby(RoadMark: TRoadMark);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateNearby);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

procedure TStateDrive.OnRoadMark(RoadMark: TRoadMark; ADirection: string);
begin
  if ADirection = NO_DIRECTION then
  begin
    FRoadMarkJudge.SetState(FRoadMarkJudge.StateKeepDirection);
    TStateKeepDirection(FRoadMarkJudge.State).Init;
    RoadMark.road_direction := FRoadMarkJudge.DriveRoadDirection; // 주행방향 유지
  end;
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

procedure TStateKeepDirection.Init;
begin
  FCount := 0;
end;

procedure TStateKeepDirection.OnRoadMark(RoadMark: TRoadMark; ADirection: string);
const
  KEEP_LIMIT = 5;
begin
  if ADirection = NO_DIRECTION then
  begin
    Inc(FCount);
    if FCount > KEEP_LIMIT then
      FRoadMarkJudge.SetState(FRoadMarkJudge.StateDrive) // 주행방향 변경
    else
      RoadMark.road_direction := FRoadMarkJudge.DriveRoadDirection; // 주행방향 유지
  end
  else
    FRoadMarkJudge.SetState(FRoadMarkJudge.StateDrive);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

procedure TStateUndirection.OnRoadMark(RoadMark: TRoadMark; ADirection: string);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateDrive);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

function TRoadMarkJudge.CalcDirection(NewMark: TRoadMark): string;
begin
  if FDriveMark.road_direction = NO_DIRECTION then // 기존 주행 방향이 없는경우
    Exit(NewMark.road_direction)
  else if FDriveMark.dist = NewMark.dist then // 이정이 같은 경우 기존 방향 유지
    Exit(FDriveMark.road_direction);

  if FDriveMark.mark_value > NewMark.mark_value then
    result := START_DRIECTION
  else
    result := END_DRIECTION;

  if FDriveMark.road_direction = result then
    Exit;

  result := NO_DIRECTION;
end;

constructor TRoadMarkJudge.Create;
begin
  FStateNoRoadMark := TStateNoRoadMark.Create(Self);
  FStateUndirection := TStateUndirection.Create(Self);
  FStateNearby := TStateNearby.Create(Self);
  FStateDrive := TStateDrive.Create(Self);
  FStateKeepDirection := TStateKeepDirection.Create(Self);

  SetNoRoadMark;
end;

function TRoadMarkJudge.DriveRoadCode: string;
begin
  result := FDriveMark.road_code;
end;

function TRoadMarkJudge.DriveRoadDirection: string;
begin
  result := FDriveMark.road_direction;
end;

function TRoadMarkJudge.HasDirection: Boolean;
begin
  result := (FState = FStateDrive) or (FState = FStateKeepDirection);
end;

procedure TRoadMarkJudge.SetNoRoadMark;
begin
  SetState(FStateNoRoadMark);
  NoRoadmark;
end;

procedure TRoadMarkJudge.NoRoadmark;
begin
  FDriveMark.dist := 0;
  FDriveMark.road_direction := NO_DIRECTION;
  FDriveMark.road_code := '0000';
  FDriveMark.road_name := NO_DIRECTION;
  FDriveMark.mark_name := NO_DIRECTION;
  FDriveMark.latitude := 0;
  FDriveMark.longitude := 0;
end;

procedure TRoadMarkJudge.OnRoadMarks(Marks: TArray<TRoadMark>);

  function GetRoadMark: TRoadMark;
  var
    I: integer;
  begin
    for I := Low(Marks) to High(Marks) do
    begin
      // 기존 주행 노선을 우선으로 매칭
      // JC구간에서 노선이 중복되는 경우에 대한 예외처리
      if FDriveMark.road_code = Marks[I].road_code then
        Exit(Marks[I]);
    end;
    result := Marks[0];
  end;

var
  NewMark: TRoadMark;
begin
  if Length(Marks) = 0 then
    FState.NoRoadmark
  else if Marks[0].dist > ROADMARK_DIST then
    FState.Nearby(Marks[0])
  else
  begin
    NewMark := GetRoadMark;
    if NewMark.road_code <> FDriveMark.road_code then
      FState.Undirection(NewMark) // 노선 변경
    else
      FState.OnRoadMark(NewMark, CalcDirection(NewMark));
  end;
end;

procedure TRoadMarkJudge.SetState(AState: TState);
begin
  FState := AState;
end;

function TRoadMarkJudge.FileName(AIndex: integer; AGPS: TGPS): string;
begin
  result := FormatDateTime('YYYYMMDD_HHNNSS.ZZZ_', AGPS.DateTime) +
    Format('%0.6d_%0.7f_%0.7f_%d_%s_%s_%s_%0.3d.jpg', [AIndex, AGPS.latitude, AGPS.longitude,
    AGPS.quality, FDriveMark.road_code, FDriveMark.GetRoadDirection, FDriveMark.GetMarkName,
    Trunc(AGPS.Speed)]);
end;

function TRoadMarkJudge.MarkCaption: String;
begin
  result := FState.MarkCaption;
end;

procedure TRoadMarkJudge.UpdateRoadMark(RoadMark: TRoadMark);
begin
  FDriveMark := RoadMark;
end;

{ TStateNoRoadMark }

function TStateNoRoadMark.MarkCaption: string;
begin
  result := '알수없음';
end;

{ TStateNearby }

function TStateNearby.MarkCaption: string;
begin
  result := Format('%s %skm 근방', [FRoadMarkJudge.DriveMark.road_name,
    FRoadMarkJudge.DriveMark.mark_name])
end;

{ TRoadMark }

function TRoadMark.GetMarkName: string;
begin
  result := FormatFloat('000.00', StrToFloatDef(Self.mark_name, 0));
end;

function TRoadMark.GetRoadDirection: string;
begin
  if Self.dist > ROADMARK_DIST then
    result := NEAR_DIRECTION
  else
    result := Self.road_direction;
end;

function TRoadMark.mark_value: double;
begin
  result := StrToFloatDef(Self.mark_name, 0);
end;

function TRoadMark.ToString(AName: string): String;
begin
  result := Format('%s %s %skm (%s)', [Self.road_name, Self.GetRoadDirection,
    Self.mark_name, AName]);
end;

end.
