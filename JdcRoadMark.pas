unit JdcRoadMark;

interface

uses System.SysUtils, System.Classes, System.DateUtils, JdcGlobal.DSCommon,
  JdcGlobal.ClassHelper, JdcGlobal, JdcGPS, JdcLogging;

type
  TRoadMarkJudge = class;

  TRoadLink = record
    a2_link_id: Integer;
    id: string;
    dist: double; // 차량과의 거리
    raodrank: Integer; // 1:고속국도, 2:일반국도
    roadtype: Integer; // 1:일반도로, 2:터널, 3:교량, 4:지하차도, 5:고가차도
    roadno: Integer; // 노선번호
    linktype: Integer; // 차로유형
    laneno: Integer; // 차로번호
    r_linkid: string; // 우측링크ID
    l_linkid: string; // 좌측링크ID
    fromnodeid: string; // 시점노드ID
    tonodeid: string; // 정점노드ID
    sectionid: string; // 영역ID
    length: double; // 길이
    function Init: TRoadLink;
    function GetLaneNo: Integer;
  end;

  TLaneInfo = record
    Lane: Integer; // 주행차로
    Total: Integer; // 전체차로
    function ToString: string;
  end;

  TRoadMark = record
    dist: double;
    road_direction: string;
    road_code: string;
    road_name: string;
    mark_name: string;
    latitude: double;
    longitude: double;
    branch_name: string;
    branch_code: string;
    lane_count: Integer;
    pavement: Integer;
    road_no: Integer;
    function ToString: String;
    function mark_value: double;
    function GetRoadDirection: string;
    function GetMarkName: string;
    function GetBranchCode: string;

    procedure NoRoadMark;
    function IsValid: Boolean;
  end;

  TState = class abstract
  private
    FRoadMarkJudge: TRoadMarkJudge;
  public
    constructor Create(Sender: TObject); virtual;

    procedure OnRoadMark(var RoadMark: TRoadMark; const ADirection: string); virtual;
    procedure NoRoadMark; virtual;
    procedure Undirection(const RoadMark: TRoadMark); virtual;
    procedure Nearby(const RoadMark: TRoadMark); virtual;
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

    FDriveMark: TRoadMark; // 1st
    FAltMark: TRoadMark; // 2nd

    FMarkTick: UInt64;

    FLastDebugMsg: string;
    function GetRoadMark(const Marks: TArray<TRoadMark>): TRoadMark;
  public
    procedure SetState(AState: TState);
    constructor Create;
    destructor Destroy; override;

    procedure OnRoadMarks(const Marks: TArray<TRoadMark>);

    procedure SetNoRoadMark;
    procedure UpdateRoadMark(const RoadMark: TRoadMark);
    function CalcDirection(const NewMark: TRoadMark): string;
    function DriveRoadDirection: string;
    function DriveRoadNo: Integer;
    function HasDirection: Boolean;

    function MarkCaption: String;
    property State: TState read FState;
    property StateNoRoadMark: TState read FStateNoRoadMark;
    property StateUndirection: TState read FStateUndirection;
    property StateNearby: TState read FStateNearby;
    property StateDrive: TState read FStateDrive;
    property StateKeepDirection: TState read FStateKeepDirection;

    property DriveMark: TRoadMark read FDriveMark;
    property AltMArk: TRoadMark read FAltMark;

    property MarkTick: UInt64 read FMarkTick;
    property LastDebugMsg: string read FLastDebugMsg;
  end;

  TMarkParam = record
    utm_x: double;
    utm_y: double;
    road_direction: string;
  end;

  TStateNoRoadMark = class(TState)
  public
    function MarkCaption: string; override;
  end;

  TStateUndirection = class(TState)
  public
    procedure OnRoadMark(var RoadMark: TRoadMark; const ADirection: string); override;
  end;

  TStateNearby = class(TState)
  public
    function MarkCaption: string; override;
  end;

  TStateDrive = class(TState)
  public
    procedure OnRoadMark(var RoadMark: TRoadMark; const ADirection: string); override;
  end;

  TStateKeepDirection = class(TState)
  strict private
    FCount: Integer;
  public
    procedure Init;
    procedure OnRoadMark(var RoadMark: TRoadMark; const ADirection: string); override;
  end;

const
  ROADMARK_DIST = 35;
  NO_DIRECTION = 'X'; // 방향 미정
  START_DRIECTION = 'S'; // 시점
  END_DRIECTION = 'E'; // 종점

  NEAR_DIRECTION = 'N'; // 근방

implementation

uses Winapi.Windows;

constructor TState.Create(Sender: TObject);
begin
  FRoadMarkJudge := Sender as TRoadMarkJudge;
end;

procedure TState.NoRoadMark;
begin
  FRoadMarkJudge.SetNoRoadMark;
end;

procedure TState.OnRoadMark(var RoadMark: TRoadMark; const ADirection: string);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateUndirection);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

procedure TState.Undirection(const RoadMark: TRoadMark);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateUndirection);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

function TState.MarkCaption: string;
begin
  result := FRoadMarkJudge.DriveMark.ToString;
end;

procedure TState.Nearby(const RoadMark: TRoadMark);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateNearby);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

procedure TStateDrive.OnRoadMark(var RoadMark: TRoadMark; const ADirection: string);
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

procedure TStateKeepDirection.OnRoadMark(var RoadMark: TRoadMark; const ADirection: string);
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

procedure TStateUndirection.OnRoadMark(var RoadMark: TRoadMark; const ADirection: string);
begin
  FRoadMarkJudge.SetState(FRoadMarkJudge.StateDrive);
  FRoadMarkJudge.UpdateRoadMark(RoadMark);
end;

function TRoadMarkJudge.CalcDirection(const NewMark: TRoadMark): string;
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

  FMarkTick := GetTickCount64;
end;

destructor TRoadMarkJudge.Destroy;
begin
  FreeAndNilEx(FStateNoRoadMark);
  FreeAndNilEx(FStateUndirection);
  FreeAndNilEx(FStateNearby);
  FreeAndNilEx(FStateDrive);
  FreeAndNilEx(FStateKeepDirection);

  inherited;
end;

function TRoadMarkJudge.DriveRoadNo: Integer;
begin
  result := FDriveMark.road_no;
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
  FDriveMark.NoRoadMark;
  FAltMark.NoRoadMark;
end;

function TRoadMarkJudge.GetRoadMark(const Marks: TArray<TRoadMark>): TRoadMark;
var
  I: Integer;
begin
  FAltMark := Marks[0];
  for I := Low(Marks) to High(Marks) do
  begin
    // 기존 주행 노선을 우선으로 매칭
    // JC구간에서 노선이 중복되는 경우에 대한 예외처리
    if FDriveMark.road_code = Marks[I].road_code then
      Exit(Marks[I]);
  end;
  if length(Marks) > 1 then
    FAltMark := Marks[1];
  result := Marks[0];
end;

procedure TRoadMarkJudge.OnRoadMarks(const Marks: TArray<TRoadMark>);
var
  NewMark: TRoadMark;
begin
  FLastDebugMsg := 'OnRoadMarksBegin';

  FMarkTick := GetTickCount64;
  FLastDebugMsg := 'GetTickCount64';

  if length(Marks) = 0 then
  begin
    FLastDebugMsg := 'NoRoadMark';
    FState.NoRoadMark;
  end
  else if Marks[0].dist > ROADMARK_DIST then
  begin
    FLastDebugMsg := 'Nearby';
    FState.Nearby(Marks[0]);
    if length(Marks) > 1 then
      FAltMark := Marks[1]
    else
      FAltMark := Marks[0];
  end
  else
  begin
    FLastDebugMsg := 'NewMark';
    NewMark := GetRoadMark(Marks);
    if NewMark.road_code <> FDriveMark.road_code then
      FState.Undirection(NewMark) // 노선 변경
    else
      FState.OnRoadMark(NewMark, CalcDirection(NewMark));
  end;

  FLastDebugMsg := 'OnRoadMarksEnd';
end;

procedure TRoadMarkJudge.SetState(AState: TState);
begin
  FState := AState;
end;

function TRoadMarkJudge.MarkCaption: String;
begin
  result := FState.MarkCaption;
end;

procedure TRoadMarkJudge.UpdateRoadMark(const RoadMark: TRoadMark);
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
  result := Format('%s %skm 근방(%s)', [FRoadMarkJudge.DriveMark.road_name, FRoadMarkJudge.DriveMark.mark_name,
    FRoadMarkJudge.DriveMark.branch_name])
end;

{ TRoadMark }

function TRoadMark.GetBranchCode: string;
begin
  if Self.branch_code.IsEmpty then
    result := 'N00000'
  else
    result := Self.branch_code
end;

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

function TRoadMark.IsValid: Boolean;
begin
  result := (Self.dist < ROADMARK_DIST) and ((Self.road_direction = START_DRIECTION) or
    (Self.road_direction = END_DRIECTION));
end;

function TRoadMark.mark_value: double;
begin
  result := StrToFloatDef(Self.mark_name, 0);
end;

procedure TRoadMark.NoRoadMark;
begin
  Self.dist := 0;
  Self.road_direction := NO_DIRECTION;
  Self.road_code := '0000';
  Self.road_name := NO_DIRECTION;
  Self.mark_name := NO_DIRECTION;
  Self.latitude := 0;
  Self.longitude := 0;
  Self.branch_code := '';
  Self.branch_name := '';
  Self.road_no := 0;
end;

function TRoadMark.ToString: String;
begin
  result := Format('%s %s %skm (%s)', [Self.road_name, Self.GetRoadDirection, Self.mark_name,
    Self.branch_name]);
end;

{ TLaneInfo }

function TLaneInfo.ToString: string;
begin
  result := Format('%d (%d)', [Self.Lane, Self.Total]);
end;

{ TRoadLink }

function TRoadLink.GetLaneNo: Integer;
begin
  // 좌회전 포켓링크는 91부터 시작
  if Self.laneno > 90 then
    result := Self.laneno - 90
  else
    result := Self.laneno;
end;

function TRoadLink.Init: TRoadLink;
begin
  Self.a2_link_id := 0;
  Self.id := '';
  Self.dist := 0;
  Self.raodrank := 0;
  Self.roadtype := 0;
  Self.roadno := 0;
  Self.linktype := 0;
  Self.laneno := 0;
  Self.r_linkid := '';
  Self.l_linkid := '';
  Self.fromnodeid := '';
  Self.tonodeid := '';
  Self.length := 0;
  result := Self;
end;

end.
