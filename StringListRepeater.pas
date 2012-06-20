unit StringListRepeater;

interface

uses SysUtils, Classes;

const
  DEFAULT_MAX_MESSAGE = 10;

type
  TOnClearEvent = procedure(const ACount: Integer) of Object;
  TAfterExcuteEvent = procedure(Sender: TObject; AData: String) of Object;

  TStringListRepeater = class(TStringList)
  private
    FOnBeforeExcute: TNotifyEvent;
    FOnExcute: TGetStrProc;
    FOnClear: TOnClearEvent;
    FExcutionCount: Integer;
    FMaxMessage: Integer;
    function GetEmpty: boolean;
    function GetActive: boolean;
  protected

  public
    constructor Create(Max: Integer = DEFAULT_MAX_MESSAGE);
    destructor Destroy; override;

    procedure Excute;

    function Extract: String;
    property Empty: boolean read GetEmpty;
    property OnExcute: TGetStrProc read FOnExcute write FOnExcute;
    property OnClear: TOnClearEvent read FOnClear write FOnClear;
    property OnBeforeExcute: TNotifyEvent read FOnBeforeExcute
      write FOnBeforeExcute;

    property Active: boolean read GetActive;
  published

  end;

implementation

{ TStringQueue }

constructor TStringListRepeater.Create(Max: Integer);
begin
  FMaxMessage := Max;
  FExcutionCount := 0;
end;

destructor TStringListRepeater.Destroy;
begin

  inherited;
end;

procedure TStringListRepeater.Excute;

  procedure _Excute;
  begin
    if not Empty then
    begin
      FOnExcute(Extract);
      _Excute;
    end;
  end;

var
  tmp: Integer;
begin
  if not Assigned(FOnExcute) then
  begin
    raise Exception.Create('Not assigned OnExcute');
  end;

  // 기본적으로 FExcutionCount > 0 크면 Queue 가 실행 중임으로 exit 한다.
  // FExcutionCount 값이 FMaxMessage를 초과했다면 Queue가 실행 중 오류가 발생한 것으로 판단하고
  // 작업을 진행한다.
  if (FExcutionCount > 0) and (FExcutionCount < FMaxMessage) then
    exit;

  if Assigned(FOnBeforeExcute) then
  begin
    FOnBeforeExcute(Self);
  end;

  try
    _Excute;

  finally
    tmp := FExcutionCount;
    FExcutionCount := 0;

    if Assigned(FOnClear) then
    begin
      FOnClear(tmp);
    end;
  end;

end;

function TStringListRepeater.Extract: String;
begin
  inc(FExcutionCount);
  if Count = 0 then
  begin
    dec(FExcutionCount);
    result := '';
  end
  else
  begin
    result := Strings[0];
    Delete(0);
  end;
end;

function TStringListRepeater.GetActive: boolean;
begin
  result := FExcutionCount > 0;
end;

function TStringListRepeater.GetEmpty: boolean;
begin
  result := Count = 0;
end;

end.
