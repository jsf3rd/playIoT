unit Global;

interface

uses
  Classes, SysUtils;

const
  COPY_RIGHT_FILEMAN = 'JudicoApp v1.0 - ¨Ï 2014 ENB GROUP';
  HOME_PAGE_URL = 'http://www.enbgourp.co.kr';

type
  TGlobal = class(TComponent)
  strict private
    FInitialized: boolean;
  private
    FExeName: String;
    FLogName: string;
    procedure SetExeName(const Value: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure Initialize;
    procedure Finalize;
  published
    property Initialized: boolean read FInitialized;
    property ExeName: String read FExeName write SetExeName;
    property LogName: string read FLogName;
  end;

implementation

uses Option;

var
  MyObj: TGlobal = nil;

  { TGlobal }

constructor TGlobal.Create(AOwner: TComponent);
begin
  inherited;

  FExeName := '';
  FInitialized := false;
end;

destructor TGlobal.Destroy;
begin
  Finalize;

  inherited;
end;

procedure TGlobal.Finalize;
begin
  if not FInitialized then
    Exit;
  FInitialized := false;

end;

procedure TGlobal.Initialize;
begin
  if FInitialized then
    Exit;

  // Todo :

  FInitialized := true;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then
    MyObj := TGlobal.Create(nil);
  result := MyObj;
end;

procedure TGlobal.SetExeName(const Value: String);
begin
  FExeName := Value;
  FLogName := ChangeFileExt(FExeName, '.log');
end;

end.
