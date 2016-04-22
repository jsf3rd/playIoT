unit MyGlobal;

interface

uses
  Classes, SysUtils;

const
  APPLICATION_TITLE = 'palyIoT Form Application Templete';
  APPLICATION_VERSION = 'v1.0';
  COPY_RIGHT_SIGN = '¨Ï 2016 playIoT';
  HOME_PAGE_URL = 'http://www.playIoT.biz';

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

uses MyOption;

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
  // FLogName := GetEnvironmentVariable('LOCALAPPDATA') + '\palyIoT\' +  ExtractFileName(FLogName);
end;

end.
