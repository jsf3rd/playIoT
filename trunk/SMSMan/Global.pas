unit Global;

interface

uses
  Classes, SysUtils, JdcView2, Vcl.Imaging.jpeg, Data.DBXJSON, JdcOption,
  System.Types, ValueList;

const
  COPY_RIGHT_QUEENANT = 'JudicoApp v1.0 - ⓒ 2012 ENB GROUP';
  HOME_PAGE_URL = 'http://www.enbgourp.co.kr';

  NEXT_LINE = #13#10;

type
  TGlobal = class(TComponent)
  strict private
    FInitialized: Boolean;
  private
    FExeName: String;
    FDebugMessage: Boolean;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure Initialize;
    procedure Finalize;

  published
    property Initialized: Boolean read FInitialized;
    property ExeName: String read FExeName write FExeName;
    property DebugMessage: Boolean read FDebugMessage write FDebugMessage;

  end;

implementation

var
  MyObj: TGlobal = nil;

  { TGlobal }

constructor TGlobal.Create(AOwner: TComponent);
begin
  inherited;

  FExeName := '';
  FInitialized := false;
  FDebugMessage := false;
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
  Result := MyObj;
end;

end.
