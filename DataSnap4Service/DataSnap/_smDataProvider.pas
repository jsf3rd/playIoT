unit _smDataProvider;

interface

uses System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, Datasnap.DSProviderDataModuleAdapter, JdcGlobal.DSCommon,
  _ServerContainer;

type
  TsmDataProvider = class(TDSServerModule)
    procedure DSServerModuleCreate(Sender: TObject);
  private
  public
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
  end;

implementation

{$R *.dfm}

uses System.StrUtils;

procedure TsmDataProvider.DSServerModuleCreate(Sender: TObject);
begin
  TDSCommon.InitDataType(Self, ServerContainer.GetIdleConnection);
end;

function TsmDataProvider.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TsmDataProvider.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.
