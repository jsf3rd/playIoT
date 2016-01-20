unit _smDataProvider;

interface

uses System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, Datasnap.DSProviderDataModuleAdapter;

type
  TsmDataProvider = class(TDSServerModule)
  private
  public
    function EchoString(Value: string): string;
    function ReverseString(Value: string): string;
  end;

implementation

{$R *.dfm}

uses System.StrUtils;

function TsmDataProvider.EchoString(Value: string): string;
begin
  Result := Value;
end;

function TsmDataProvider.ReverseString(Value: string): string;
begin
  Result := System.StrUtils.ReverseString(Value);
end;

end.
