unit _smDataProvider;

interface

uses System.SysUtils, System.Classes, System.Json,
  Datasnap.DSServer, Datasnap.DSAuth, Datasnap.DSProviderDataModuleAdapter, JdcGlobal.DSCommon,
  _ServerContainer, FireDAC.Stan.Intf, FireDAC.Stan.Option, FireDAC.Stan.Param,
  FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.Async,
  FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client;

type
  TsmDataProvider = class(TDSServerModule)
    qryMember: TFDQuery;
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
