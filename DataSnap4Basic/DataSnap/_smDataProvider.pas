unit _smDataProvider;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth,
  Data.DB, Datasnap.DBClient, Data.DBXDBReaders,
  Datasnap.Provider, Data.FMTBcd, Data.SqlExpr, System.IOUtils,
  Data.DBXJSONCommon, System.DateUtils, Data.SqlTimSt,
  Datasnap.DSProviderDataModuleAdapter, System.JSON;

type
  TsmDataProvider = class(TDSServerModule)
    procedure DSServerModuleCreate(Sender: TObject);
  private
  public
    function GetData(AValue: TJSONValue): TStream;
    function EchoString(AValue: string): string;
  public
  end;

implementation

{$R *.dfm}

uses ServerContainerUnit, MyOption, JdcGlobal, MyGlobal;

{ TsmDataProvider }

procedure TsmDataProvider.DSServerModuleCreate(Sender: TObject);
begin
  Randomize;
end;

function TsmDataProvider.EchoString(AValue: string): string;
begin
  Sleep(60000);
  result := AValue;
end;

function TsmDataProvider.GetData(AValue: TJSONValue): TStream;
begin
  //
  result := nil;
end;

end.
