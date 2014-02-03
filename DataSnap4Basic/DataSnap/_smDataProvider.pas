unit _smDataProvider;

interface

uses
  System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth,
  Data.DBXCommon, Data.DB, Datasnap.DBClient, Data.DBXDBReaders,
  Datasnap.Provider, Data.DBXJSON, Data.FMTBcd, Data.SqlExpr, System.IOUtils,
  Data.DBXJSONCommon, System.DateUtils, Data.SqlTimSt;

type
  TsmDataProvider = class(TDSServerModule)
  private
  public
    function GetData(AValue: TJSONValue): TStream;
  public
  end;

implementation

{$R *.dfm}

uses ServerContainerUnit, Option, JdcGlobal, JdcView2, Global;

{ TsmDataProvider }

function TsmDataProvider.GetData(AValue: TJSONValue): TStream;
begin
  //
  result := nil;
end;

end.
