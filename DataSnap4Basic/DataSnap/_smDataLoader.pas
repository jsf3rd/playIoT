unit _smDataLoader;

interface

uses System.SysUtils, System.Classes, Datasnap.DSServer, Datasnap.DSAuth,
  Data.FMTBcd, Data.DB, Data.SqlExpr, System.MaskUtils,
  Datasnap.DBClient, Data.DBXDBReaders, ServerContainerUnit,
  Vcl.Imaging.jpeg, System.IOUtils, IdGlobal, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys,
  FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf, FireDAC.DApt,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  Datasnap.DSProviderDataModuleAdapter, System.JSON, FireDAC.VCLUI.Wait;

type
  TsmDataLoader = class(TDSServerModule)
    FDConnection: TFDConnection;
    FDQuery: TFDQuery;
    procedure DSServerModuleCreate(Sender: TObject);
    procedure DSServerModuleDestroy(Sender: TObject);
  private
  public
    procedure Upload(AData: TJSONValue);
  end;

implementation

{$R *.dfm}

uses System.StrUtils, _fmMain, MyGlobal, MyOption, JdcGlobal,
  DBXCommon;

procedure TsmDataLoader.DSServerModuleCreate(Sender: TObject);
begin
  FDConnection.Params.CommaText := TOption.Obj.DBInfo;

  try
    FDConnection.Connected := true;
  except
    on E: Exception do
      TGlobal.Obj.ApplicationMessage(msError, 'Connect DB', E.Message);
  end;
end;

procedure TsmDataLoader.DSServerModuleDestroy(Sender: TObject);
begin
  FDConnection.Connected := False;
end;

procedure TsmDataLoader.Upload(AData: TJSONValue);
begin
  { TODO -oOwner -cGeneral : Upload Process }
end;

end.
