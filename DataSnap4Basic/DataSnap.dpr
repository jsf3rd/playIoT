program DataSnap;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain} ,
  _smDataLoader
    in 'DataSnap\_smDataLoader.pas' {smDataLoader: TDSServerModule} ,
  _smDataProvider
    in 'DataSnap\_smDataProvider.pas' {smDataProvider: TDSServerModule} ,
  ServerContainerUnit
    in 'DataSnap\ServerContainerUnit.pas' {ServerContainer: TDataModule} ,
  Global in 'Global\Global.pas',
  Option in 'Global\Option.pas',
  _fmOption in '_fmOption.pas' {fmOption};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'DataSnap for Basic';
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.Run;

end.
