program DataSnap;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  _smDataLoader in 'DataSnap\_smDataLoader.pas' {smDataLoader: TDSServerModule},
  _smDataProvider in 'DataSnap\_smDataProvider.pas' {smDataProvider: TDSServerModule},
  ServerContainerUnit in 'DataSnap\ServerContainerUnit.pas' {ServerContainer: TDataModule},
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas',
  _fmOption in '_fmOption.pas' {fmOption};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := APPLICATION_TITLE;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.Run;

end.
