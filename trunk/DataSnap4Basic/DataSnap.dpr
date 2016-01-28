program DataSnap;

uses
  Vcl.Forms,
  Winapi.Windows,
  JclAppInst, // �ߺ� ���� ����
  _fmMain in '_fmMain.pas' {fmMain} ,
  _smDataLoader
    in 'DataSnap\_smDataLoader.pas' {smDataLoader: TDSServerModule} ,
  _smDataProvider
    in 'DataSnap\_smDataProvider.pas' {smDataProvider: TDSServerModule} ,
  ServerContainerUnit
    in 'DataSnap\ServerContainerUnit.pas' {ServerContainer: TDataModule} ,
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas',
  _fmOption in '_fmOption.pas' {fmOption};

{$R *.res}

begin
  if not JclAppInstances.CheckInstance(1) then
  begin
    MessageBox(0, '���α׷��� �̹� �������Դϴ�.', 'Ȯ��', MB_ICONEXCLAMATION);
    JclAppInstances.SwitchTo(0);
    JclAppInstances.KillInstance;
  end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := APPLICATION_TITLE;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.Run;

end.
