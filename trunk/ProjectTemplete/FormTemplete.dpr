program FormTemplete;

uses
  Vcl.Forms,
  // JclAppInst,  // �ߺ� ���� ����
  _fmMain in 'View\_fmMain.pas' {fmMain} ,
  Core in 'Core\Core.pas',
  MyCommon in 'Global\MyCommon.pas',
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas';

{$R *.res}

begin
  // �ߺ� ������ �������� Ȱ��ȭ �Ͻÿ�.
  // JclAppInstances.CheckSingleInstance;

  Application.Initialize;
  Application.Title := APPLICATION_TITLE;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;

end.
