program FormTemplete;

uses
  Vcl.Forms,
  // Winapi.Windows, {MessageBox}
  // JclAppInst, {�ߺ� ���� ����}
  _fmMain in 'View\_fmMain.pas' {fmMain} ,
  Core in 'Core\Core.pas',
  MyCommon in 'Global\MyCommon.pas',
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas';

{$R *.res}

begin
  {
    // �ߺ� ������ �������� Ȱ��ȭ �Ͻÿ�.
    if not JclAppInstances.CheckInstance(1) then
    begin
    MessageBox(0, '���α׷��� �̹� �������Դϴ�.', 'Ȯ��', MB_ICONEXCLAMATION);
    JclAppInstances.SwitchTo(0);
    JclAppInstances.KillInstance;
    end;
  }

  Application.Initialize;
  Application.Title := APPLICATION_TITLE;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;

end.
