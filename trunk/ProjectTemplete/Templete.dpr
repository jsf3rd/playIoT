program Templete;

uses
  Vcl.Forms, Winapi.Windows,
  _fmMain in 'View\_fmMain.pas' {fmMain} ,
  Core in 'Core\Core.pas',
  Common in 'Global\Common.pas',
  Global in 'Global\Global.pas',
  Option in 'Global\Option.pas';

{$R *.res}

var
  Mutex: THandle;

begin
  // �ߺ� ���� üũ
  Mutex := CreateMutex(nil, True, APPLICATION_TITLE);
  // APPLICATION_TITLE ���� �޶�� �ٸ����α׷����� �ν��մϴ�. ^^
  if (Mutex = 0) or (GetLastError <> 0) then
  begin
    // MessageBox(0, '���α׷��� �̹� �������Դϴ�.', 'Ȯ��', mb_IconInformation);

    // Exit; // ���α׷� ����.
  end;

  Application.Initialize;
  Application.Title := APPLICATION_TITLE;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;

  // ���α׷� ����� �ڵ� �ݱ�
  if (Mutex <> 0) then
    CloseHandle(Mutex);

end.
