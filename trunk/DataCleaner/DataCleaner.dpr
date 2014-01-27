program DataCleaner;

uses
  Vcl.Forms, Winapi.Windows,
  _fmMain in '_fmMain.pas' {fmMain} ,
  Global in 'Global.pas',
  Option in 'Option.pas';

{$R *.res}

var
  Mutex: THandle; // ���ؽ� �ڵ��� �ϳ� ��������.

begin
  // �ߺ� ���� üũ
  Mutex := CreateMutex(nil, True, 'DataCleaner');
  // noDuplicate ���� �޶�� �ٸ����α׷����� �ν��մϴ�. ^^
  if (Mutex = 0) or (GetLastError <> 0) then
  begin
    // MessageBox(0, '���α׷��� �̹� �������Դϴ�.', 'Ȯ��', mb_IconInformation);
    Exit;
  end;
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
  // ���α׷� ����� �ڵ� �ݱ�
  if (Mutex <> 0) then
    CloseHandle(Mutex);

end.
