program FormTemplete;

uses
  Vcl.Forms,
  Winapi.Windows,
  _fmMain in 'View\_fmMain.pas' {fmMain} ,
  Core in 'Core\Core.pas',
  MyCommon in 'Global\MyCommon.pas',
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas';

{$R *.res}

var
  Mutex: THandle;

begin
  // 중복 실행 체크
  Mutex := CreateMutex(nil, True, APPLICATION_TITLE);
  // APPLICATION_TITLE 명이 달라야 다른프로그램으로 인식합니다. ^^
  if (Mutex = 0) or (GetLastError <> 0) then
  begin
    // MessageBox(0, '프로그램이 이미 실행중입니다.', '확인', mb_IconInformation);

    // Exit; // 프로그램 종료.
  end;

  Application.Initialize;
  Application.Title := APPLICATION_TITLE;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;

  // 프로그램 종료시 핸들 닫기
  if (Mutex <> 0) then
    CloseHandle(Mutex);

end.
