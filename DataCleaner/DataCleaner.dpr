program DataCleaner;

uses
  Vcl.Forms,
  Winapi.Windows,
  _fmMain in '_fmMain.pas' {fmMain} ,
  Global in 'Global.pas',
  Option in 'Option.pas';

{$R *.res}

var
  Mutex: THandle; // 뮤텍스 핸들을 하나 선언한후.

begin
  // 중복 실행 체크
  Mutex := CreateMutex(nil, True, 'DataCleaner');
  // noDuplicate 명이 달라야 다른프로그램으로 인식합니다. ^^
  if (Mutex = 0) or (GetLastError <> 0) then
  begin
    // MessageBox(0, '프로그램이 이미 실행중입니다.', '확인', mb_IconInformation);
    Exit;
  end;
  Application.Initialize;
  Application.MainFormOnTaskbar := False;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
  // 프로그램 종료시 핸들 닫기
  if (Mutex <> 0) then
    CloseHandle(Mutex);

end.
