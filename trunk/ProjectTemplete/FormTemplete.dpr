program FormTemplete;

uses
  Vcl.Forms,
  // JclAppInst,  // 중복 실행 방지
  _fmMain in 'View\_fmMain.pas' {fmMain} ,
  Core in 'Core\Core.pas',
  MyCommon in 'Global\MyCommon.pas',
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas';

{$R *.res}

begin
  // 중복 실행을 막으려면 활성화 하시오.
  // JclAppInstances.CheckSingleInstance;

  Application.Initialize;
  Application.Title := APPLICATION_TITLE;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;

end.
