program FileMan;

uses
  SysUtils,
  Forms,
  Dialogs,
  Global in 'Globals\Global.pas',
  Option in 'Options\Option.pas',
  _fmMain in 'Views\_fmMain.pas' {fmMain},
  Core in 'Core\Core.pas',
  Common in 'Globals\Common.pas',
  FileClient in 'Core\FileClient\FileClient.pas';

{$R *.res}

begin
try
  TGlobal.Obj.Initialize;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.ShowMainForm := true;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;

except
  on E : Exception do MessageDlg(E.Message, mtError, [mbOk], 0);
end;

TGlobal.Obj.Finalize;
end.

