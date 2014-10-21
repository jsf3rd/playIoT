program Templete;

uses
  Vcl.Forms,
  _fmMain in 'View\_fmMain.pas' {fmMain},
  Core in 'Core\Core.pas',
  Common in 'Global\Common.pas',
  Global in 'Global\Global.pas',
  Option in 'Global\Option.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
