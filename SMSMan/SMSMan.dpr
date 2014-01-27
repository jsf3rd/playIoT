program SMSMan;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  Global in 'Global.pas',
  Option in 'Option.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
