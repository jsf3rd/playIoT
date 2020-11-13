program SharedMemTest;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {Form2},
  JdcSharedMem.Dictionary in '..\JdcSharedMem.Dictionary.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
