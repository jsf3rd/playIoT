program MSeedParser;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {Form1} ,
  JdcMSeed in '..\JdcMSeed.pas',
  JdcMSeed.Steim in '..\JdcMSeed.Steim.pas',
  JdcMSeed.Common in '..\JdcMSeed.Common.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
