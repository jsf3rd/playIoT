// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
program MSeedParser;

uses
  Vcl.Forms,
  _fmMain in '_fmMain.pas' {fmMain},
  _fmHeaderInfo in '_fmHeaderInfo.pas' {fmHeaderInfo};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmHeaderInfo, fmHeaderInfo);
  Application.Run;

end.
