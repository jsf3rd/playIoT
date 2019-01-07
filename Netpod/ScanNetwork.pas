unit ScanNetwork;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Dialogs, ComCtrls, Gauges;

type
  TfrmScanNetwork = class(TForm)
    Button1: TButton;
    Gauge1: TGauge;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  end;

var
  frmScanNetwork: TfrmScanNetwork;

implementation

{$R *.DFM}

uses dllinc;

procedure TfrmScanNetwork.FormActivate(Sender: TObject);
var
  i: Integer;
begin
  NP_SetStatus(NP_SCANNET);

  i := 0;
  while NP_GetStatus(NP_ISINITSCAN) = 0 do
    begin
      Sleep(100);
      inc(i);
      Gauge1.Progress := i;
      Application.ProcessMessages;
    end;
  Gauge1.Progress := 100;
  PostMessage(Handle, WM_CLOSE, 0, 0)
end;

procedure TfrmScanNetwork.Button1Click(Sender: TObject);
begin
  Close;
end;

end.
