unit ScanNetwork;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  StdCtrls, ExtCtrls, Forms, Dialogs, ComCtrls, Gauges, _Netpod;

type
  TfrmScanNetwork = class(TForm)
    Button1: TButton;
    Gauge1: TGauge;
    procedure FormActivate(Sender: TObject);
    procedure Button1Click(Sender: TObject);

  private
    FNetpod: TNetpod;
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  frmScanNetwork: TfrmScanNetwork;

implementation

{$R *.DFM}

uses dllinc;

constructor TfrmScanNetwork.Create(AOwner: TComponent);
begin
  inherited;

  FNetpod := TNetpod(AOwner);
end;

procedure TfrmScanNetwork.FormActivate(Sender: TObject);
var
  i: Integer;
begin
  FNetpod.SetStatus(NP_SCANNET);

  i := 0;
  while not FNetpod.Scanned do
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
