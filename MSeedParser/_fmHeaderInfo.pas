unit _fmHeaderInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  JdcMSeed.Common;

type
  TfmHeaderInfo = class(TForm)
    edtStation: TLabeledEdit;
    edtChannel: TLabeledEdit;
    edtLocation: TLabeledEdit;
    edtNetwork: TLabeledEdit;
    edtSampleRate: TLabeledEdit;
    btnOK: TButton;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    { Public declarations }
  end;

var
  fmHeaderInfo: TfmHeaderInfo;

implementation

{$R *.dfm}

uses _fmMain;

procedure TfmHeaderInfo.btnOKClick(Sender: TObject);
var
  Station: string;
begin
  Station := Format('%-5s', [edtStation.Text]);
  fmMain.MSeedHeader := TMSeedHeader.Create(Station, edtLocation.Text,
    edtChannel.Text, edtNetwork.Text, StrToInt(edtSampleRate.Text));
  ModalResult := mrOk;
end;

procedure TfmHeaderInfo.FormCreate(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
