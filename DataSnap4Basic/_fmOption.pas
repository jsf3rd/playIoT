unit _fmOption;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Global;

type
  TfmOption = class(TForm)
    btnConfirm: TButton;
    btnCancle: TButton;
    RzGroupBox1: TGroupBox;
    RzLabel1: TLabel;
    RzLabel2: TLabel;
    RzLabel3: TLabel;
    RzLabel4: TLabel;
    RzLabel8: TLabel;
    edtHost: TEdit;
    edtDataBase: TEdit;
    edtUser: TEdit;
    edtPass: TEdit;
    RzGroupBox3: TGroupBox;
    RzLabel6: TLabel;
    edtDBPort: TEdit;
    edtDSPort: TEdit;
    procedure btnConfirmClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnCancleClick(Sender: TObject);
  private
    FDBInfo: TConnectionParam;
  public
    { Public declarations }
  end;

var
  fmOption: TfmOption;

implementation

{$R *.dfm}

uses Option;

procedure TfmOption.btnCancleClick(Sender: TObject);
begin
  Close;
end;

procedure TfmOption.btnConfirmClick(Sender: TObject);
var
  params: TStringList;
begin
  params := TStringList.Create;
  params.Values[DB_HOST] := edtHost.Text;
  params.Values[DB_NAME] := edtDataBase.Text;
  params.Values[DB_USER_NAME] := edtUser.Text;
  params.Values[DB_PASSWORD] := edtPass.Text;
  params.Values[DB_PORT] := edtDBPort.Text;
  TOption.Obj.DBInfo := params.CommaText;
  params.Free;

  TOption.Obj.DSPort := StrToIntDef(edtDSPort.Text, 211);
  Close;
end;

procedure TfmOption.FormShow(Sender: TObject);
var
  params: TStringList;
begin
  params := TStringList.Create;
  params.CommaText := TOption.Obj.DBInfo;

  edtHost.Text := params.Values[DB_HOST];
  edtDataBase.Text := params.Values[DB_NAME];
  edtUser.Text := params.Values[DB_USER_NAME];
  edtPass.Text := params.Values[DB_PASSWORD];
  edtDBPort.Text := params.Values[DB_PORT];

  edtDSPort.Text := IntToStr(TOption.Obj.DSPort);

  edtHost.SetFocus;
end;

end.
