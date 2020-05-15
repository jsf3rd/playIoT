unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, JdcLogging,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, CR1000;

type
  TfmMain = class(TForm)
    edtPort: TLabeledEdit;
    Panel1: TPanel;
    Memo1: TMemo;
    edtBaudRate: TLabeledEdit;
    btnOpen: TButton;
    btnGetData: TButton;
    btnClose: TButton;
    edtDataIndex: TLabeledEdit;
    btnCommaData: TButton;
    Button1: TButton;
    procedure btnOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnGetDataClick(Sender: TObject);
    procedure btnCommaDataClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FCR1K: TCR1000;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses JdcGlobal;

procedure TfmMain.btnCloseClick(Sender: TObject);
begin
  if Assigned(FCR1K) then
  begin
    FCR1K.ClosePort;
    FreeAndNil(FCR1K);
    PrintLog(Memo1, '연결 해제');
  end;
end;

procedure TfmMain.btnCommaDataClick(Sender: TObject);
var
  pData: PAnsiChar;
  result: Integer;
begin
  if not Assigned(FCR1K) then
    Exit;

  result := FCR1K.GetCommaData(2, string(edtDataIndex.Text).ToInteger, pData);

  if result = RESULT_CODE_OK then
    PrintLog(Memo1, 'Measure Data , ' + pData)
  else if result = RESULT_CODE_OK_EX then
  begin
    PrintLog(Memo1, 'Measure Data , ' + pData);
    PrintLog(Memo1, RESULT_MSG_OK_EX);
  end
  else if result = RESULT_CODE_TIME_OUT then
    PrintLog(Memo1, RESULT_MSG_TIME_OUT)
  else if result = RESULT_CODE_PORT_CLOSED then
    PrintLog(Memo1, RESULT_MSG_PORT_CLOSED);
end;

procedure TfmMain.btnGetDataClick(Sender: TObject);
var
  pData: PAnsiChar;
  result: Integer;
begin
  if not Assigned(FCR1K) then
    Exit;

  result := FCR1K.GetData(2, string(edtDataIndex.Text).ToInteger, pData);

  if result = RESULT_CODE_OK then
    PrintLog(Memo1, 'Measure Data , ' + pData)
  else if result = RESULT_CODE_OK_EX then
  begin
    PrintLog(Memo1, 'Measure Data , ' + pData);
    PrintLog(Memo1, RESULT_MSG_OK_EX);
  end
  else if result = RESULT_CODE_TIME_OUT then
    PrintLog(Memo1, RESULT_MSG_TIME_OUT)
  else if result = RESULT_CODE_PORT_CLOSED then
    PrintLog(Memo1, RESULT_MSG_PORT_CLOSED);

end;

procedure TfmMain.btnOpenClick(Sender: TObject);
var
  Opened: boolean;
begin
  FCR1K := TCR1000.Create;

  Opened := FCR1K.OpenPort(string(edtPort.Text).ToInteger, string(edtBaudRate.Text).ToInteger);

  if Opened then
  begin
    PrintLog(Memo1, '연결 성공');

    PrintLog(Memo1, FCR1K.GetStatus);

    // FCR1K.GetTableNames(pTable);
    // PrintLog(Memo1, pTable);
  end
  else
  begin
    PrintLog(Memo1, '연결 실패');

    FreeAndNil(FCR1K);
  end;

end;

procedure TfmMain.Button1Click(Sender: TObject);
var
 version: PAnsiChar;
begin
  caption := FCR1K.GetDllVersion(version);
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  btnCloseClick(nil);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FCR1K := nil;
end;

end.
