unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Winapi.Shellapi,
  Vcl.Controls, Vcl.Forms, Vcl.ActnList, ValueList, Vcl.Dialogs, System.Actions,
  Vcl.Menus, Vcl.AppEvnts, Vcl.ExtCtrls, Vcl.StdCtrls, VclTee.TeeGDIPlus,
  IdBaseComponent, IdComponent, IdUDPBase, IdUDPServer, Vcl.ComCtrls,
  VclTee.TeEngine, VclTee.Series, VclTee.TeeProcs, VclTee.Chart, IdGlobal,
  IdSocketHandle, JdcQscd;

type
  TDataType = (dtTPGA, dtHPGA, dtWMMA, dtTMMA, dtMEC);

  TfmMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    ActionList: TActionList;
    actAbout: TAction;
    actClearLog: TAction;
    actExit: TAction;
    actShowIni: TAction;
    actShowLog: TAction;
    actTestMenu: TAction;
    MenuTest: TMenuItem;
    Exit1: TMenuItem;
    Panel1: TPanel;
    edtUDPPort: TLabeledEdit;
    Label2: TLabel;
    cmbData: TComboBox;
    Chart1: TChart;
    LineSeries2: TLineSeries;
    Chart2: TChart;
    LineSeries1: TLineSeries;
    Chart3: TChart;
    LineSeries3: TLineSeries;
    Panel2: TPanel;
    IdUDPServer: TIdUDPServer;
    btnExecute: TButton;
    actExecute: TAction;
    edtKeyCode: TLabeledEdit;
    procedure actExitExecute(Sender: TObject);
    procedure actTestMenuExecute(Sender: TObject);
    procedure actExecuteExecute(Sender: TObject);
    procedure IdUDPServerUDPRead(AThread: TIdUDPListenerThread;
      const AData: TIdBytes; ABinding: TIdSocketHandle);
    procedure FormResize(Sender: TObject);
    procedure About1Click(Sender: TObject);
  private
    FKeyCode: string;
    FDataType: TDataType;
    procedure OnQscd(AData: TQscdData);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses JdcGlobal, JdcView, System.UITypes;

procedure TfmMain.About1Click(Sender: TObject);
begin
  MessageDlg('Qsdc Test Server.' + #13#10 + 'jsf3rd@playiot.biz' + #13#10 +
    'www.playiot.biz (c)playIoT.', mtInformation, [mbOK], 0)
end;

procedure TfmMain.actExecuteExecute(Sender: TObject);
begin
  IdUDPServer.DefaultPort := StrToInt(edtUDPPort.Text);
  IdUDPServer.Active := not IdUDPServer.Active;

  if not IdUDPServer.Active then
  begin
    actExecute.Caption := '시작';
    Exit;
  end;

  FKeyCode := edtKeyCode.Text;
  FDataType := TDataType(cmbData.ItemIndex);

  Chart1.Series[0].Clear;
  Chart2.Series[0].Clear;
  Chart3.Series[0].Clear;

  actExecute.Caption := '중지';
  case FDataType of
    dtTPGA, dtHPGA:
      begin
        Height := 300;
        Chart2.Visible := false;
        Chart3.Visible := false;
        Chart1.Align := alClient;
      end;
    dtWMMA, dtTMMA, dtMEC:
      begin
        Height := 834;
        Chart1.Align := alTop;
        Chart2.Visible := true;
        Chart3.Visible := true;
        Chart1.Height := round(Panel2.Height / 3);
        Chart3.Height := round(Panel2.Height / 3);
      end;
  end;

  case FDataType of
    dtTPGA:
      begin
        Chart1.Title.Caption := 'TPAG';
      end;
    dtHPGA:
      begin
        Chart1.Title.Caption := 'HPGA';
      end;
    dtWMMA, dtTMMA, dtMEC:
      begin
        Chart1.Title.Caption := 'North-South';
        Chart2.Title.Caption := 'East-West';
        Chart3.Title.Caption := 'Up-Down';
      end;
  end;
end;

procedure TfmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.actTestMenuExecute(Sender: TObject);
begin
  MenuTest.Visible := not MenuTest.Visible;
end;

procedure TfmMain.FormResize(Sender: TObject);
begin
  if not Chart2.Visible then
    Exit;

  Chart1.Height := round(Panel2.Height / 3);
  Chart3.Height := round(Panel2.Height / 3);
end;

procedure TfmMain.IdUDPServerUDPRead(AThread: TIdUDPListenerThread;
  const AData: TIdBytes; ABinding: TIdSocketHandle);
var
  QscdHeader: TQscdHeader;
  QscdBody: TQscdBody;
  PgaHeader: TPGAHeader;
  QscdData: TQscdData;
begin
  CopyMemory(@QscdHeader, @AData[0], Sizeof(TQscdHeader));
  CopyMemory(@QscdBody, @AData[Sizeof(TQscdHeader)], Sizeof(TQscdBody));

  PgaHeader := TJdcQscd.GetHeaderInfo(QscdHeader);
  QscdData.KeyCode := PgaHeader.Station;
  QscdData.EventDate := PgaHeader.EventDate;
  QscdData.Body := QscdBody;

  OnQscd(QscdData);
end;

procedure TfmMain.OnQscd(AData: TQscdData);
begin
  if not AData.KeyCode.Equals(FKeyCode) then
    Exit;

  case FDataType of
    dtTPGA:
      Chart1.Series[0].AddXY(AData.EventDate, Rev4BytesF(AData.Body.TPGA));
    dtHPGA:
      Chart1.Series[0].AddXY(AData.EventDate, Rev4BytesF(AData.Body.HPGA));
    dtWMMA:
      begin
        Chart1.Series[0].AddXY(AData.EventDate,
          Rev4BytesF(AData.Body.WMMA.N.Maximum));
        Chart2.Series[0].AddXY(AData.EventDate,
          Rev4BytesF(AData.Body.WMMA.E.Maximum));
        Chart3.Series[0].AddXY(AData.EventDate,
          Rev4BytesF(AData.Body.WMMA.Z.Maximum));
      end;
    dtTMMA:
      begin
        Chart1.Series[0].AddXY(AData.EventDate,
          Rev4BytesF(AData.Body.TMM.N.Maximum));
        Chart2.Series[0].AddXY(AData.EventDate,
          Rev4BytesF(AData.Body.TMM.E.Maximum));
        Chart3.Series[0].AddXY(AData.EventDate,
          Rev4BytesF(AData.Body.TMM.Z.Maximum));
      end;
    dtMEC:
      begin
        Chart1.Series[0].AddXY(AData.EventDate, Rev4BytesF(AData.Body.MEC.N));
        Chart2.Series[0].AddXY(AData.EventDate, Rev4BytesF(AData.Body.MEC.E));
        Chart3.Series[0].AddXY(AData.EventDate, Rev4BytesF(AData.Body.MEC.Z));
      end;
  end;

  while Chart1.Series[0].Count > 60 * 10 do
    Chart1.Series[0].Delete(0);

  while Chart2.Series[0].Count > 60 * 10 do
    Chart1.Series[0].Delete(0);

  while Chart3.Series[0].Count > 60 * 10 do
    Chart1.Series[0].Delete(0);
end;

end.
