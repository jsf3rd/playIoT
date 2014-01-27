unit _fmMain;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvToolEdit, JvCombobox, JvExStdCtrls,
  JvDBCombobox, JdcView, ValueList, Vcl.ActnList, Vcl.ComCtrls, Vcl.ExtCtrls,
  JvTextListBox, Vcl.Menus, DBXJSON, System.Actions;

type
  TfmMain = class(TForm)
    mmLog: TMemo;
    ActionList: TActionList;
    actClearLog: TAction;
    StatusBar: TStatusBar;
    lbUserList: TListBox;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    chkMeaDataLog: TCheckBox;
    chkEventLog: TCheckBox;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ools1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    Option1: TMenuItem;
    actAbout: TAction;
    actExit: TAction;
    actOptions: TAction;
    X1: TMenuItem;
    N1: TMenuItem;
    Exit1: TMenuItem;
    StaticText1: TStaticText;
    chkErrorLog: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure chkMeaDataLogClick(Sender: TObject);
    procedure chkEventLogClick(Sender: TObject);
    procedure chkErrorLogClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
  private
    procedure DeleteItem(AIP: string);
  public
  published
    procedure rp_ShowMessage(APacket: TValueList);
    procedure rp_ErrorMessage(APacket: TValueList);
    procedure rp_ChannelStateChanged(APacket: TValueList);

    procedure rp_MeasureData(APacket: TValueList);

    procedure rp_UserConnected(APacket: TValueList);
    procedure rp_UserDisconnected(APacket: TValueList);

    procedure rp_StartDSServer(APacket: TValueList);

  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses Global, JdcGlobal, _fmOption, Option, ServerContainerUnit;

procedure TfmMain.actAboutExecute(Sender: TObject);
begin
  ShowMessage(APPLICATION_TITLE + ' - ' + COPY_RIGHT + #13#10#13#10 +
    HOME_PAGE_URL);
end;

procedure TfmMain.actClearLogExecute(Sender: TObject);
begin
  mmLog.Clear;
end;

procedure TfmMain.actExitExecute(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TfmMain.actOptionsExecute(Sender: TObject);
var
  fmOptions: TfmOption;
begin
  fmOptions := TfmOption.Create(Self);
  fmOptions.ShowModal;
  fmOptions.Free;
end;

procedure TfmMain.chkErrorLogClick(Sender: TObject);
begin
  TOption.Obj.ErrorLog := chkErrorLog.Checked;
end;

procedure TfmMain.chkEventLogClick(Sender: TObject);
begin
  TOption.Obj.EventLog := chkEventLog.Checked;
end;

procedure TfmMain.chkMeaDataLogClick(Sender: TObject);
begin
  TOption.Obj.MeaDataLog := chkMeaDataLog.Checked;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);
  TGlobal.Obj.ExeName := Application.ExeName;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  chkMeaDataLog.Checked := TOption.Obj.MeaDataLog;
  chkEventLog.Checked := TOption.Obj.EventLog;
  chkErrorLog.Checked := TOption.Obj.ErrorLog;
end;

procedure TfmMain.rp_ChannelStateChanged(APacket: TValueList);
begin
  StatusBar.Panels[1].Text := 'Callback Clients : ' + APacket.Values['Msg'];
end;

procedure TfmMain.rp_ErrorMessage(APacket: TValueList);
begin
  PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'),
    'Error Log - ' + APacket.Values['Msg']);
  PrintLog(mmLog, 'Error Log - ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_MeasureData(APacket: TValueList);
begin
  PrintLog(mmLog, '계측 데이터 수신 - ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_ShowMessage(APacket: TValueList);
begin
  PrintLog(mmLog, APacket.Values['Msg']);
end;

procedure TfmMain.rp_StartDSServer(APacket: TValueList);
begin
  StatusBar.Panels[0].Text := 'Port : ' + APacket.Values['Msg'];
end;

procedure TfmMain.DeleteItem(AIP: string);
var
  Index: Integer;
begin
  Index := lbUserList.Items.IndexOf(AIP);
  if Index >= 0 then
    lbUserList.Items.Delete(Index);
end;

procedure TfmMain.rp_UserConnected(APacket: TValueList);
begin
  DeleteItem(APacket.Values['Msg']);
  lbUserList.Items.Insert(0, APacket.Values['Msg']);
end;

procedure TfmMain.rp_UserDisconnected(APacket: TValueList);
begin
  DeleteItem(APacket.Values['Msg']);
end;

end.
