unit _fmMain;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvToolEdit, JvCombobox, JvExStdCtrls,
  JvDBCombobox, JdcView2, ValueList, Vcl.ActnList, Vcl.ComCtrls, Vcl.ExtCtrls,
  JvTextListBox, Vcl.Menus, DBXJSON, System.Actions, System.UITypes;

type
  TfmMain = class(TForm)
    mmLog: TMemo;
    ActionList: TActionList;
    actClearLog: TAction;
    StatusBar: TStatusBar;
    lbUserList: TListBox;
    Panel1: TPanel;
    GroupBox1: TGroupBox;
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
    procedure FormCreate(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
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
  MessageDlg(APPLICATION_TITLE + ' ' + APPLICATION_VERSION + ' ' +
    COPY_RIGHT_SIGN + #13#10#13#10 + HOME_PAGE_URL, mtInformation, [mbOK], 0);
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

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);
  TGlobal.Obj.ExeName := Application.ExeName;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  Caption := APPLICATION_TITLE;
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
