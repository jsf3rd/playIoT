unit _fmMain;

interface

uses
  JsonData,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Vcl.ActnList, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Grids, Vcl.DBGrids,
  Vcl.ComCtrls, IniFiles, Data.DB, Datasnap.DBClient, DBXJSON, Vcl.Menus,
  Vcl.Imaging.jpeg, TickCounter, JdcGlobal, JvBaseDlg,
  JvSelectDirectory, System.Actions;

type
  TfmMain = class(TForm)
    btnStart: TButton;
    btnStop: TButton;
    ActionList: TActionList;
    actStartFileMan: TAction;
    actStopFileMan: TAction;
    actBackupFile: TAction;
    Panel2: TPanel;
    StatusBar: TStatusBar;
    GroupBox2: TGroupBox;
    edtFileIP: TLabeledEdit;
    edtFilePort: TLabeledEdit;
    actInit: TAction;
    ClientDataSet: TClientDataSet;
    DataSource: TDataSource;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ool1: TMenuItem;
    About1: TMenuItem;
    StartFileMan1: TMenuItem;
    StopFileMan1: TMenuItem;
    N1: TMenuItem;
    actExit: TAction;
    Exit1: TMenuItem;
    SendFile1: TMenuItem;
    actClearLog: TAction;
    ClearLog1: TMenuItem;
    actAbout: TAction;
    About2: TMenuItem;
    GroupBox4: TGroupBox;
    edtCommPort: TLabeledEdit;
    StaticText1: TStaticText;
    edtBaudRate: TLabeledEdit;
    actSendFile: TAction;
    Panel3: TPanel;
    mmLog: TMemo;
    GroupBox1: TGroupBox;
    lbxFolder: TListBox;
    Button2: TButton;
    Button3: TButton;
    actAddFolder: TAction;
    actDeleteFolder: TAction;
    DeleteFolder1: TMenuItem;
    SendFile2: TMenuItem;
    FileTimer: TTimer;
    ShowCDMALog1: TMenuItem;
    JvSelectDirectory: TJvSelectDirectory;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actInitExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actStartFileManExecute(Sender: TObject);
    procedure actStopFileManExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure actBackupFileExecute(Sender: TObject);
    procedure ShowCDMALog1Click(Sender: TObject);
    procedure actSendFileExecute(Sender: TObject);
    procedure actAddFolderExecute(Sender: TObject);
    procedure actDeleteFolderExecute(Sender: TObject);
    procedure FileTimerTimer(Sender: TObject);
  private
    procedure ShowLog(AMsg: String);
    procedure ShowError(APacket: TJsonData);

    procedure AddNewLine;
    procedure AddSpliter;
    procedure SetCDMAInfo;
  public
  published
    procedure rp_Terminate(APacket: TJsonData);

    procedure rp_Init(APacket: TJsonData);

    procedure rp_ShowMessage(APacket: TJsonData);
    procedure rp_ErrorMessage(APacket: TJsonData);

    procedure rp_CDMALog(APacket: TJsonData);
    procedure rp_PrepareSendFile(APacket: TJsonData);
    procedure rp_BeginSendFile(APacket: TJsonData);
    procedure rp_erSendFile(APacket: TJsonData);
    procedure rp_okSendFile(APacket: TJsonData);
    procedure rp_endSendFile(APacket: TJsonData);
    procedure rp_SendingFile(APacket: TJsonData);

  end;

var
  fmMain: TfmMain;

implementation

uses
  Global, JdcView, Option, Core, Common;

{$R *.dfm}

procedure TfmMain.actAboutExecute(Sender: TObject);
begin
  MessageDlg(COPY_RIGHT_FILEMAN + #13#10#13#10 + HOME_PAGE_URL, mtInformation,
    [mbOK], 0);
end;

procedure TfmMain.actClearLogExecute(Sender: TObject);
begin
  mmLog.Clear;
end;

procedure TfmMain.actDeleteFolderExecute(Sender: TObject);
begin
  lbxFolder.DeleteSelected;
  TOption.Obj.DataFolder := lbxFolder.Items.CommaText;
end;

procedure TfmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.actInitExecute(Sender: TObject);
begin
  TCore.Obj.Init;
end;

procedure TfmMain.SetCDMAInfo;
var
  conn: TConnInfo;
begin
  conn.StringValue := edtCommPort.Text;
  conn.IntegerValue := StrToIntDef(edtBaudRate.Text, CDMA_DEFAULT_BAUD_RATE);
  TOption.Obj.CDMAInfo := conn;

  conn.StringValue := edtFileIP.Text;
  conn.IntegerValue := StrToIntDef(edtFilePort.Text, FILE_DEFAULT_PORT);
  TOption.Obj.FileServer := conn;
end;

procedure TfmMain.actSendFileExecute(Sender: TObject);
begin
  SetCDMAInfo;
  TCore.Obj.SendFile;
end;

procedure TfmMain.actAddFolderExecute(Sender: TObject);
var
  path: String;
begin
  if lbxFolder.ItemIndex >= 0 then
    JvSelectDirectory.InitialDir := lbxFolder.Items[lbxFolder.ItemIndex];

  if JvSelectDirectory.Execute then
  begin
    path := JvSelectDirectory.Directory;

    if path = '' then
      exit;

    if lbxFolder.Items.IndexOf(path) > 0 then
      exit;

    lbxFolder.Items.Add(JvSelectDirectory.Directory);

    TOption.Obj.DataFolder := lbxFolder.Items.CommaText;
  end;
end;

procedure TfmMain.actBackupFileExecute(Sender: TObject);
begin
  SetCDMAInfo;
end;

procedure TfmMain.actStartFileManExecute(Sender: TObject);
begin
  FileTimer.Enabled := true;
  StatusBar.Panels[0].Text := 'FileMan ON';
end;

procedure TfmMain.actStopFileManExecute(Sender: TObject);
begin
  FileTimer.Enabled := false;
  StatusBar.Panels[0].Text := 'FileMan OFF';
  TCore.Obj.StopFileMan;
end;

procedure TfmMain.AddNewLine;
begin
  mmLog.Lines.Add('');
end;

procedure TfmMain.AddSpliter;
begin
  mmLog.Lines.Add('========================================');
  AddNewLine;
end;

procedure TfmMain.rp_BeginSendFile(APacket: TJsonData);
begin
  ShowLog('파일 전송... ' + APacket.Values['Msg']);
{$IFDEF DEBUG}
  PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), APacket.Values['Msg']);
{$ENDIF}
end;

procedure TfmMain.rp_endSendFile(APacket: TJsonData);
begin
  StatusBar.Panels[2].Text := '';
  ShowLog('파일 전송 완료. ');
  AddSpliter;
end;

procedure TfmMain.rp_erSendFile(APacket: TJsonData);
begin
  StatusBar.Panels[2].Text := '';
  AddSpliter;
  ShowLog('파일 전송에 실패 하였습니다. ');
  ShowLog(APacket.Values['Msg']);
  PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), APacket.Values['Msg']);
end;

procedure TfmMain.FileTimerTimer(Sender: TObject);
begin
  actSendFile.Execute;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TOption.Obj.CheckInterval := FileTimer.Interval;
  TCore.Obj.Terminate;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TView.Obj.Add(Self);
  TGlobal.Obj.ExeName := Application.ExeName;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  TGlobal.Obj.Finalize;
  TView.Obj.Remove(Self);
end;

procedure TfmMain.FormShow(Sender: TObject);
var
  conn: TConnInfo;
begin
  conn := TOption.Obj.CDMAInfo;
  edtCommPort.Text := conn.StringValue;
  edtBaudRate.Text := IntToStr(conn.IntegerValue);
  ShowCDMALog1.Checked := TOption.Obj.ShowCDMALog;

  conn := TOption.Obj.FileServer;
  edtFileIP.Text := conn.StringValue;
  edtFilePort.Text := IntToStr(conn.IntegerValue);

  lbxFolder.Items.CommaText := TOption.Obj.DataFolder;

  FileTimer.Interval := TOption.Obj.CheckInterval;

  actInit.Execute;
end;

procedure TfmMain.rp_ErrorMessage(APacket: TJsonData);
begin
  ShowError(APacket);
end;

procedure TfmMain.rp_Init(APacket: TJsonData);
begin
  actStartFileMan.Execute;
end;

procedure TfmMain.rp_PrepareSendFile(APacket: TJsonData);
begin
  StatusBar.Panels[2].Text := '파일 전송 준비 중..';
end;

procedure TfmMain.rp_CDMALog(APacket: TJsonData);
begin
  if TOption.Obj.ShowCDMALog then
    ShowLog('<C> ' + APacket.Values['Msg']);

{$IFDEF DEBUG}
  PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), APacket.Values['Msg']);
  Application.ProcessMessages;
{$ENDIF}
end;

procedure TfmMain.rp_Terminate(APacket: TJsonData);
begin
  Application.Terminate;
end;

procedure TfmMain.rp_SendingFile(APacket: TJsonData);
begin
  StatusBar.Panels[2].Text := '파일 전송 중... ' +
    FloatToStr(round(APacket.Doubles['Msg'] * 1000) / 10) + '%';
end;

procedure TfmMain.rp_ShowMessage(APacket: TJsonData);
begin
  ShowLog(APacket.Values['Msg']);
end;

procedure TfmMain.rp_okSendFile(APacket: TJsonData);
begin
  StatusBar.Panels[2].Text := '';
end;

procedure TfmMain.ShowCDMALog1Click(Sender: TObject);
begin
  ShowCDMALog1.Checked := not ShowCDMALog1.Checked;
  TOption.Obj.ShowCDMALog := ShowCDMALog1.Checked;
end;

procedure TfmMain.ShowError(APacket: TJsonData);
begin
  ShowLog('<E> ' + APacket.Values['Msg']);
  PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), APacket.Values['Msg']);
  Application.ProcessMessages;
end;

procedure TfmMain.ShowLog(AMsg: String);
begin
  Application.ProcessMessages;

  if mmLog.Lines.Count > 3000 then
    mmLog.Lines.Clear;

  if AMsg = '' then
    mmLog.Lines.Add('')
  else
    mmLog.Lines.Add(FormatDateTime('YYYY-MM-DD, HH:NN:SS.zzz, ', now) + AMsg);
end;

end.
