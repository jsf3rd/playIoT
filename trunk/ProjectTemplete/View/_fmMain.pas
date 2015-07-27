unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Winapi.Shellapi,
  Vcl.Controls, Vcl.Forms, Vcl.ActnList, ValueList, Vcl.Dialogs, System.Actions,
  Vcl.Menus, Vcl.AppEvnts, Vcl.ExtCtrls;

type
  TfmMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ool1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    TrayIcon: TTrayIcon;
    ActionList: TActionList;
    actAbout: TAction;
    actClearLog: TAction;
    actExit: TAction;
    actShowIni: TAction;
    actShowLog: TAction;
    actTestMenu: TAction;
    MenuTest: TMenuItem;
    Exit1: TMenuItem;
    ShowIniFile1: TMenuItem;
    ShowLog1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAboutExecute(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actClearLogExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actShowIniExecute(Sender: TObject);
    procedure actShowLogExecute(Sender: TObject);
    procedure actTestMenuExecute(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    procedure ApplicationEventsMinimize(Sender: TObject);
  private
  published
    procedure rp_Terminate(APacket: TValueList);
    procedure rp_Init(APacket: TValueList);

    procedure rp_ShowMessage(APacket: TValueList);
    procedure rp_ErrorMessage(APacket: TValueList);
    procedure rp_LogMessage(APacket: TValueList);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses MyGlobal, MyOption, MyCommon, JdcView2, Core, JdcGlobal, System.UITypes;

procedure TfmMain.actAboutExecute(Sender: TObject);
begin
  MessageDlg(APPLICATION_TITLE + ' ' + APPLICATION_VERSION + ' ' +
    COPY_RIGHT_SIGN + #13#10#13#10 + HOME_PAGE_URL, mtInformation, [mbOK], 0);
end;

procedure TfmMain.actClearLogExecute(Sender: TObject);
begin
  // ClipBoard.AsText := mmLog.Lines.Text;
  // mmLog.Clear;
end;

procedure TfmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.actShowIniExecute(Sender: TObject);
begin
  ShellExecute(handle, 'open', PWideChar('notepad.exe'),
    PWideChar(TOption.Obj.IniName), '', SW_SHOWNORMAL);
end;

procedure TfmMain.actShowLogExecute(Sender: TObject);
begin
  ShellExecute(handle, 'open', PWideChar('notepad.exe'),
    PWideChar(TGlobal.Obj.LogName), '', SW_SHOWNORMAL);
end;

procedure TfmMain.actTestMenuExecute(Sender: TObject);
begin
  MenuTest.Visible := not MenuTest.Visible;
end;

procedure TfmMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  TView.Obj.sp_ErrorMessage('System Error on ' + Sender.ClassName + ', ' +
    E.Message);
end;

procedure TfmMain.ApplicationEventsMinimize(Sender: TObject);
begin
  Hide;
  WindowState := wsMinimized;
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TCore.Obj.Finalize;
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := MessageDlg(APPLICATION_TITLE + '을(를) 종료하시겠습니까?',
    TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TGlobal.Obj.ExeName := Application.ExeName;
  TView.Obj.Add(Self);
  TCore.Obj.Initialize;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  TGlobal.Obj.Finalize;
  TView.Obj.Remove(Self);
end;

procedure TfmMain.rp_ErrorMessage(APacket: TValueList);
begin
  // TODO : Print Error Log..
  // PrintLog(mmLog, '<ERR> ' + APacket.Values['Msg']);
  // PrintLog(TGlobal.Obj.LogName, '<ERR> ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_Init(APacket: TValueList);
begin
  // TODO : Form Initialized
end;

procedure TfmMain.rp_LogMessage(APacket: TValueList);
begin
  // TODO : Print Debug Log
  // PrintDebug('::LOG:: ' + APacket.Values['Msg']);
  // PrintLog(mmLog, '<ERR> ' + APacket.Values['Msg']);
  // PrintLog(TGlobal.Obj.LogName, '<LOG> ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_ShowMessage(APacket: TValueList);
begin
  // TODO : Print User Message..
  // PrintLog(mmLog, '<MSG> ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_Terminate(APacket: TValueList);
begin
  Application.Terminate;
end;

procedure TfmMain.TrayIconDblClick(Sender: TObject);
begin
  Show;
  WindowState := wsNormal;
  Application.BringToFront;
end;

end.
