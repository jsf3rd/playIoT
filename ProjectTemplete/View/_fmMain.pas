unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Winapi.Shellapi,
  Vcl.Controls, Vcl.Forms, Vcl.ActnList, ValueList, Vcl.Dialogs, System.Actions,
  Vcl.Menus, Vcl.AppEvnts, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TfmMain = class(TForm)
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ool1: TMenuItem;
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
    ShowIniFile1: TMenuItem;
    ShowLog1: TMenuItem;
    actDebug: TAction;
    DebugLog1: TMenuItem;
    N1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure actClearLogExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actShowIniExecute(Sender: TObject);
    procedure actShowLogExecute(Sender: TObject);
    procedure actTestMenuExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure actDebugExecute(Sender: TObject);
  published
    procedure rp_Terminate(APacket: TValueList);
    procedure rp_Init(APacket: TValueList);

    procedure rp_ErrorMessage(APacket: TValueList);
    procedure rp_LogMessage(APacket: TValueList);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses JdcGlobal, MyGlobal, MyOption, MyCommon, JdcView, Core, System.UITypes;

procedure TfmMain.actAboutExecute(Sender: TObject);
begin
  MessageDlg(APPLICATION_TITLE + ' ' + FileVersion(Application.ExeName) + ' ' + COPY_RIGHT_SIGN
    + #13#10#13#10 + HOME_PAGE_URL, mtInformation, [mbOK], 0);
end;

procedure TfmMain.actClearLogExecute(Sender: TObject);
begin
  // ClipBoard.AsText := mmLog.Lines.Text;
  // mmLog.Clear;
end;

procedure TfmMain.actDebugExecute(Sender: TObject);
begin
  actDebug.Checked := not actDebug.Checked;
  TGlobal.Obj.UseDebug := actDebug.Checked;
end;

procedure TfmMain.actExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TfmMain.actShowIniExecute(Sender: TObject);
begin
  ShellExecute(handle, 'open', PWideChar('notepad.exe'),
    PWideChar(TOption.Obj.IniFile.FileName), '', SW_SHOWNORMAL);
end;

procedure TfmMain.actShowLogExecute(Sender: TObject);
begin
  ShellExecute(handle, 'open', PWideChar('notepad.exe'), PWideChar(TGlobal.Obj.LogName), '',
    SW_SHOWNORMAL);
end;

procedure TfmMain.actTestMenuExecute(Sender: TObject);
begin
  MenuTest.Visible := not MenuTest.Visible;
end;

procedure TfmMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  TGlobal.Obj.ApplicationMessage(msError, 'System Error', '[%s] %s',
    [Sender.ClassName, E.Message]);
end;

procedure TfmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  if MessageDlg(APPLICATION_TITLE + '을(를) 종료하시겠습니까?', TMsgDlgType.mtConfirmation, mbYesNo, 0) = mrYes
  then
    TCore.Obj.Finalize;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TGlobal.Obj.ExeName := Application.ExeName;
  TView.Obj.Add(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  TView.Obj.Remove(Self);
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  TCore.Obj.Initialize;
end;

procedure TfmMain.rp_ErrorMessage(APacket: TValueList);
begin
  MessageDlg('오류 : ' + APacket.Values['Name'] + #13#10 + APacket.Values['Msg'],
    TMsgDlgType.mtError, [mbOK], 0);
end;

procedure TfmMain.rp_Init(APacket: TValueList);
begin
  Caption := TOption.Obj.AppName + ' ' + FileVersion(Application.ExeName);
  actDebug.Checked := TGlobal.Obj.UseDebug;
end;

procedure TfmMain.rp_LogMessage(APacket: TValueList);
begin
  MessageDlg('알림 : ' + APacket.Values['Name'] + #13#10 + APacket.Values['Msg'],
    TMsgDlgType.mtInformation, [mbOK], 0);
end;

procedure TfmMain.rp_Terminate(APacket: TValueList);
begin
  Application.Terminate;
end;

end.
