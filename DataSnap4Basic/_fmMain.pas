unit _fmMain;

interface

uses Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvToolEdit, JvCombobox, JvExStdCtrls,
  JvDBCombobox, ValueList, Vcl.ActnList, Vcl.ComCtrls, Vcl.ExtCtrls,
  JvTextListBox, Vcl.Menus, DBXJSON, System.Actions, System.UITypes,
  Vcl.AppEvnts;

type
  TfmMain = class(TForm)
    mmLog: TMemo;
    ActionList: TActionList;
    actClearLog: TAction;
    StatusBar: TStatusBar;
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
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure actClearLogExecute(Sender: TObject);
    procedure actOptionsExecute(Sender: TObject);
    procedure actExitExecute(Sender: TObject);
    procedure actAboutExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
  private

  public
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses MyGlobal, MyOption, JdcGlobal, _fmOption, ServerContainerUnit;

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

procedure TfmMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  mmLog.Lines.Add('SysError, ' + E.Message);
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TGlobal.Obj.Finalize;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TGlobal.Obj.ExeName := ParamStr(0);
  TGlobal.Obj.Initialize;
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  Caption := APPLICATION_TITLE;
end;

end.
