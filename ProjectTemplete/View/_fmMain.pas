unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ActnList, ValueList, Vcl.Dialogs, System.Actions,
  Vcl.Menus, Vcl.AppEvnts;

type
  TfmMain = class(TForm)
    ActionList: TActionList;
    actAbout: TAction;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    ool1: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAboutExecute(Sender: TObject);
    procedure ApplicationEventsException(Sender: TObject; E: Exception);
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

uses Option, Global, JdcView2, Core, JdcGlobal, Common, System.UITypes;

procedure TfmMain.actAboutExecute(Sender: TObject);
begin
  MessageDlg(APPLICATION_TITLE + ' ' + APPLICATION_VERSION + ' ' +
    COPY_RIGHT_SIGN + #13#10#13#10 + HOME_PAGE_URL, mtInformation, [mbOK], 0);
end;

procedure TfmMain.ApplicationEventsException(Sender: TObject; E: Exception);
begin
  TView.Obj.sp_ErrorMessage('System Error on ' + Sender.ClassName + ', ' +
    E.Message);
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TCore.Obj.Finalize;
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
  // PrintDebug(mmLog, '<ERR> ' + APacket.Values['Msg']);
  // PrintDebug(TGlobal.Obj.LogName, '<ERR> ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_Init(APacket: TValueList);
begin
  // TODO : Init Form
end;

procedure TfmMain.rp_LogMessage(APacket: TValueList);
begin
  // TODO : Print Debug Log
  // PrintDebug('::LOG:: ' + APacket.Values['Msg']);
  // PrintDebug(TGlobal.Obj.LogName, '<LOG> ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_ShowMessage(APacket: TValueList);
begin
  // TODO : Print User Message..
  // PrintDebug(mmLog, '<MSG> ' + APacket.Values['Msg']);
end;

procedure TfmMain.rp_Terminate(APacket: TValueList);
begin
  Application.Terminate;
end;

end.
