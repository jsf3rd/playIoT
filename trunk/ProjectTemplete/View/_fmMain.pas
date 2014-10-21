unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.ActnList, ValueList, Vcl.Dialogs, System.Actions;

type
  TfmMain = class(TForm)
    ActionList: TActionList;
    actAbout: TAction;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actAboutExecute(Sender: TObject);
  private
  published
    procedure rp_Terminate(APacket: TValueList);
    procedure rp_Init(APacket: TValueList);

    procedure rp_ShowMessage(APacket: TValueList);
    procedure rp_ErrorMessage(APacket: TValueList);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses Option, Global, JdcView2, Core, JdcGlobal, Common, System.UITypes;

procedure TfmMain.actAboutExecute(Sender: TObject);
begin
  MessageDlg(COPY_RIGHT_FILEMAN + #13#10#13#10 + HOME_PAGE_URL, mtInformation,
    [mbOK], 0);
end;

procedure TfmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  TCore.Obj.Finalize;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  TGlobal.Obj.ExeName := Application.ExeName;
  TView.Obj.Add(Self);
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  TGlobal.Obj.Finalize;
  TView.Obj.Remove(Self);
end;

procedure TfmMain.FormShow(Sender: TObject);
begin
  TCore.Obj.Initialize;
end;

procedure TfmMain.rp_ErrorMessage(APacket: TValueList);
begin
  // TODO : Print Error Log..
end;

procedure TfmMain.rp_Init(APacket: TValueList);
begin
  // TODO : Init Form
end;

procedure TfmMain.rp_ShowMessage(APacket: TValueList);
begin
  // TODO : Print User Message..
end;

procedure TfmMain.rp_Terminate(APacket: TValueList);
begin
  Application.Terminate;
end;

end.
