unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, NetDEVSDK,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    Button2: TButton;
    Button3: TButton;
    Panel1: TPanel;
    Button4: TButton;
    Button1: TButton;
    Button5: TButton;
    edtHost: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtID: TLabeledEdit;
    edtPwd: TLabeledEdit;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    NetDEV: TNetDEV;
    procedure OnException(AType: TNETDEVException);
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure ExceptionCallBack(plUserID: IntPtr; dwType: Int32;
  stAlarmInfo, plExpHandle, plUserData: IntPtr)stdcall;
begin
  raise Exception.Create('Device is offline');
end;

procedure SourceDataCallBack(lpRealHandle: IntPtr; var pucBuffer: Byte; dwBufSize: Int32;
  dwMediaDataType: Int32; lpUserParam: IntPtr)stdcall;
begin
  Format('%d %d', [dwBufSize, dwMediaDataType])
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  NetDEV.Logout;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if not NetDEV.Login(edtHost.Text, StrToIntDef(edtPort.Text, 80), edtID.Text, edtPwd.Text)
  then
    ShowMessage(NetDEV.GetLastError);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  PreviewInfo: TPreviewInfo;
begin
  PreviewInfo := TPreviewInfo.Create(Panel1.Handle);
  if not NetDEV.RealPlay(PreviewInfo) then
  begin
    Caption := NetDEV.GetLastError();
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  if not NetDEV.CapturePicture('d:\tmp.jpg', TNetDEVPictureFormat.NETDEV_PICTURE_JPG) then
    Caption := NetDEV.GetLastError;
end;

procedure TForm1.Button5Click(Sender: TObject);
begin
  NetDEV.StopRealPlay;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  NetDEV.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NetDEV := TNetDEV.Create(ExtractFilePath(Application.ExeName) + 'NetDEV');
  NetDEV.Init;
  NetDEV.OnException := OnException;
end;

procedure TForm1.OnException(AType: TNETDEVException);
begin
  raise Exception.Create('Error Message');
end;

end.
