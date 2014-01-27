unit fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IdContext, Vcl.ExtCtrls, Vcl.StdCtrls,
  IdBaseComponent, IdComponent, IdCustomTCPServer, IdTCPServer, Vcl.ComCtrls,
  System.IniFiles, IdGlobal, System.ZLib, System.IOUtils,
  Vcl.Mask, JvExMask, JvToolEdit;

type
  TMainForm = class(TForm)
    IdTCPServer: TIdTCPServer;
    btnStart: TButton;
    Panel1: TPanel;
    mmLog: TMemo;
    btnStop: TButton;
    edtPort: TLabeledEdit;
    StatusBar: TStatusBar;
    edtFolder: TJvDirectoryEdit;
    Label1: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edtFolderChange(Sender: TObject);
    procedure IdTCPServerExecute(AContext: TIdContext);
    procedure IdTCPServerConnect(AContext: TIdContext);
    procedure IdTCPServerDisconnect(AContext: TIdContext);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

procedure TMainForm.btnStartClick(Sender: TObject);
var
  Ini: TIniFile;
begin
  btnStop.Enabled := true;
  btnStart.Enabled := false;

  IdTCPServer.Active := false;
  IdTCPServer.Bindings.Clear;
  IdTCPServer.Bindings.DefaultPort := StrToIntDef(edtPort.Text, 8001);
  IdTCPServer.Active := true;

  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Ini.WriteInteger('Option', 'Port', IdTCPServer.Bindings.DefaultPort);
  finally
    Ini.Free;
  end;

  StatusBar.Panels[0].Text := 'Server ON';
  StatusBar.Panels[1].Text := 'Port : ' +
    IntToStr(IdTCPServer.Bindings.DefaultPort);

end;

procedure TMainForm.btnStopClick(Sender: TObject);
begin
  btnStop.Enabled := false;
  btnStart.Enabled := true;

  IdTCPServer.Active := false;
  StatusBar.Panels[0].Text := 'Server OFF';
  StatusBar.Panels[1].Text := '';
end;

procedure TMainForm.edtFolderChange(Sender: TObject);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    Ini.WriteString('Option', 'Folder', edtFolder.Text);
  finally
    Ini.Free;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  btnStop.Enabled := false;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  Ini: TIniFile;
begin

  Ini := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
  try
    edtPort.Text := Ini.ReadString('Option', 'Port', '8001');
    edtFolder.Text := Ini.ReadString('Option', 'Folder',
      ExtractFileDir(Application.ExeName) + '\Data');
  finally
    Ini.Free;
  end;
  StatusBar.Panels[0].Text := 'Server OFF';
  StatusBar.Panels[1].Text := '';

  btnStartClick(nil);
end;

procedure TMainForm.IdTCPServerConnect(AContext: TIdContext);
begin
  mmLog.Lines.Add('');
  mmLog.Lines.Add(FormatDateTime('YYYY-MM-DD HH:NN', now) + ' Connected : ' +
    AContext.Binding.PeerIP + ':' + IntToStr(AContext.Binding.PeerPort));
end;

procedure TMainForm.IdTCPServerDisconnect(AContext: TIdContext);
begin
  mmLog.Lines.Add(FormatDateTime('YYYY-MM-DD HH:NN', now) + ' Disconnected : ' +
    AContext.Binding.PeerIP + ':' + IntToStr(AContext.Binding.PeerPort));
  mmLog.Lines.Add('========================================');
  mmLog.Lines.Add('');

end;

function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;
const
  BuffSize = 65535; // 버퍼 사이즈
var
  DS: TZDeCompressionStream;
  Buff: PChar; // 임시 버퍼
  ReadSize: integer; // 읽은 크기
begin
  if Stream = OutStream then
    // 입력 스트림과 출력스트림이 같으면 문제가 발생한다
    raise Exception.Create('입력 스트림과 출력 스트림이 같습니다');
  Stream.Position := 0; // 스트림 커서 초기화
  OutStream.Position := 0;
  // 인풋 스트림을 옵션으로 객체 생성.
  DS := TZDeCompressionStream.Create(Stream);
  try
    if Assigned(OnProgress) then
      DS.OnProgress := OnProgress;
    GetMem(Buff, BuffSize);
    try
      // 버퍼 사이즈만큼 읽어온다. Read함수를 부르면 압축이 풀리게 된다.
      repeat
        ReadSize := DS.Read(Buff^, BuffSize);
        if ReadSize <> 0 then
          OutStream.Write(Buff^, ReadSize);
      until ReadSize < BuffSize;
      if Assigned(OnProgress) then
        OnProgress(DS); // Compress와 같은이유
      Result := true;
    finally
      FreeMem(Buff)
    end;
  finally
    DS.Free;
  end;
end;

procedure TMainForm.IdTCPServerExecute(AContext: TIdContext);
var
  FileName: String;
  Stream, OutStream: TFileStream;

  Data: TIdBytes;
  DataFiles: TBytesStream;
  Size: integer;
begin
  FileName := AContext.Connection.IOHandler.ReadLn(#13);
  mmLog.Lines.Add('');
  mmLog.Lines.Add('FileName : ' + FileName);

  SetLength(Data, 0);
  AContext.Connection.IOHandler.ReadBytes(Data, SizeOf(Int64));
  Size := BytesToInt64(Data);

  SetLength(Data, 0);
  AContext.Connection.IOHandler.ReadBytes(Data, Size);

  mmLog.Lines.Add('Received : ' + IntToStr(Size) + 'Bytes');

  FileName := edtFolder.Text + FileName;

  if not TDirectory.Exists(ExtractFileDir(FileName)) then
    TDirectory.CreateDirectory(ExtractFileDir(FileName));

  DataFiles := TBytesStream.Create;
  WriteTIdBytesToStream(DataFiles, Data);
  DataFiles.SaveToFile(FileName);
  DataFiles.Free;

  Stream := TFileStream.Create(FileName, fmOpenRead);
  OutStream := TFileStream.Create(ChangeFileExt(FileName, ''), fmCreate);
  try
    DeCompressStream(Stream, OutStream, nil);
  finally
    OutStream.Free;
    Stream.Free;
    TFile.Delete(FileName);
  end;

end;

end.
