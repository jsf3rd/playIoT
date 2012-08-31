unit JdcFileTransfer;

interface

uses System.SysUtils, System.Classes, IdTCPClient, IdContext, Vcl.ExtCtrls,
  ThreadRepeater, IdIOHandler, IdExceptionCore, Global,
  IdException, System.IOUtils, System.Types, SerialNG, IdGlobal,
  JdcGlobal, JdcCdmaTcp, JdcCdma;

const
  ZLIB_EXT = '.zilb';
  THIRTY_MINS = 1800000;

type
  TFileTransfer = class abstract
  protected
    FActive: boolean;

    FSendTimer: TTimer;
    FFileThread: TThreadRepeater;

    FSubFolder: String;
    FFiles: TStringList;
    FTransfer: ICDMA;
    FCheckCount: Integer;

    FFileIndex: Integer;
    FFileBytes: TBytes;

    FBufferSize: Integer;

    procedure OnSendTimer(Sender: TObject);

    procedure SNGTCPClientConnected(Sender: TObject);
    procedure SNGTCPClientDisconnected(Sender: TObject);
    procedure SNGTCPSendDone(Sender: TObject);

    procedure DoCompress(Sender: TObject); virtual; abstract;

    function LoadZipFiles: Integer;
    procedure DoSendProcess; virtual; abstract;
  public
    constructor Create(AInterval : Integer = THIRTY_MINS);
    destructor Destroy; override;

    procedure CompressFile(SubFolder: String); virtual; abstract;
    procedure SendFile(Sender: TObject);

    property Active: boolean read FActive;
  end;

implementation

{ TFileClient }

uses Option, Common, View;

constructor TFileTransfer.Create(AInterval : Integer = THIRTY_MINS);
var
  TCPInfo: TTCPInfo;
begin
  // 30분 주기로 zip 파일이 있는지 체크한다.
  FSendTimer := TTimer.Create(nil);
  FSendTimer.Interval := AInterval;
  FSendTimer.OnTimer := OnSendTimer;
  FSendTimer.Enabled := true;

  TCPInfo := TTCPInfo.Create;
  TCPInfo.OnTCPConnected := SNGTCPClientConnected;
  TCPInfo.OnTCPDisconnected := SNGTCPClientDisconnected;
  TCPInfo.OnTCPSendDone := SNGTCPSendDone;

  // CDMA 명령어 Context
  FTransfer := TCdmaTcp.Create(TCPInfo);

  FFiles := TStringList.Create;

  FActive := false;

  FBufferSize := TOption.Obj.BufferSize;
end;

destructor TFileTransfer.Destroy;
begin
  if Assigned(FTransfer) then
  begin
    FTransfer := nil;
  end;

  FSendTimer.Free;

  FFiles.Free;
  inherited;
end;

procedure TFileTransfer.SNGTCPClientConnected(Sender: TObject);
begin
  FCheckCount := 0;
  DoSendProcess;
end;

procedure TFileTransfer.SNGTCPClientDisconnected(Sender: TObject);
begin
  FTransfer.Disconnect;

  if FFiles.Count > 0 then
  begin
    TView.Obj.sp_SyncMessage('erSendFile', FFiles.Text);
  end
  else
    TView.Obj.sp_SyncMessage('endSendFile');

  FActive := false;
end;

procedure TFileTransfer.OnSendTimer(Sender: TObject);
begin
  SendFile(Self);
end;

procedure TFileTransfer.SendFile(Sender: TObject);
var
  conn: TConnInfo;
begin
  if FTransfer.Connected then
    exit;

  if LoadZipFiles = 0 then
    exit; // zip 파일 없음..

  TView.Obj.sp_AsyncMessage('PrepareSendFile');

  try
    with TCdmaTcp(FTransfer) do
    begin
      conn := TOption.Obj.CDMAInfo;
      CommPort := conn.StringValue;
      BaudRate := conn.IntegerValue;
      Connect;

      // Init..
      conn := TOption.Obj.FileServer;
      TCPInfo.TCPHost := conn.StringValue;
      TCPInfo.TCPPort := conn.IntegerValue;
      OpenTCP;
    end;
  except
    on E: Exception do
      TView.Obj.sp_ErrorMessage(E.Message);
  end;
end;

function TFileTransfer.LoadZipFiles: Integer;
var
  Dir: String;
  ZipFiles: TStringDynArray;
  I: Integer;

begin
  result := 0;

  FFiles.Clear;

  Dir := ExtractFileDir(TGlobal.Obj.ExeName) + BACKUP_FOLDER;

  if not DirectoryExists(Dir) then
    exit;

  // zlib 파일 삭제
  ZipFiles := TDirectory.GetFiles(Dir, '*' + ZLIB_EXT,
    TSearchOption.soAllDirectories);

  result := Length(ZipFiles);

  for I := Low(ZipFiles) to High(ZipFiles) do
  begin
    FFiles.Add(ZipFiles[I]);
  end;

end;

procedure TFileTransfer.SNGTCPSendDone(Sender: TObject);
begin
  DoSendProcess;
end;

end.
