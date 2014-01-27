unit FileClient;

interface

uses System.SysUtils, System.Classes, IdTCPClient, IdContext, Vcl.ExtCtrls,
  IdIOHandler, IdExceptionCore, Global,
  IdException, System.IOUtils, System.Types, IdGlobal,
  JdcGlobal, JdcCdmaTcp, JdcCdma, JdcFileTransfer;

type

  TFileClient = class(TFileTransfer)
  private
    procedure DoCompress(Sender: TObject); override;
    procedure DoSendProcess; override;
  public
    procedure CompressFile(SubFolder: String); override;
  end;

implementation

{ TFileClient }

uses Option, Common, JdcView;

procedure TFileClient.CompressFile(SubFolder: String);
begin
  FSubFolder := SubFolder;
  DoCompress(nil);
end;

procedure TFileClient.DoCompress(Sender: TObject);
var
  Dir: String;
  Files: TStringDynArray;
  I: Integer;
  Stream, OutStream: TFileStream;
  SubFolder: String;
begin
  FActive := true;

  FSendTimer.Enabled := false;
  try
    Dir := ExtractFileDir(TGlobal.Obj.ExeName) + BACKUP_FOLDER + '\' +
      FSubFolder;

    if not DirectoryExists(Dir) then
      exit;

    // zlib »ý¼º
    Files := TDirectory.GetFiles(Dir);
    for I := Low(Files) to High(Files) do
    begin
      if ExtractFileExt(Files[I]) = ZLIB_EXT then
        Continue;

      Stream := TFileStream.Create(Files[I], fmOpenRead);
      OutStream := TFileStream.Create(Files[I] + ZLIB_EXT, fmCreate);
      try
        CompressStream(Stream, OutStream, nil);
      finally
        OutStream.Free;
        Stream.Free;

        SubFolder := GetSubFolder(ExtractFileName(Files[I]));
        TGlobal.Obj.BackupFile(Files[I], SubFolder);
      end;
    end;

  finally
    FSendTimer.Enabled := true;
  end;

end;

procedure TFileClient.DoSendProcess;
  procedure SetCommand(Msg: String); overload;
  begin
    FTransfer.SetCommand(COMMAND_WRITE + ToHex(ToBytes(Msg + #13)));
  end;

  procedure SetCommand(Msg: TIdBytes); overload;
  begin
    FTransfer.SetCommand(COMMAND_WRITE + ToHex(Msg));
  end;

var
  FileName: String;
  FileContents: TBytesStream;
begin

  if FFiles.Count = 0 then
  begin
    TCdmaTcp(FTransfer).CloseTCP;
    exit;
  end;

  case FCheckCount of
    0:
      begin
        inc(FCheckCount);
        FileName := '\' + ExtractFileName(FFiles.Strings[0]);
        SetCommand(FileName);

        TView.Obj.sp_AsyncMessage('BeginSendFile', ExtractFileName(FileName));
      end;
    1:
      begin
        inc(FCheckCount);
        FileName := FFiles.Strings[0];
        FileContents := TBytesStream.Create;
        FileContents.LoadFromFile(FileName);
        SetCommand(ToBytes(FileContents.Size));
        FileContents.Free;

        FFileIndex := 0;
      end;
    2:
      begin
        inc(FCheckCount);
        FileName := FFiles.Strings[0];
        FileContents := TBytesStream.Create;
        FileContents.LoadFromFile(FileName);

        SetLength(FFileBytes, FileContents.Size);
        ReadTIdBytesFromStream(FileContents, FFileBytes, FileContents.Size);
        FileContents.Free;

        DoSendProcess;
      end;
    3:
      begin

      end;

        SetCommand(ToBytes(FFileBytes, FBufferSize, FFileIndex));

        if (FFileIndex + FBufferSize) >= Length(FFileBytes) then
        begin
          inc(FCheckCount);

          TView.Obj.sp_AsyncMessage('SendingFile', '1');

        end
        else
        begin
          FFileIndex := FFileIndex + FBufferSize;

          TView.Obj.sp_AsyncMessage('SendingFile',
            FloatToStr(FFileIndex / Length(FFileBytes)));
        end;

      end;
    4:
      begin
        FCheckCount := 0;

        FileName := FFiles.Strings[0];
        DeleteFile(FileName);
        FFiles.Delete(0);

        TView.Obj.sp_AsyncMessage('okSendFile', ExtractFileName(FileName));

        DoSendProcess;
      end;
  else
    begin
      Assert(false, 'SendProcess Count Error.');
    end;

  end;

end;

end.
