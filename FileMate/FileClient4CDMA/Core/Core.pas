unit Core;

interface

uses Classes, SysUtils, FileClient, JdcCDMA, System.IOUtils, System.Types;

type
  TCore = class
  private
    FFileClient: TFileClient;
    constructor Create;
  public
    class function Obj: TCore;

    procedure Terminate;

    procedure Init;

    procedure SendFile;
    procedure StopFileMan;
  end;

implementation

uses Global, Option, JdcView, Common;

var
  MyObj: TCore = nil;

  { TCore }
class function TCore.Obj: TCore;
begin
  if MyObj = nil then
    MyObj := TCore.Create;
  result := MyObj;
end;

constructor TCore.Create;
begin
  FFileClient := TFileClient.Create;
end;

procedure TCore.Init;
begin
  TView.Obj.sp_SyncMessage('Init');
end;

procedure TCore.SendFile;

  function MoveFile: boolean;
  var
    Folders: TStringList;
    MyFolder: String;
    Files: TStringDynArray;
    I: Integer;
  begin

    result := false;
    Folders := TStringList.Create;
    try
      Folders.CommaText := TOption.Obj.DataFolder;

      for MyFolder in Folders do
      begin
        if not TDirectory.Exists(MyFolder) then
          Continue;

        Files := TDirectory.GetFiles(MyFolder);
        for I := Low(Files) to High(Files) do
        begin
          TGlobal.Obj.BackupFile(Files[I], TEMP_FOLDER);
          result := true;
        end;
      end;

    finally
      Folders.Free;
    end;

  end;

begin

  if MoveFile then
  begin
    FFileClient.CompressFile(TEMP_FOLDER);
    FFileClient.SendFile(nil);
  end;

end;

procedure TCore.StopFileMan;
begin
  FFileClient.Cancel;
end;

procedure TCore.Terminate;
begin
  if Assigned(FFileClient) then
    FreeAndNil(FFileClient);
end;

end.
