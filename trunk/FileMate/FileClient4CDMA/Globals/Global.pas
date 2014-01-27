unit Global;

interface

uses
  ValueList,
  Classes, SysUtils, System.IOUtils;

const
  HOUR = 3600000;

  COPY_RIGHT_FILEMAN = 'FileMan v1.2 - ¨Ï 2012 ENB GROUP';
  HOME_PAGE_URL = 'http://www.enbgourp.co.kr';

  LOCAL_HOST = '127.0.0.1';
  FILE_DEFAULT_PORT = 8001;
  DEFAULT_BUFFER_SIZE = 512;

  CDMA_DEFAULT_COMM_PORT = 'COM1';
  CDMA_DEFAULT_BAUD_RATE = 9600;

  TAB_CHAR = #9;

  BACKUP_FOLDER = '\Data';
  TEMP_FOLDER = 'temp';

type

  TConnInfo = record
    StringValue: String;
    IntegerValue: Integer;
  end;

  TOpenSourceCallBack = procedure(SourceData: TStringList) of object;

type
  TGlobal = class(TComponent)
  strict private
    FInitialized: boolean;
  private
    FExeName: String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function Obj: TGlobal;

    procedure Initialize;
    procedure Finalize;

    procedure BackUpFile(FileName: String; SubFolder: String);
  published
    property Initialized: boolean read FInitialized;
    property ExeName: String read FExeName write FExeName;
  end;

implementation

uses
  Option, Common;

var
  MyObj: TGlobal = nil;

  { TGlobal }

procedure TGlobal.BackUpFile(FileName: String; SubFolder: String);
var
  BackUpDir: String;
  SrcFileName: String;
  DstFileName: String;
begin
  SrcFileName := FileName;
  BackUpDir := ExtractFileDir(TGlobal.Obj.ExeName) + BACKUP_FOLDER + '/' +
    SubFolder;
  DstFileName := ChangeFilePath(SrcFileName, BackUpDir);

  if not TDirectory.Exists(BackUpDir) then
    TDirectory.CreateDirectory(BackUpDir);

  if FileExists(SrcFileName) then
  begin
    TFile.Copy(SrcFileName, DstFileName, true);
    TFile.Delete(SrcFileName);
  end;

end;

constructor TGlobal.Create(AOwner: TComponent);
begin
  inherited;

  FInitialized := false;
end;

destructor TGlobal.Destroy;
begin
  Finalize;

  inherited;
end;

procedure TGlobal.Finalize;
begin
  if not FInitialized then
    Exit;
  FInitialized := false;

  // Todo :
end;

procedure TGlobal.Initialize;
begin
  if FInitialized then
    Exit;

  // Todo :

  FInitialized := true;
end;

class function TGlobal.Obj: TGlobal;
begin
  if MyObj = nil then
    MyObj := TGlobal.Create(nil);
  result := MyObj;
end;

end.
