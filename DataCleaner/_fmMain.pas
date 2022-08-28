unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, JdcLogging,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.AppEvnts,
  Vcl.ComCtrls, Vcl.StdCtrls, JvExStdCtrls, JvEdit, System.IOUtils,
  System.DateUtils, System.Types, JdcGlobal, System.StrUtils, System.ZLib,
  ShellAPI;

type
  TfmMain = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FDebug: boolean;
    FSubFolder: boolean;

    procedure PrintLog(AValue: string);

    procedure CheckDir(APath: string; ADateTime: TDateTime);
    procedure CheckFile(MyFile: string; ADateTime: TDateTime);

    function GetCommandFTP: string;
  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

uses Global, Option;

procedure TfmMain.FormCreate(Sender: TObject);
var
  DateTime: TDateTime;
begin
  try
    try
      TGlobal.Obj.ExeName := Application.ExeName;

      TOption.Obj.Year := TOption.Obj.Year;
      TOption.Obj.Month := TOption.Obj.Month;
      TOption.Obj.Day := TOption.Obj.Day;
      TOption.Obj.Hour := TOption.Obj.Hour;
      TOption.Obj.Minute := TOption.Obj.Minute;
      TOption.Obj.Second := TOption.Obj.Second;

      TOption.Obj.SubDir := TOption.Obj.SubDir;
      TOption.Obj.Debug := TOption.Obj.Debug;
      TOption.Obj.SearchPattern := TOption.Obj.SearchPattern;
      // 파일 복사 하는 방식 FTP로 바꿈
      TOption.Obj.FTPServer := TOption.Obj.FTPServer;
      TOption.Obj.DeleteExtension := TOption.Obj.DeleteExtension;

      if (TOption.Obj.Year + TOption.Obj.Month + TOption.Obj.Day + TOption.Obj.Hour +
        TOption.Obj.Minute + TOption.Obj.Second) = 0 then
        Exit;

      DateTime := now;
      DateTime := IncYear(DateTime, -1 * TOption.Obj.Year);
      DateTime := IncMonth(DateTime, -1 * TOption.Obj.Month);
      DateTime := IncDay(DateTime, -1 * TOption.Obj.Day);
      DateTime := IncHour(DateTime, -1 * TOption.Obj.Hour);
      DateTime := IncMinute(DateTime, -1 * TOption.Obj.Minute);
      DateTime := IncSecond(DateTime, -1 * TOption.Obj.Second);

      FDebug := TOption.Obj.Debug;
      FSubFolder := TOption.Obj.SubDir;

      CheckDir(ExtractFilePath(TGlobal.Obj.ExeName), DateTime);
    except
      on E: Exception do
        PrintLog('Error - ' + E.Message);
    end;

  finally
    Application.Terminate;
  end;
end;

function TfmMain.GetCommandFTP: string;
var
  FTPServer: TFTPServer;
begin
  FTPServer := TOption.Obj.FTPServer;

  Result := '-u ' + FTPServer.User + ' -p ' + FTPServer.Password + ' -m -DD ';
  if FDebug then
    Result := Result + '-d ncftpupt.log ';
  Result := Result + '-P ' + FTPServer.Port + ' ' + FTPServer.Address + ' ';
end;

procedure TfmMain.PrintLog(AValue: string);
begin
  JdcLogging.PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), now, AValue);
end;

procedure TfmMain.CheckDir(APath: string; ADateTime: TDateTime);
var
  Files: TStringDynArray;
  MyDir, MyFile: String;
  tmp: string;
  WriteTime: TDateTime;
begin
  if not TDirectory.Exists(APath) then
    raise Exception.Create('Not exist. ' + APath);

  for MyDir in TDirectory.GetDirectories(APath) do
  begin
    CheckDir(MyDir, ADateTime);
  end;

  // File Copy  FTPServer
  tmp := ChangeFileExt(ExtractFileName(TGlobal.Obj.ExeName), '');
  for MyFile in TDirectory.GetFiles(APath, TOption.Obj.SearchPattern) do
  begin
    try
      CheckFile(MyFile, ADateTime);
    except
      on E: Exception do
        PrintLog('Error on CheckFile, ' + MyFile);
    end;
    Sleep(1);
  end;

  // File Delete - .tdms_index
  for MyFile in TDirectory.GetFiles(APath, TOption.Obj.DeleteExtension) do
  begin
    try
      WriteTime := TFile.GetLastWriteTime(MyFile);

      if WriteTime < ADateTime then
      begin

        if TFile.Exists(MyFile) then
        begin
          try
            TFile.Delete(MyFile);
          except
            on E: Exception do
              PrintLog('Error on tfile.Delete, ' + MyFile);
          end;

          if FDebug then
            PrintLog('DeleteExtension file - ' + MyFile);
        end;
      end;

      Sleep(1);
    except
      on E: Exception do
      begin
        PrintLog('deleteindex, ' + MyFile + ':' + E.Message);
      end;
    end;

  end;

  Files := TDirectory.GetFiles(APath, '*', TSearchOption.soAllDirectories);
  if Length(Files) = 0 then
    TDirectory.Delete(APath, True);
end;

procedure TfmMain.CheckFile(MyFile: string; ADateTime: TDateTime);
var
  WriteTime: TDateTime;
  Way: string;
  Lane: string;
  TargetFolder: string;
  Destination: string;
  Source: string;
  SEInfo: TShellExecuteInfo;
begin
  WriteTime := TFile.GetLastWriteTime(MyFile);

  if WriteTime < ADateTime then
  begin
    try
      if MyFile.Contains(UpperCase('UP')) then
        Way := 'UP'
      else
        Way := 'DN';
      if MyFile.Contains('_1_') or MyFile.Contains('1차선') then
        Lane := '_Lane1'
      else
        Lane := '_Lane2';

      TargetFolder := Way + Lane;

      Destination := '\' + FormatDateTime('YYYY-MM', WriteTime) + '\' +
        FormatDateTime('DD', WriteTime) + '\' + TargetFolder;
      Source := MyFile;

      FillChar(SEInfo, SizeOf(SEInfo), 0);
      SEInfo.cbSize := SizeOf(TShellExecuteInfo);
      SEInfo.fMask := SEE_MASK_NOCLOSEPROCESS;
      SEInfo.lpVerb := 'OPEN';
      SEInfo.lpFile := PChar('ncftpput.exe');
      SEInfo.lpParameters := PChar(GetCommandFTP + ' ' + Destination + ' ' + Source);
      SEInfo.lpDirectory := PChar(ExtractFilePath(TGlobal.Obj.ExeName));

      if ShellExecuteExW(@SEInfo) then
        WaitForSingleObject(SEInfo.hProcess, 1000 * 60);

      if FDebug then
      begin
        PrintLog('FileInfo :  source:' + Source + ', Destination:' + Destination);
      end;

    except
      on E: Exception do
      begin
        PrintLog(Destination + ', ' + MyFile + ':' + E.Message);
      end;
    end;
  end;
end;

end.
