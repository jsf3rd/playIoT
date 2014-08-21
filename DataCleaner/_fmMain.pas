unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.AppEvnts,
  Vcl.ComCtrls, Vcl.StdCtrls, JvExStdCtrls, JvEdit, System.IOUtils,
  System.DateUtils, System.Types, JdcGlobal, System.StrUtils, System.ZLib;

type
  TfmMain = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    FDebug: boolean;
    FSubFolder: boolean;
    FBackUpFolder: string;

    procedure PrintLog(AValue: string);

    procedure CheckDir(APath: string; ADateTime: TDateTime);
    procedure CheckFile(MyFile: string; ADateTime: TDateTime);
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
      TOption.Obj.Backup := TOption.Obj.Backup;
      TOption.Obj.SubDir := TOption.Obj.SubDir;
      TOption.Obj.Debug := TOption.Obj.Debug;
      TOption.Obj.SearchPattern := TOption.Obj.SearchPattern;

      if (TOption.Obj.Year + TOption.Obj.Month + TOption.Obj.Day +
        TOption.Obj.Hour + TOption.Obj.Minute + TOption.Obj.Second) = 0 then
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
      FBackUpFolder := TOption.Obj.Backup;

      CheckDir(ExtractFilePath(TGlobal.Obj.ExeName), DateTime);
    except
      on E: Exception do
        PrintLog('Error - ' + E.Message);
    end;

  finally
    Application.Terminate;
  end;
end;

procedure TfmMain.PrintLog(AValue: string);
begin
  JdcGlobal.PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), AValue);
end;

procedure TfmMain.CheckDir(APath: string; ADateTime: TDateTime);
var
  Files: TStringDynArray;
  MyDir, MyFile: String;
  tmp: string;
begin
  if not TDirectory.Exists(APath) then
    raise Exception.Create('Not exist. ' + APath);

  if (FBackUpFolder <> '') and StartsText(FBackUpFolder, APath) then
    Exit;

  for MyDir in TDirectory.GetDirectories(APath) do
  begin
    CheckDir(MyDir, ADateTime);
  end;

  tmp := ChangeFileExt(ExtractFileName(TGlobal.Obj.ExeName), '');
  for MyFile in TDirectory.GetFiles(APath, TOption.Obj.SearchPattern) do
  begin

    if StartsText(tmp, ExtractFileName(MyFile)) then
      Continue;

    CheckFile(MyFile, ADateTime);
    Sleep(1);
  end;

  Files := TDirectory.GetFiles(APath, '*', TSearchOption.soAllDirectories);
  if Length(Files) = 0 then
    TDirectory.Delete(APath, True);
end;

procedure TfmMain.CheckFile(MyFile: string; ADateTime: TDateTime);
var
  WriteTime: TDateTime;
  BackUpPath: String;
begin
  WriteTime := TFile.GetLastWriteTime(MyFile);

  if FDebug then
    PrintLog(MyFile + '\ Time : ' + DateTimeToStr(WriteTime) + ', Check Time : '
      + DateTimeToStr(ADateTime));

  if WriteTime < ADateTime then
  begin
    try
      try
        if FBackUpFolder = '' then
          Exit;

        if FSubFolder then
          BackUpPath := FBackUpFolder + '\' + FormatDateTime('YYYYMM',
            WriteTime) + '\' + FormatDateTime('DD', WriteTime)
        else
          BackUpPath := FBackUpFolder;

        TDirectory.CreateDirectory(BackUpPath);
        BackUpPath := BackUpPath + '\' + ExtractFileName(MyFile);
        TFile.Copy(MyFile, BackUpPath, True);
      except
        on E: Exception do
        begin
          PrintLog(BackUpPath + ', ' + E.Message);
        end;
      end;

    finally
      if TFile.Exists(MyFile) then
        TFile.Delete(MyFile);
    end;
  end;
end;

end.
