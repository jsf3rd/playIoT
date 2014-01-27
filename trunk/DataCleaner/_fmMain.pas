unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.Menus, Vcl.AppEvnts,
  Vcl.ComCtrls, Vcl.StdCtrls, JvExStdCtrls, JvEdit, System.IOUtils,
  System.DateUtils, System.Types, JdcGlobal, System.StrUtils;

type
  TfmMain = class(TForm)
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    procedure PrintLog(AValue: string);
    procedure CleanData(APath: string; ADateTime: TDateTime);
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

      CleanData(ExtractFilePath(TGlobal.Obj.ExeName), DateTime);
    except
      on E: Exception do
        PrintLog(E.Message);
    end;

  finally
    Application.Terminate;
  end;
end;

procedure TfmMain.PrintLog(AValue: string);
begin
  JdcGlobal.PrintLog(ChangeFileExt(TGlobal.Obj.ExeName, '.log'), AValue);
end;

procedure TfmMain.CleanData(APath: string; ADateTime: TDateTime);
var
  Files: TStringDynArray;
  MyElem: string;
  WriteTime: TDateTime;
  BackUpFolder, BackUpPath: string;
  Dirs: TStringDynArray;
begin
  Files := TDirectory.GetFiles(APath, TOption.Obj.SearchPattern,
    TSearchOption.soAllDirectories);

  BackUpFolder := TOption.Obj.Backup;
  for MyElem in Files do
  begin
    try
      // Backup 폴더 생략..
      if StartsText(BackUpFolder, MyElem) then
        Continue;

      WriteTime := TFile.GetLastWriteTime(MyElem);

      if TOption.Obj.Debug then
        PrintLog(MyElem + '\ Time : ' + DateTimeToStr(WriteTime) +
          ', Check Time : ' + DateTimeToStr(ADateTime));

      if WriteTime < ADateTime then
      begin
        try
          try
            if TOption.Obj.Backup = '' then
              Continue;

            if TOption.Obj.SubDir then
              BackUpPath := BackUpFolder + '\' + FormatDateTime('YYYYMM',
                WriteTime) + '\' + FormatDateTime('DD', WriteTime)
            else
              BackUpPath := BackUpFolder;

            TDirectory.CreateDirectory(BackUpPath);
            BackUpPath := BackUpPath + '\' + ExtractFileName(MyElem);
            TFile.Copy(MyElem, BackUpPath, True);
          except
            on E: Exception do
            begin
              PrintLog(BackUpPath + ', ' + E.Message);
            end;
          end;

        finally
          // TimeOver 파일 삭제..
          if TFile.Exists(MyElem) then
            TFile.Delete(MyElem);
        end;
      end;

    finally
      Sleep(1);
    end;
  end;

  // 빈폴더 삭제..
  Dirs := TDirectory.GetDirectories(APath);
  for MyElem in Dirs do
  begin
    if TOption.Obj.Debug then
      PrintLog('Check Folder - ' + MyElem);

    Files := TDirectory.GetFiles(MyElem, '*', TSearchOption.soAllDirectories);

    if Length(Files) = 0 then
      TDirectory.Delete(MyElem, True);

    Sleep(1);
  end;

end;

end.
