unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvToolEdit, Vcl.ExtCtrls, IdGlobal, Math, JdcGlobal,
  System.Generics.Collections, DateUtils, JdcMSeed, JdcMSeed.Common,
  System.IOUtils, _fmHeaderInfo;

type
  TfmMain = class(TForm)
    mmLog: TMemo;
    Panel1: TPanel;
    edtFileName: TJvFilenameEdit;
    btnMSeed2MSeed_ST1: TButton;
    btnMSeed2ASCII: TButton;
    btnASCII2MSeed_ST1: TButton;
    btnASCII2MSeed_ST2: TButton;
    btnMSeed2MSeed_ST2: TButton;
    edtBegin: TEdit;
    edtEnd: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure btnMSeed2MSeed_ST1Click(Sender: TObject);
    procedure btnASCII2MSeed_ST1Click(Sender: TObject);
    procedure btnMSeed2MSeed_ST2Click(Sender: TObject);
    procedure btnASCII2MSeed_ST2Click(Sender: TObject);
    procedure btnMSeed2ASCIIClick(Sender: TObject);
    procedure TestTimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMSeedHeader: TMSeedHeader;
    procedure ASCII2MSeed(AHeader: TMSeedHeader; AType: TSteimType);
    procedure Test;
    procedure MSeed2MSeed(AType: TSteimType);
  public
    property MSeedHeader: TMSeedHeader read FMSeedHeader write FMSeedHeader;
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.btnMSeed2MSeed_ST2Click(Sender: TObject);
begin
  mmLog.Lines.Add('>MSeed2MSeed steim2');
  MSeed2MSeed(stLevel2);
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  edtBegin.Text := FormatDateTime('YYYY-MM-DD HH:NN:SS', MinDateTime);
  edtEnd.Text := FormatDateTime('YYYY-MM-DD HH:NN:SS', MaxDateTime);
end;

procedure TfmMain.btnMSeed2MSeed_ST1Click(Sender: TObject);
begin
  mmLog.Lines.Add('>MSeed2MSeed steim1');
  MSeed2MSeed(stLevel1);
end;

procedure TfmMain.MSeed2MSeed(AType: TSteimType);
var
  MSeedFile: TMSeedFile;
  FileName: string;
  MyElem: String;
  BeginTime: TDateTime;
  EndTime: TDateTime;
begin
  mmLog.Lines.Add('Source - ' + edtFileName.Text);
  MSeedFile := TMSeedFile.Create(edtFileName.Text);

  BeginTime := StrToDateTime(edtBegin.Text);
  EndTime := StrToDateTime(edtEnd.Text);

  for MyElem in MSeedFile.GetChannelList do
  begin
    FileName := ExtractFilePath(edtFileName.Text) + MyElem + '_' +
      FormatDateTime('YYYYMMDD', now) + '.mseed';
    MSeedFile.ExtractToMSeed(FileName, MyElem, AType, BeginTime, EndTime);
    mmLog.Lines.Add('Created - ' + FileName);
  end;
  MSeedFile.Free;
  mmLog.Lines.Add('----------------------------------------------------');
end;

procedure TfmMain.btnASCII2MSeed_ST1Click(Sender: TObject);
begin
  if fmHeaderInfo.ShowModal = mrOk then
    ASCII2MSeed(FMSeedHeader, stLevel1);
end;

procedure TfmMain.btnASCII2MSeed_ST2Click(Sender: TObject);
begin
  if fmHeaderInfo.ShowModal = mrOk then
    ASCII2MSeed(FMSeedHeader, stLevel2);
end;

function CreateSampleData: String;
var
  I: Integer;
  DateTime: TDateTime;
  SmapleCount: Integer;
  Sample: TStreamWriter;
begin
  result := 'D:\TEST\Sample1.txt';

  TFile.Create(result).Free;
  Sample := TFile.AppendText(result);
  try
    DateTime := now;

    SmapleCount := 10000;

    for I := 0 to SmapleCount do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(8).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;

    for I := 0 to SmapleCount do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(32).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;

    for I := 0 to SmapleCount do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(128).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;

    for I := 0 to SmapleCount do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(256).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;

    for I := 0 to SmapleCount * 2 do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(500000000).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;

    for I := 0 to SmapleCount do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(65535).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;

    for I := 0 to SmapleCount do
    begin
      Sample.WriteLine(FormatDateTime('YYYY-MM-DD HH:NN:SS.zzz', DateTime) + ' '
        + Random(16).ToString);
      DateTime := IncMilliSecond(DateTime, 10);
    end;
  finally
    Sample.Free;
  end;
end;

procedure CompareFile(AFile1, AFile2: String);
var
  File1, File2: TStreamReader;
  tmp1, tmp2: string;
begin

  File1 := TFile.OpenText(AFile1);
  File2 := TFile.OpenText(AFile2);
  try
    while True do
    begin
      tmp1 := File1.ReadLine;
      tmp2 := File2.ReadLine;

      if tmp1 <> tmp2 then
        raise Exception.Create('Test Fail');

      if tmp1 = '' then
        break;
    end;
  finally
    File1.Free;
    File2.Free;
  end;
end;

procedure TfmMain.Test;
var
  File1, File2: String;
  Header: TMSeedHeader;
begin
  TDirectory.CreateDirectory('D:\TEST');

  File1 := CreateSampleData;
  edtFileName.Text := File1;
  Header := TMSeedHeader.Create('TST01', 'MY', 'HHZ', 'UP', 100);
  ASCII2MSeed(Header, stLevel1);
  File2 := ExtractFilePath(edtFileName.Text) + Header.ChannelCode + '.mseed';

  edtFileName.Text := File2;
  btnMSeed2MSeed_ST2.Click;

  edtFileName.Text := ExtractFilePath(edtFileName.Text) + Header.ChannelCode +
    '_' + FormatDateTime('YYYYMMDD', now) + '.mseed';

  btnMSeed2MSeed_ST1.Click;
  btnMSeed2MSeed_ST2.Click;

  btnMSeed2ASCII.Click;
  File2 := ExtractFilePath(edtFileName.Text) + Header.ChannelCode + '_' +
    FormatDateTime('YYYYMMDD', now) + '.txt';
  CompareFile(File1, File2);

  edtFileName.Text := File2;
  ASCII2MSeed(Header, stLevel2);
  File2 := ExtractFilePath(edtFileName.Text) + Header.ChannelCode + '.mseed';
  edtFileName.Text := File2;
  btnMSeed2ASCII.Click;
  File2 := ExtractFilePath(edtFileName.Text) + Header.ChannelCode + '_' +
    FormatDateTime('YYYYMMDD', now) + '.txt';

  CompareFile(File1, File2);

  TDirectory.Delete('D:\TEST', True);
end;

procedure TfmMain.TestTimerTimer(Sender: TObject);
begin
  Test;
  if mmLog.Lines.Count > 10000 then
    mmLog.Clear;
end;

procedure TfmMain.btnMSeed2ASCIIClick(Sender: TObject);
var
  MSeedFile: TMSeedFile;
  MyElem: String;
  FileName: string;
  BeginTime: TDateTime;
  EndTime: TDateTime;
begin
  mmLog.Lines.Add('>MSeed2Ascii');
  mmLog.Lines.Add('Source - ' + edtFileName.Text);
  MSeedFile := TMSeedFile.Create(edtFileName.Text);

  BeginTime := StrToDateTime(edtBegin.Text);
  EndTime := StrToDateTime(edtEnd.Text);

  for MyElem in MSeedFile.GetChannelList do
  begin
    FileName := ExtractFilePath(edtFileName.Text) + MyElem + '_' +
      FormatDateTime('YYYYMMDD', now) + '.txt';

    try
      MSeedFile.ExtractToASCii(FileName, MyElem, BeginTime, EndTime);
    except
      on E: Exception do
      begin
        mmLog.Lines.Add('Error - ' + FileName);
        mmLog.Lines.Add(E.Message);
        Continue;
      end;
    end;

    mmLog.Lines.Add('Created - ' + FileName);
    Application.ProcessMessages;
  end;
  MSeedFile.Free;
  mmLog.Lines.Add('----------------------------------------------------');
end;

procedure TfmMain.ASCII2MSeed(AHeader: TMSeedHeader; AType: TSteimType);
var
  MSeedFile: TMSeedFile;
begin
  case AType of
    stLevel1:
      mmLog.Lines.Add('>AsciiToMSeed Steim1');
    stLevel2:
      mmLog.Lines.Add('>AsciiToMSeed Steim2');
  else
    raise Exception.Create('Unknown SteimType, ' + Integer(AType).ToString);
  end;

  mmLog.Lines.Add('Source - ' + edtFileName.Text);
  MSeedFile := TMSeedFile.Create;
  MSeedFile.AsciiToMSeed(edtFileName.Text, AHeader, AType);
  MSeedFile.Free;
  mmLog.Lines.Add('Created - ' + ExtractFilePath(edtFileName.Text) +
    AHeader.ChannelCode + '.mseed');
  mmLog.Lines.Add('----------------------------------------------------');
end;

end.
