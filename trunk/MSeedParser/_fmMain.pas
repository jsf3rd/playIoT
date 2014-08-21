unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, JvExMask,
  JvToolEdit, Vcl.ExtCtrls, IdGlobal, Math, JdcGlobal,
  System.Generics.Collections, DateUtils, JdcMSeed, JdcMSeed.Common,
  System.IOUtils;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    edtFileName: TJvFilenameEdit;
    btnMSeed2MSeed_ST1: TButton;
    btnMSeed2ASCII: TButton;
    btnASCII2MSeed_ST1: TButton;
    btnASCII2MSeed_ST2: TButton;
    btnMSeed2MSeed_ST2: TButton;
    btnTEST: TButton;
    TestTimer: TTimer;
    procedure btnMSeed2MSeed_ST1Click(Sender: TObject);
    procedure btnASCII2MSeed_ST1Click(Sender: TObject);
    procedure btnMSeed2MSeed_ST2Click(Sender: TObject);
    procedure btnASCII2MSeed_ST2Click(Sender: TObject);
    procedure btnTESTClick(Sender: TObject);
    procedure btnMSeed2ASCIIClick(Sender: TObject);
    procedure TestTimerTimer(Sender: TObject);
  private
    function ASCII2MSeed(AHeader: TMSeedHeader; AType: TSteimType): String;
    procedure Test;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnMSeed2MSeed_ST2Click(Sender: TObject);
var
  MSeedFile: TMSeedFile;
  FileName: string;
  MyElem: String;
begin
  Memo1.Lines.Add('>MSeed2MSeed st2');
  Memo1.Lines.Add('Source - ' + edtFileName.Text);
  MSeedFile := TMSeedFile.Create(edtFileName.Text);
  for MyElem in MSeedFile.GetChannelList do
  begin
    FileName := ExtractFilePath(edtFileName.Text) + MyElem + '_' +
      FormatDateTime('YYYYMMDD', now) + '.mseed';
    MSeedFile.ExtractToMSeed(FileName, MyElem, stLevel2);
    Memo1.Lines.Add('Created - ' + FileName);
  end;
  MSeedFile.Free;
  Memo1.Lines.Add('');
end;

procedure TForm1.btnMSeed2MSeed_ST1Click(Sender: TObject);
var
  MSeedFile: TMSeedFile;
  FileName: string;
  MyElem: String;
begin
  Memo1.Lines.Add('>MSeed2MSeed st1');
  Memo1.Lines.Add('Source - ' + edtFileName.Text);
  MSeedFile := TMSeedFile.Create(edtFileName.Text);
  for MyElem in MSeedFile.GetChannelList do
  begin
    FileName := ExtractFilePath(edtFileName.Text) + MyElem + '_' +
      FormatDateTime('YYYYMMDD', now) + '.mseed';
    MSeedFile.ExtractToMSeed(FileName, MyElem, stLevel1);
    Memo1.Lines.Add('Created - ' + FileName);
  end;
  MSeedFile.Free;
  Memo1.Lines.Add('');
end;

procedure TForm1.btnASCII2MSeed_ST1Click(Sender: TObject);
var
  Header: TMSeedHeader;
begin
  Header := TMSeedHeader.Create('ENB02', 'LT', 'BHZ', 'NT', 100);
  ASCII2MSeed(Header, stLevel1);
end;

procedure TForm1.btnASCII2MSeed_ST2Click(Sender: TObject);
var
  Header: TMSeedHeader;
begin
  Header := TMSeedHeader.Create('ENB01', 'LT', 'BHZ', 'NT', 100);
  ASCII2MSeed(Header, stLevel2);
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

procedure TForm1.Test;
var
  File1, File2: String;
  Header: TMSeedHeader;
begin
  TDirectory.CreateDirectory('D:\TEST');

  File1 := CreateSampleData;
  edtFileName.Text := File1;
  Header := TMSeedHeader.Create('TST01', 'MY', 'HHZ', 'UP', 100);
  File2 := ASCII2MSeed(Header, stLevel1);

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
  File2 := ASCII2MSeed(Header, stLevel2);
  edtFileName.Text := File2;
  btnMSeed2ASCII.Click;
  File2 := ExtractFilePath(edtFileName.Text) + Header.ChannelCode + '_' +
    FormatDateTime('YYYYMMDD', now) + '.txt';

  CompareFile(File1, File2);

  TDirectory.Delete('D:\TEST', True);
end;

procedure TForm1.TestTimerTimer(Sender: TObject);
begin
  Test;
  if Memo1.Lines.Count > 10000 then
    Memo1.Clear;
end;

procedure TForm1.btnTESTClick(Sender: TObject);
begin
  Test;
  Exit;

  TestTimer.Enabled := not TestTimer.Enabled;

  if TestTimer.Enabled then
    Caption := 'Testing...'
  else
    Caption := 'MSeed Parser';

end;

procedure TForm1.btnMSeed2ASCIIClick(Sender: TObject);
var
  MSeedFile: TMSeedFile;
  MyElem: String;
  FileName: string;
begin
  Memo1.Lines.Add('>MSeed2Ascii');
  Memo1.Lines.Add('Source - ' + edtFileName.Text);
  MSeedFile := TMSeedFile.Create(edtFileName.Text);
  for MyElem in MSeedFile.GetChannelList do
  begin
    FileName := ExtractFilePath(edtFileName.Text) + MyElem + '_' +
      FormatDateTime('YYYYMMDD', now) + '.txt';
    MSeedFile.ExtractToASCii(FileName, MyElem);
    Memo1.Lines.Add('Created - ' + FileName);
  end;
  MSeedFile.Free;
  Memo1.Lines.Add('');
end;

function TForm1.ASCII2MSeed(AHeader: TMSeedHeader; AType: TSteimType): String;
var
  MSeedFile: TMSeedFile;
begin
  Memo1.Lines.Add('>AsciiToMSeed ' + Integer(AType).ToString);
  Memo1.Lines.Add('Source - ' + edtFileName.Text);
  result := ChangeFileExt(edtFileName.Text, '.mseed');
  MSeedFile := TMSeedFile.Create;
  MSeedFile.AsciiToMSeed(edtFileName.Text, AHeader, AType);
  MSeedFile.Free;
  Memo1.Lines.Add('Created - ' + result);
  Memo1.Lines.Add('');

end;

end.
