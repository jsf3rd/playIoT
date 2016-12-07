unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.DateUtils, Vcl.ExtCtrls, JdcSharedMem.Reader, JdcSharedMem.Writer,
  JdcSharedMem.Common, System.StrUtils;

type
  TForm2 = class(TForm)
    Memo1: TMemo;
    Panel1: TPanel;
    GetTimer: TTimer;
    PutTimer: TTimer;
    edtCodeName: TLabeledEdit;
    GroupBox1: TGroupBox;
    btnPutInit: TButton;
    btnPut: TButton;
    btnAutoPut: TButton;
    GroupBox2: TGroupBox;
    btnGetInit: TButton;
    btnGetFirst: TButton;
    btnAutoGet: TButton;
    btnGetNext: TButton;
    btnGetLast: TButton;
    edtPutInterval: TEdit;
    edtGetInterval: TEdit;
    btnPutFinal: TButton;
    btnGetFinal: TButton;
    procedure btnPutClick(Sender: TObject);
    procedure btnGetFirstClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPutInitClick(Sender: TObject);
    procedure btnAutoPutClick(Sender: TObject);
    procedure PutTimerTimer(Sender: TObject);
    procedure GetTimerTimer(Sender: TObject);
    procedure btnAutoGetClick(Sender: TObject);
    procedure btnGetInitClick(Sender: TObject);
    procedure btnGetNextClick(Sender: TObject);
    procedure btnGetLastClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnPutFinalClick(Sender: TObject);
    procedure btnGetFinalClick(Sender: TObject);
  private
    FMemWriter: TJdcSharedMemWriter;
    FMemReader: TJdcSharedMemReader;
    procedure PrintData(p: Pointer);
  public
    { Public declarations }
  end;

  TDataList = array [0 .. 99] of double;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure PrintLog(AMemo: TMemo; const AMsg: String);
begin
  if AMemo.Lines.Count > 5000 then
    AMemo.Lines.Clear;

  if AMsg = '' then
    AMemo.Lines.Add('')
  else
    AMemo.Lines.Add(FormatDateTime('YYYY-MM-DD, HH:NN:SS.zzz, ', now) + AMsg);
end;

procedure TForm2.btnPutClick(Sender: TObject);
var
  DataList: TDataList;
  I: Integer;
  Sum: double;
  Stream: TStream;
begin
  Sum := 0;
  for I := Low(DataList) to High(DataList) do
  begin
    DataList[I] := FMemWriter.Sequence * 10; // Random(1000) / 0.0001;
    Sum := Sum + DataList[I];
  end;

  Sum := Sum / Length(DataList);

  Stream := TMemoryStream.Create;
  Stream.WriteBuffer(DataList, SizeOf(DataList));
  FMemWriter.PutData(Stream);
  Stream.Free;

  PrintLog(Memo1, 'Put Data : ' + edtCodeName.Text + ', Avg :' + Sum.ToString +
    ', Seq : ' + FMemWriter.Sequence.ToString);
end;

procedure TForm2.btnGetFinalClick(Sender: TObject);
begin
  if Assigned(FMemReader) then
    FreeAndNil(FMemReader);
end;

procedure TForm2.btnGetFirstClick(Sender: TObject);
var
  p: Pointer;
begin
  p := FMemReader.GetFirstPointer;

  if not Assigned(p) then
  begin
    PrintLog(Memo1, 'NoData, Seq : ' + FMemReader.Sequence.ToString);
    exit;
  end;

  PrintData(p);
end;

procedure TForm2.btnGetInitClick(Sender: TObject);
begin
  Caption := 'Memory Reader.';
  FMemReader := TJdcSharedMemReader.Create(edtCodeName.Text);
end;

procedure TForm2.btnGetLastClick(Sender: TObject);
var
  p: Pointer;
begin
  p := FMemReader.GetLastPointer;

  if not Assigned(p) then
  begin
    PrintLog(Memo1, 'NoData, Seq : ' + FMemReader.Sequence.ToString);
    exit;
  end;

  PrintData(p);
end;

procedure TForm2.btnGetNextClick(Sender: TObject);
var
  p: Pointer;
begin

  p := FMemReader.GetNextPointer;

  if not Assigned(p) then
  begin
    PrintLog(Memo1, 'EOF, Seq : ' + FMemReader.Sequence.ToString);
    exit;
  end;

  PrintData(p);
end;

procedure TForm2.btnPutInitClick(Sender: TObject);
begin
  Caption := 'Memory Writer.';

  if Assigned(FMemWriter) then
    FreeAndNil(FMemWriter);

  FMemWriter := TJdcSharedMemWriter.Create(edtCodeName.Text, SizeOf(TDataList));
end;

procedure TForm2.btnAutoPutClick(Sender: TObject);
begin
  PutTimer.Interval := StrToInt(edtPutInterval.Text);
  PutTimer.Enabled := not PutTimer.Enabled;

  if PutTimer.Enabled then
    btnPut.Click;
end;

procedure TForm2.btnPutFinalClick(Sender: TObject);
begin
  if Assigned(FMemWriter) then
    FreeAndNil(FMemWriter);
end;

procedure TForm2.btnAutoGetClick(Sender: TObject);
begin
  GetTimer.Interval := StrToInt(edtGetInterval.Text);
  GetTimer.Enabled := not GetTimer.Enabled;

  if GetTimer.Enabled then
  begin
    btnGetNext.Click;
  end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FMemWriter := nil;
  FMemReader := nil;
  Randomize;
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  if Assigned(FMemWriter) then
    FreeAndNil(FMemWriter);

  if Assigned(FMemReader) then
    FreeAndNil(FMemReader);

end;

procedure TForm2.PutTimerTimer(Sender: TObject);
begin
  btnPut.Click;
end;

procedure TForm2.PrintData(p: Pointer);
var
  DataList: TDataList;
  Sum: double;
  I: Integer;
begin
  CopyMemory(@DataList, p, SizeOf(DataList));

  Sum := 0;
  for I := Low(DataList) to High(DataList) do
  begin
    Sum := Sum + DataList[I];
  end;

  Sum := Sum / Length(DataList);
  PrintLog(Memo1, 'Get Data : ' + edtCodeName.Text + ', Avg :' + Sum.ToString);

end;

procedure TForm2.GetTimerTimer(Sender: TObject);
begin
  btnGetNext.Click;
end;

end.
