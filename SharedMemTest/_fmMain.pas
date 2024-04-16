unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.DateUtils, Vcl.ExtCtrls, JdcSharedMem.Reader, JdcSharedMem.Writer,
  JdcSharedMem.Common, System.StrUtils, Vcl.Mask;

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

  TDataHeader = packed record
    FrameCount: Cardinal;
    latitude: double;
    longitude: double;
    mark: double;
    route: Array [0 .. 3] of AnsiChar;
    way: AnsiChar;
  end;

  TData = Array [0 .. 99] of Integer;

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
  Header: TDataHeader;
  Data: TData;

  Value: double;
  I: Integer;
  Stream: TStream;
  str: string;
begin
  Header.FrameCount := 1;
  Header.latitude := 37.567;
  Header.longitude := 127.890;
  Header.mark := 107.01;

  str := '0010';
  CopyMemory(@Header.route[0], PChar(str), str.Length);

  Value := FMemWriter.LastSequence;
  for I := Low(Data) to High(Data) do
  begin
    Data[I] := FMemWriter.LastSequence * (I + 1); // Random(1000) / 0.0001;
  end;

  Stream := TMemoryStream.Create;
  Stream.Write(Header, SizeOf(Header));
  Stream.Write(Data[0], SizeOf(TData));
  FMemWriter.PutData(Stream);
  Stream.Free;

  PrintLog(Memo1, 'Put Data : ' + edtCodeName.Text + ', Value :' + Value.ToString + ', Seq : ' +
    FMemWriter.LastSequence.ToString);
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
    PrintLog(Memo1, 'NoData, Seq : ' + FMemReader.GetCurrentSequence.ToString);
    exit;
  end;

  PrintData(p);
end;

procedure TForm2.btnGetInitClick(Sender: TObject);
begin
  Caption := 'Memory Mapped File Tester.';
  FMemReader := TJdcSharedMemReader.Create(edtCodeName.Text);
end;

procedure TForm2.btnGetLastClick(Sender: TObject);
var
  p: Pointer;
begin
  p := FMemReader.GetLastPointer;

  if not Assigned(p) then
  begin
    PrintLog(Memo1, 'NoData, Seq : ' + FMemReader.GetCurrentSequence.ToString);
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
    PrintLog(Memo1, 'EOF, Seq : ' + FMemReader.GetCurrentSequence.ToString);
    exit;
  end;

  PrintData(p);
end;

procedure TForm2.btnPutInitClick(Sender: TObject);
begin
  Caption := 'Memory Writer.';

  if Assigned(FMemWriter) then
    FreeAndNil(FMemWriter);

  FMemWriter := TJdcSharedMemWriter.Create(edtCodeName.Text,
    SizeOf(TDataHeader) + SizeOf(TData), dc16);
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
  Header: TDataHeader;
  Data: TData;
  pdata: Pointer;
begin
  CopyMemory(@Header, p, SizeOf(Header));
  pdata := Pointer(NativeUInt(p) + SizeOf(Header));
  CopyMemory(@Data[0], pdata, SizeOf(TData));

  PrintLog(Memo1, Format('Get Data: %s, Data[0]: %d, seq: %d', [edtCodeName.Text, Data[0],
    FMemReader.GetCurrentSequence]));
end;

procedure TForm2.GetTimerTimer(Sender: TObject);
begin
  btnGetNext.Click;
end;

end.
