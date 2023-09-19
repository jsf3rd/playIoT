unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, JdcGlobal, JdcLogging,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Mask;

type
  TForm1 = class(TForm)
    GroupBox2: TGroupBox;
    btnGetInit: TButton;
    btnGetFirst: TButton;
    btnAutoGet: TButton;
    btnGetNext: TButton;
    btnGetLast: TButton;
    edtGetInterval: TEdit;
    btnGetFinal: TButton;
    edtCodeName: TLabeledEdit;
    Memo1: TMemo;
    procedure btnGetInitClick(Sender: TObject);
    procedure btnGetFirstClick(Sender: TObject);
    procedure btnGetNextClick(Sender: TObject);
    procedure btnGetLastClick(Sender: TObject);
  private
    FMemReader: Pointer;
    procedure PrintData(p: Pointer);
  public
    { Public declarations }
  end;

  TItemDataFormatHeader = packed record
    Version: Byte;
    SensorItemID: Word;
    SampleCount: Byte;
    TimeStamp: Double;
    DataLength: Cardinal;
  end;

  TData = TArray<Double>;

var
  Form1: TForm1;

implementation

const
  MMFReader_DLL = 'MMFReader.DLL';

function OpenMMF(AValue: PAnsiChar): Pointer; stdcall; external MMFReader_DLL;
procedure CloseMMF(AValue: PAnsiChar); stdcall; external MMFReader_DLL;
function GetFirst(AValue: Pointer): Pointer; stdcall; external MMFReader_DLL;
function GetNext(AValue: Pointer): Pointer; stdcall; external MMFReader_DLL;
function GetLast(AValue: Pointer): Pointer; stdcall; external MMFReader_DLL;

{$R *.dfm}

procedure TForm1.btnGetFirstClick(Sender: TObject);
var
  tmp: Pointer;
begin
  tmp := GetFirst(FMemReader);
  PrintData(tmp);
end;

procedure TForm1.btnGetInitClick(Sender: TObject);
var
  str: PAnsiChar;
begin
  str := PAnsiChar(AnsiString(edtCodeName.Text));
  FMemReader := OpenMMF(str);
end;

procedure TForm1.btnGetLastClick(Sender: TObject);
var
  tmp: Pointer;
begin
  tmp := GetLast(FMemReader);

  if not Assigned(tmp) then
  begin
    PrintLog(Memo1, 'NoData');
    exit;
  end;

  PrintData(tmp);
end;

procedure TForm1.btnGetNextClick(Sender: TObject);
var
  tmp: Pointer;
begin
  tmp := GetNext(FMemReader);

  if not Assigned(tmp) then
  begin
    PrintLog(Memo1, 'EOF');
    exit;
  end;

  PrintData(tmp);
end;

procedure TForm1.PrintData(p: Pointer);
var
  Header: TItemDataFormatHeader;
  Data: TData;

  pdata: Pointer;

  Sum: Double;
  I: Integer;
begin
  CopyMemory(@Header, p, SizeOf(Header));
  pdata := Pointer(NativeUInt(p) + SizeOf(Header));
  SetLength(Data, Header.SampleCount);
  CopyMemory(@Data[0], pdata, Header.DataLength);

  PrintLog(Memo1, 'Get Data : ' + edtCodeName.Text + ', Data[0] :' + Data[0].ToString);
end;

{ TItemDataFormatHeader }

end.
