unit _fmMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, JdcGlobal,
  IdGlobal, System.Threading, System.IOUtils, JdcMSeed;

type
  TfmMain = class(TForm)
    IdTCPClient: TIdTCPClient;
    Panel1: TPanel;
    mmLog: TMemo;
    edtHost: TLabeledEdit;
    edtPort: TLabeledEdit;
    edtStation: TLabeledEdit;
    btnConnect: TButton;
    btnDisconnect: TButton;
    btnHello: TButton;
    btnData: TButton;
    bntEND: TButton;
    edtNetwork: TLabeledEdit;
    edtChannel: TLabeledEdit;
    btnStation: TButton;
    btnChannel: TButton;
    btnCat: TButton;
    procedure btnConnectClick(Sender: TObject);
    procedure btnDisconnectClick(Sender: TObject);
    procedure IdTCPClientConnected(Sender: TObject);
    procedure IdTCPClientDisconnected(Sender: TObject);
    procedure btnHelloClick(Sender: TObject);
    procedure btnStationClick(Sender: TObject);
    procedure btnChannelClick(Sender: TObject);
    procedure btnDataClick(Sender: TObject);
    procedure bntENDClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCatClick(Sender: TObject);
  private
    FExePath: string;
    FThread: TThread;

    FMSeed: TMSeedFile;

    procedure SendString(value: string);
    procedure RecvString;
    procedure RecvData;
    procedure SaveToFile(const ABuffer: TIdBytes);

  public
    { Public declarations }
  end;

var
  fmMain: TfmMain;

implementation

{$R *.dfm}

procedure TfmMain.bntENDClick(Sender: TObject);
begin
  SendString('END');
  RecvString;
  FThread := TThread.CreateAnonymousThread(
    procedure
    begin
      while not TThread.CurrentThread.CheckTerminated do
        try
          RecvData;
        except
          on E: Exception do
            PrintLog(mmLog, E.Message);
        end;
    end);
  FThread.FreeOnTerminate := false;
  FThread.Start;
end;

procedure TfmMain.btnCatClick(Sender: TObject);
begin
  SendString('CAT');
  RecvString;
end;

procedure TfmMain.btnChannelClick(Sender: TObject);
begin
  SendString('SELECT ' + edtChannel.Text);
  RecvString;
end;

procedure TfmMain.btnConnectClick(Sender: TObject);
begin
  IdTCPClient.Host := edtHost.Text;
  IdTCPClient.Port := StrToIntDef(edtPort.Text, 18000);

  PrintLog(mmLog, 'Connect to SEED Link server.....');
  try
    IdTCPClient.Connect;
  except
    on E: Exception do
      PrintLog(mmLog, 'Connection failed.');
  end;

end;

procedure TfmMain.btnDataClick(Sender: TObject);
begin
  SendString('DATA');
  RecvString;
end;

procedure TfmMain.btnDisconnectClick(Sender: TObject);
var
  MyElem: String;
  FileName: string;
begin

  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
  end;

  IdTCPClient.Disconnect;

  for MyElem in FMSeed.GetChannelList do
  begin
    FileName := FExePath + MyElem + '_' + FormatDateTime('YYYYMMDD_HHMM',
      now) + '.txt';
    FMSeed.ExtractToASCii(FileName, MyElem);
    mmLog.Lines.Add('Created - ' + FileName);
  end;
  FMSeed.Clear;
end;

procedure TfmMain.btnHelloClick(Sender: TObject);
begin
  SendString('HELLO');
  RecvString;
end;

procedure TfmMain.btnStationClick(Sender: TObject);
begin
  SendString('STATION ' + edtStation.Text + ' ' + edtNetwork.Text);
  RecvString;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FThread := nil;
  FExePath := ExtractFilePath(Application.ExeName);
  FMSeed := TMSeedFile.Create;

  IdTCPClient.ConnectTimeout := 2900;
  IdTCPClient.ReadTimeout := 1000;
end;

procedure TfmMain.IdTCPClientConnected(Sender: TObject);
begin
  PrintLog(mmLog, 'Server Connected.');
end;

procedure TfmMain.IdTCPClientDisconnected(Sender: TObject);
begin
  PrintLog(mmLog, 'Server Disconnected.');
end;

procedure TfmMain.RecvData;
var
  buffer, Seq: TIdBytes;
  tmp: string;
  num: PInteger;

  SL: TIdBytes;
  Index: Integer;
begin
  SetLength(buffer, 0);
  IdTCPClient.IOHandler.ReadBytes(buffer, 520);

  tmp := BytesToString(buffer, 0, 8);
  if not tmp.Contains('SL') then
  begin
    SL := ToBytes('SL');
    Index := IdBytesPos(SL, buffer);

    if Index < 0 then
      Exit;

    PrintLog(mmLog, 'SEEDLink Log - ' + BytesToString(buffer, Index, 8));

    RemoveBytes(buffer, Index);
    IdTCPClient.IOHandler.ReadBytes(buffer, Index);
    Exit;
  end;

  tmp := BytesToString(buffer, 2, 6);
  Seq := HexStrToBytes('00' + tmp);

  num := @Seq[0];
  num^ := Rev4Bytes(num^);

  PrintLog(mmLog, 'Data seq - ' + tmp + '(' + num^.ToString + ')');

  SaveToFile(buffer);
end;

procedure TfmMain.RecvString;
begin
  if not IdTCPClient.Connected then
    Exit;

  TThread.CreateAnonymousThread(
    procedure
    var
      str: string;
    begin
      Sleep(300);
      while true do
      begin
        str := IdTCPClient.IOHandler.ReadLn;
        if str = '' then
        begin
          break;
        end;

        TThread.Synchronize(nil,
          procedure
          begin
            PrintLog(mmLog, 'RECV - ' + str);
          end);
      end;
    end).Start;
end;

procedure TfmMain.SaveToFile(const ABuffer: TIdBytes);
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    WriteTIdBytesToStream(Stream, ABuffer, 512, 8);
    FMSeed.AddStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TfmMain.SendString(value: string);
begin
  if not IdTCPClient.Connected then
  begin
    PrintLog(mmLog, 'Server Disconnected.');
    Exit;
  end;

  PrintLog(mmLog, 'SEND - ' + value);
  IdTCPClient.IOHandler.WriteLn(value, IndyTextEncoding_UTF8);
end;

end.
