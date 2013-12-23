unit JdcCdma;

interface

uses Classes, SysUtils, SerialNG, IdGlobal, Vcl.ExtCtrls;

const
  // M2M COMMAND
  COMMAND_CRM129 = 'at+crm=129';
  COMMAND_ATDT = 'atdt';
  COMMAND_CLEAR = 'clear';
  COMMAND_LOAD = 'load';
  COMMAND_MEASURE_TIME = 'int';
  COMMAND_DATE_TIME = 'time';
  COMMAND_SMS = 'at$smsmo=';
  COMMAND_PHONENUM = 'at$phonenum?';

  // TCPIP COMMAND
  COMMAND_CRM251 = 'at+crm=251';
  COMMAND_TCPUID = 'at$tcpuid=sktelecom';
  COMMAND_TCPPWD = 'at$tcppasswdnull';
  COMMAND_ATD1501 = 'atd1501';

  COMMAND_TCPOPEN = 'AT$TCPOPEN=';
  COMMAND_WRITE = 'AT$TCPWRITE=';
  COMMAND_TCPCLOSE = 'AT$TCPCLOSE';
  COMMAND_TCPEXIT = 'AT$TCPEXIT';

  // RESPONSE
  RESPONSE_OK = 'OK';
  RESPONSE_CONNECT = 'CONNECT';
  RESPONSE_TCP_OPEN = 'TCPOPEN' + #$D#$A;
  RESPONSE_SEND_DONE = 'TCPSENDDONE';
  RESPONSE_TCPCLOSED = 'TCPCLOSED';
  RESPONSE_NO_CARRIER = 'CARRIER'; // NO CARRIER
  RESPONSE_ERROR = 'ERROR';
  RESPONSE_NO_DIALTONE = 'DIALTONE'; // NO DIALTONE

type
  IState = Interface
    ['{057392FC-923D-4473-8947-2D5601A59B3A}']
    procedure Incoming(const AMessage: String);
  End;

  ICdmaContext = Interface
    ['{5D98B985-56B3-4A53-B670-6335742E2161}']
    procedure SetState(AState: IState);
    procedure SetCommand(const ACommand: String);
    function GetCommand: String;
    procedure Incoming(const AMessage: String);

    property Command: String read GetCommand write SetCommand;
  End;

  ICDMA = Interface
    ['{67B7B40E-9C31-4681-9E65-6E3581E87D73}']
    procedure Connect;
    procedure Disconnect;

    function GetConnected: Boolean;

    procedure SendCmd(ACommand: string);
    procedure SendSMS(ToPhone, FromPhone, AMessage: string);

    procedure SetCommand(const ACommand: String);

    property Connected: Boolean read GetConnected;
  end;

  TCdmaContextAbstract = class abstract(TInterfacedObject, ICdmaContext)
  protected
    FState: IState;
    FCommand: String;
  private
    function GetCommand: String;
    procedure SetCommand(const ACommand: String);
  public
    destructor Destroy; override;
    procedure SetState(AState: IState);
    procedure Incoming(const AMessage: String);
    property State: IState read FState;

    property Command: String read GetCommand write SetCommand;
  end;

  TCdmaAbstract = class(TInterfacedObject, ICDMA)
  protected
    FTimeoutTimer: TTimer;
    FContext: TCdmaContextAbstract;

    FSerialNG: TSerialPortNG;

    procedure SendCmd(ACommand: string);
    procedure OnTimeoutTimer(Sender: TObject);
  private
    FOnTimeout: TNotifyEvent;
    function GetConnected: Boolean;
    function BuildSMS(const ToPhone, FromPhone, AMessage: string): String;

    procedure SerialRxClusterEvent(Sender: TObject);
    procedure SetOnTimeout(const Value: TNotifyEvent);
    procedure SetTimeout(const Value: Integer);
    function GetTimeout: Integer;
    function GetBaudRate: Integer;
    function GetCommPort: String;
    procedure SetBaudRate(const Value: Integer);
    procedure SetCommPort(const Value: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetCommand(const ACommand: String);
    procedure SendSMS(ToPhone, FromPhone, AMessage: string);

    procedure Connect; virtual;
    procedure Disconnect; virtual;

    property Connected: Boolean read GetConnected;
    property CommPort: String read GetCommPort write SetCommPort;
    property BaudRate: Integer read GetBaudRate write SetBaudRate;
    property OnTimeout: TNotifyEvent read FOnTimeout write SetOnTimeout;
    property Timeout: Integer read GetTimeout write SetTimeout;
  end;

  TTCPInfo = class
  protected
    FTCPHost: String;
    FTCPPort: Integer;

    FOnTCPDisconnected: TNotifyEvent;
    FOnTCPConnected: TNotifyEvent;
    FOnTCPSendDone: TNotifyEvent;
  private
    procedure SetTCPHost(const AHost: String);
    function GetTCPHost: String;
    procedure SetTCPPort(const APort: Integer);
    function GetTCPPort: Integer;

    function GetOnTCPDisconnected: TNotifyEvent;
    function GetOnTCPConnected: TNotifyEvent;
    function GetOnTCPSendDone: TNotifyEvent;

    procedure SetOnTCPDisconnected(const Value: TNotifyEvent);
    procedure SetOnTCPConnected(const Value: TNotifyEvent);
    procedure SetOnTCPSendDone(const Value: TNotifyEvent);
  public
    property TCPHost: String read GetTCPHost write SetTCPHost;
    property TCPPort: Integer read GetTCPPort write SetTCPPort;

    property OnTCPDisconnected: TNotifyEvent read GetOnTCPDisconnected
      write SetOnTCPDisconnected;
    property OnTCPConnected: TNotifyEvent read GetOnTCPConnected
      write SetOnTCPConnected;
    property OnTCPSendDone: TNotifyEvent read GetOnTCPSendDone
      write SetOnTCPSendDone;
  end;

implementation

{ TCDMABase }

procedure TCdmaAbstract.Connect;
begin
  FSerialNG.Active := true;
end;

constructor TCdmaAbstract.Create;
begin
  FSerialNG := TSerialPortNG.Create(nil);
  FSerialNG.OnRxClusterEvent := SerialRxClusterEvent;

  // CDMA로 부터 1분간 응답이 없으면 연결을 종료한다.
  FTimeoutTimer := TTimer.Create(nil);
  FTimeoutTimer.Interval := 60000; // 기본 1분..
  FTimeoutTimer.OnTimer := OnTimeoutTimer;
  FTimeoutTimer.Enabled := false;
  FTimeoutTimer.Tag := 0;
end;

destructor TCdmaAbstract.Destroy;
begin
  if Assigned(FSerialNG) then
  begin
    FreeAndNil(FSerialNG);
  end;

  if Assigned(FContext) then
    FreeAndNil(FContext);

  FTimeoutTimer.Free;
  inherited;
end;

procedure TCdmaAbstract.Disconnect;
begin
  FTimeoutTimer.Tag := 0;
  FSerialNG.Active := false;
  FTimeoutTimer.Enabled := false;
end;

function TCdmaAbstract.GetBaudRate: Integer;
begin
  result := FSerialNG.BaudRate;
end;

function TCdmaAbstract.GetCommPort: String;
begin
  result := String(FSerialNG.CommPort);
end;

function TCdmaAbstract.GetConnected: Boolean;
begin
  result := FSerialNG.Active;
end;

function TCdmaAbstract.GetTimeout: Integer;
begin
  result := FTimeoutTimer.Interval;
end;

procedure TCdmaAbstract.OnTimeoutTimer(Sender: TObject);
begin
  if Assigned(OnTimeout) then
    OnTimeout(Sender);
end;

procedure TCdmaAbstract.SendCmd(ACommand: string);
begin
  if FSerialNG.Active then
  begin
    FSerialNG.ClearRxDQueue; // 버퍼 비우기
    FSerialNG.SendString(AnsiString(ACommand) + #13);
  end;
  // else
  // Assert(false, 'CDMA is not active.');
end;

function TCdmaAbstract.BuildSMS(const ToPhone, FromPhone,
  AMessage: string): String;
var
  SMS: TStringList;
begin
  SMS := TStringList.Create;
  try
    SMS.Add(COMMAND_SMS + ToPhone); // CMD + 수신 전화번호
    SMS.Add(FromPhone); // 송신 전화번호
    SMS.Add('4098'); // Teleservice ID, 4098 : 메시지 텔레서비스
    SMS.Add('16'); // message encode - 0: octet(8bits),
    SMS.Add('0'); // reply - 기지국의 응답 메시지 요구(0: not
    SMS.Add('0'); // priority - 0: normal
    SMS.Add(ToHex(ToBytes(AMessage, IndyTextEncoding_Default)));
    // Message Hex code
    result := SMS.CommaText;
  finally
    SMS.Free;
  end;
end;

procedure TCdmaAbstract.SendSMS(ToPhone, FromPhone, AMessage: string);
begin
  SendCmd(BuildSMS(ToPhone, FromPhone, AMessage));
end;

procedure TCdmaAbstract.SetBaudRate(const Value: Integer);
begin
  FSerialNG.BaudRate := Value;
end;

procedure TCdmaAbstract.SetCommand(const ACommand: String);
begin
  FContext.Command := ACommand;
end;

procedure TCdmaAbstract.SetCommPort(const Value: String);
begin
  FSerialNG.CommPort := ShortString(Value);
end;

procedure TCdmaAbstract.SetOnTimeout(const Value: TNotifyEvent);
begin
  FOnTimeout := Value;
end;

procedure TCdmaAbstract.SetTimeout(const Value: Integer);
begin
  FTimeoutTimer.Interval := Value;
end;

procedure TCdmaAbstract.SerialRxClusterEvent(Sender: TObject);
var
  cmd: String;
begin

  FTimeoutTimer.Enabled := false;
  FTimeoutTimer.Enabled := true;

  if FSerialNG.NextClusterSize >= 0 then
  begin
    if FSerialNG.NextClusterCCError <> 0 then
    begin
      raise Exception.Create('NextClusterCCError ' +
        IntToStr(FSerialNG.NextClusterCCError));
      exit;
    end;

    if Assigned(FContext) then
      FContext.Incoming(String(FSerialNG.ReadNextClusterAsString))
    else
    begin
      FSerialNG.Active := false;
      exit;
    end;

  end;

  cmd := FContext.Command;
  if cmd <> '' then
    SendCmd(cmd);

end;

{ TCDMAContextBase }

destructor TCdmaContextAbstract.Destroy;
begin
  FState := nil;
  inherited;
end;

function TCdmaContextAbstract.GetCommand: String;
begin
  result := FCommand;
  FCommand := '';
end;

procedure TCdmaContextAbstract.Incoming(const AMessage: String);
begin
  FState.Incoming(AMessage);
end;

procedure TCdmaContextAbstract.SetCommand(const ACommand: String);
begin
  FCommand := ACommand;
end;

procedure TCdmaContextAbstract.SetState(AState: IState);
begin
  FState := AState;
end;

{ TCDMATCPInfo }
function TTCPInfo.GetOnTCPConnected: TNotifyEvent;
begin
  result := FOnTCPConnected;
end;

function TTCPInfo.GetOnTCPDisconnected: TNotifyEvent;
begin
  result := FOnTCPDisconnected;
end;

function TTCPInfo.GetOnTCPSendDone: TNotifyEvent;
begin
  result := FOnTCPSendDone;
end;

function TTCPInfo.GetTCPHost: String;
begin
  result := FTCPHost;
end;

function TTCPInfo.GetTCPPort: Integer;
begin
  result := FTCPPort;
end;

procedure TTCPInfo.SetOnTCPConnected(const Value: TNotifyEvent);
begin
  FOnTCPConnected := Value;
end;

procedure TTCPInfo.SetOnTCPDisconnected(const Value: TNotifyEvent);
begin
  FOnTCPDisconnected := Value;
end;

procedure TTCPInfo.SetOnTCPSendDone(const Value: TNotifyEvent);
begin
  FOnTCPSendDone := Value;
end;

procedure TTCPInfo.SetTCPHost(const AHost: String);
begin
  FTCPHost := AHost;
end;

procedure TTCPInfo.SetTCPPort(const APort: Integer);
begin
  FTCPPort := APort;
end;

end.
