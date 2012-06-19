unit JdcCdmaTcp;

interface

uses System.Classes, System.SysUtils, IdGlobal, JdcGlobal, JdcCDMA,
  SerialNG;

type
  TState = class;

  TCdmaTcp = class(TCdmaAbstract)
  protected
    procedure OnTimeoutTimer(Sender: TObject);
  public
    function TCPInfo: TTCPInfo;

    constructor Create(ATCPInfo: TTCPInfo); reintroduce; overload;

    procedure OpenTCP;
    procedure CloseTCP;
    destructor Destroy; override;
  end;

  TCdmaTcpContext = class(TCdmaContextAbstract)
  strict private
    FStateNormal: IState;
    FStateTCPOpen: IState;
    FStateTCPConnect: IState;
    FStateTCPClose: IState;
  public
    TCPInfo: TTCPInfo;
    constructor Create(ATCPInfo: TTCPInfo); reintroduce; overload;

    destructor Destroy; override;
    property State: IState read FState;
    property StateNormal: IState read FStateNormal;
    property StateTCPOpen: IState read FStateTCPOpen;
    property StateTCPConnect: IState read FStateTCPConnect;
    property StateTCPClose: IState read FStateTCPClose;
  end;

  TState = class abstract(TInterfacedObject, IState)
  strict protected
    FMessage: String;   // 토큰이 잘려들어오는 경우.. 이어 붙이기 위한 변수..
    FContext: TCdmaTcpContext;
  public
    procedure Incoming(const AMessage: String); virtual;
    constructor Create(AContext: TCdmaTcpContext);
  end;

  TStateNormal = class(TState)
  public
    procedure Incoming(const AMessage: String); override;
  end;

  TStateOpen = class(TState)
  public
    procedure Incoming(const AMessage: String); override;
  end;

  TStateConnect = class(TState)
  public
    procedure Incoming(const AMessage: String); override;
  end;

  TStateClose = class(TState)
  public
    procedure Incoming(const AMessage: String); override;
  end;

implementation

{ TState }

uses View, Common;

constructor TState.Create(AContext: TCdmaTcpContext);
begin
  FContext := AContext;
  FMessage := '';
end;

procedure TState.Incoming(const AMessage: String);
begin
  TView.Obj.sp_SyncMessage('CDMALog', AMessage);
end;

{ TStateNormal }

procedure TStateNormal.Incoming(const AMessage: String);
begin
  inherited;

  if IsGoodResponse(AMessage, COMMAND_CRM251, [RESPONSE_OK]) then
  begin
    FContext.Command := COMMAND_TCPUID;
  end
  else if IsGoodResponse(AMessage, COMMAND_TCPUID, [RESPONSE_OK]) then
  begin
    FContext.Command := COMMAND_TCPPWD;
  end
  else if IsGoodResponse(AMessage, COMMAND_TCPPWD, [RESPONSE_OK]) then
  begin
    FContext.Command := COMMAND_ATD1501;
  end
  else if pos(RESPONSE_NO_DIALTONE, AMessage) > 0 then
  begin
    // NO DIALTONE
    FMessage := '';
    FContext.SetState(FContext.StateTCPOpen);
    FContext.Command := COMMAND_TCPOPEN + FContext.TCPInfo.TCPHost + ',' +
      IntToStr(FContext.TCPInfo.TCPPort);
  end
  else if pos(COMMAND_ATD1501, AMessage) > 0 then
  begin
    // ppp 연결 시도..
  end
  else if pos(RESPONSE_CONNECT, AMessage) > 0 then
  begin
    // ppp 연결 됨..
    FMessage := '';
    FContext.SetState(FContext.StateTCPOpen);
    FContext.Command := COMMAND_TCPOPEN + FContext.TCPInfo.TCPHost + ',' +
      IntToStr(FContext.TCPInfo.TCPPort);
  end
  else
  begin
    // 오류..
    FContext.SetState(FContext.StateTCPClose);
    FContext.Command := COMMAND_TCPCLOSE;
  end;

end;

{ TStateOpen }

procedure TStateOpen.Incoming(const AMessage: String);
var
  Index: Integer;
begin
  inherited;

  // RESPONSE_TCP_OPEN 이 잘려서 들어오는 경우 대비..
  FMessage := FMessage + StringReplace(AMessage, #$D, '', [rfReplaceAll]);
  FMessage := StringReplace(FMessage, #$A, '', [rfReplaceAll]);

  if pos(RESPONSE_OK, FMessage) > 0 then
  begin
    // OK 까지 Trim..
    // RESPONSE_TCP_OPEN 이 잘려서 들어오는 경우 대비..
    Index := pos(RESPONSE_OK, FMessage) + Length(RESPONSE_OK);
    FMessage := Copy(FMessage, Index, Length(FMessage) - Index + 1);

    // RESPONSE_OK 와 RESPONSE_TCP_OPEN 가 한줄에 같이 온 경우..
    if pos(RESPONSE_TCP_OPEN, FMessage) > 0 then
    begin
      // TCP Open 됨..
      FContext.SetState(FContext.StateTCPConnect);
      FContext.TCPInfo.OnTCPConnected(Self);
    end;

  end
  else if pos(RESPONSE_TCP_OPEN, FMessage) > 0 then
  begin
    // TCP Open 됨..
    FContext.SetState(FContext.StateTCPConnect);
    FContext.TCPInfo.OnTCPConnected(Self);
  end

  else
  begin
    FContext.SetState(FContext.StateTCPClose);
    FContext.Command := COMMAND_TCPCLOSE;
  end;

end;

{ TStateConnect }

procedure TStateConnect.Incoming(const AMessage: String);
begin
  inherited;

  if pos(RESPONSE_SEND_DONE, AMessage) > 0 then
  begin
    FContext.TCPInfo.OnTCPSendDone(Self);
  end
  else if pos(COMMAND_TCPCLOSE, AMessage) > 0 then
  begin
    FContext.SetState(FContext.StateTCPClose);
  end
  else if pos(RESPONSE_TCPCLOSED, AMessage) > 0 then
  begin
    FContext.SetState(FContext.StateTCPClose);
    FContext.Command := COMMAND_TCPEXIT;
  end
  else if pos(RESPONSE_ERROR, AMessage) > 0 then
  begin
    FContext.SetState(FContext.StateTCPClose);
    FContext.Command := COMMAND_TCPCLOSE;
  end
  else
  begin
    // 전송 중..
  end;

end;

{ TStateClose }

procedure TStateClose.Incoming(const AMessage: String);
begin
  inherited;

  if pos(RESPONSE_NO_CARRIER, AMessage) > 0 then
  begin
    // TCP 연결 종료 됨..
    FContext.SetState(FContext.StateNormal);
    FContext.TCPInfo.OnTCPDisconnected(Self);
  end
  else if pos(RESPONSE_TCPCLOSED, AMessage) > 0 then
  begin
    FContext.Command := COMMAND_TCPEXIT;
  end
  else if pos(RESPONSE_OK, AMessage) > 0 then
  begin
    // 종료 중..
  end
  else if pos(RESPONSE_ERROR, AMessage) > 0 then
  begin
    // TCP 연결 종료 됨..
    FContext.SetState(FContext.StateNormal);
    FContext.TCPInfo.OnTCPDisconnected(Self);
  end
  else
    FContext.Command := COMMAND_TCPEXIT;

end;

{ TCDMA }

procedure TCdmaTcp.CloseTCP;
begin
  SendCmd(COMMAND_TCPCLOSE);
end;

constructor TCdmaTcp.Create(ATCPInfo: TTCPInfo);
begin
  inherited Create;

  FContext := TCdmaTcpContext.Create(ATCPInfo);

  OnTimeout := OnTimeoutTimer;
end;

destructor TCdmaTcp.Destroy;
begin
  if FSerialNG.Active then
    FSerialNG.Active := false;

  inherited;
end;

procedure TCdmaTcp.OnTimeoutTimer(Sender: TObject);
begin

  if FTimeoutTimer.Tag = 0 then
  begin
    FTimeoutTimer.Tag := 1;
    CloseTCP;
  end
  else
  begin
    FTimeoutTimer.Tag := 0;
    Disconnect;

    TView.Obj.sp_SyncMessage('erSendFile', '');
  end;

end;

procedure TCdmaTcp.OpenTCP;
begin
  if not Connected then
    raise Exception.Create('CDMA is not active.');

  FContext.SetState(TCdmaTcpContext(FContext).StateNormal);
  SendCmd(COMMAND_CRM251);
end;

function TCdmaTcp.TCPInfo: TTCPInfo;
begin
  result := TCdmaTcpContext(FContext).TCPInfo;
end;

{ TCDMAContext }

constructor TCdmaTcpContext.Create(ATCPInfo: TTCPInfo);
begin
  TCPInfo := ATCPInfo;

  FStateNormal := TStateNormal.Create(Self);
  FStateTCPOpen := TStateOpen.Create(Self);
  FStateTCPConnect := TStateConnect.Create(Self);
  FStateTCPClose := TStateClose.Create(Self);
  FState := FStateNormal;
end;

destructor TCdmaTcpContext.Destroy;
begin
  if Assigned(FStateNormal) then
    FStateNormal := nil;

  if Assigned(FStateTCPOpen) then
    FStateTCPOpen := nil;

  if Assigned(FStateTCPConnect) then
    FStateTCPConnect := nil;

  if Assigned(FStateTCPClose) then
    FStateTCPClose := nil;

  if Assigned(TCPInfo) then
    FreeAndNil(TCPInfo);

  FState := nil;

  inherited;
end;

end.
