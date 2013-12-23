unit JdcCdmaTcp;

interface

uses System.Classes, System.SysUtils, IdGlobal, JdcGlobal, JdcCdma,
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
    FStateError: IState;
  private

    FState: IState;
  public
    TCPInfo: TTCPInfo;
    constructor Create(ATCPInfo: TTCPInfo); reintroduce; overload;

    destructor Destroy; override;
    property State: IState read FState;
    property StateNormal: IState read FStateNormal;
    property StateTCPOpen: IState read FStateTCPOpen;
    property StateTCPConnect: IState read FStateTCPConnect;
    property StateTCPClose: IState read FStateTCPClose;
    property StateError: IState read FStateError;
  end;

  TState = class abstract(TInterfacedObject, IState)
  strict protected
    FMessage: String; // 토큰이 잘려들어오는 경우.. 이어 붙이기 위한 변수..
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

  TStateError = class(TState)
  public
    procedure Incoming(const AMessage: String); override;
  end;

implementation

{ TState }

uses JdcView2, Common;

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

  // RESPONSE_OK 이 잘려서 들어오는 경우 대비..
  FMessage := FMessage + AMessage;

  if IsGoodResponse(AMessage, COMMAND_CRM251, [RESPONSE_OK]) then
  begin
    FContext.Command := COMMAND_TCPUID;
    FMessage := '';
  end
  else if IsGoodResponse(FMessage, COMMAND_TCPUID, [RESPONSE_OK]) then
  begin
    FContext.Command := COMMAND_TCPPWD;
    FMessage := '';
  end
  else if IsGoodResponse(FMessage, COMMAND_TCPPWD, [RESPONSE_OK]) then
  begin
    FContext.Command := COMMAND_ATD1501;
    FMessage := '';
  end
  else if pos(COMMAND_TCPPWD, AMessage) + pos(COMMAND_TCPUID, AMessage) > 0 then
  begin
    // RESPONSE_OK 가 잘려서 온 경우
    FMessage := AMessage;
  end
  else if pos(RESPONSE_NO_DIALTONE, AMessage) > 0 then
  begin
    // NO DIALTONE.. 이미 ppp 연결이 되어있는 경우..
    FContext.SetState(FContext.StateTCPOpen);
    FContext.Command := COMMAND_TCPOPEN + FContext.TCPInfo.TCPHost + ',' +
      IntToStr(FContext.TCPInfo.TCPPort);
  end
  else if pos(COMMAND_ATD1501, AMessage) > 0 then
  begin
    // ppp 연결 중..
  end
  else if pos(RESPONSE_CONNECT, AMessage) > 0 then
  begin
    // ppp 연결 됨..
    FContext.SetState(FContext.StateTCPOpen);
    FContext.Command := COMMAND_TCPOPEN + FContext.TCPInfo.TCPHost + ',' +
      IntToStr(FContext.TCPInfo.TCPPort);
  end
  else
  begin
    // 오류..
    FContext.SetState(FContext.StateError);
    FContext.Command := COMMAND_TCPCLOSE;
    FMessage := '';
  end;

end;

{ TStateOpen }

procedure TStateOpen.Incoming(const AMessage: String);
var
  Index: Integer;
begin
  inherited;

  // RESPONSE_TCP_OPEN 이 잘려서 들어오는 경우 대비..
  FMessage := FMessage + AMessage;

  if Contains(FMessage, [COMMAND_TCPOPEN, RESPONSE_OK, RESPONSE_TCP_OPEN]) then
  begin
    FContext.SetState(FContext.StateTCPConnect);
    FContext.TCPInfo.OnTCPConnected(Self);
    FMessage := '';
  end

  else if Contains(AMessage, [COMMAND_TCPOPEN, RESPONSE_OK]) then
  begin
    // RESPONSE_TCP_OPEN 이 잘려서 들어오는 경우 대비..
    FMessage := StringsReplace(AMessage, [#$D, #$A], ['', '']);
  end
  else
  begin
    // 오류..
    FContext.SetState(FContext.StateError);
    FContext.Command := COMMAND_TCPCLOSE;
    FMessage := '';
  end;

end;

{ TStateConnect }

procedure TStateConnect.Incoming(const AMessage: String);
begin
  inherited;

  if pos(RESPONSE_SEND_DONE, AMessage) > 0 then
  begin
    // 전송 완료..
    FContext.TCPInfo.OnTCPSendDone(Self);
  end
  else if IsGoodResponse(AMessage, COMMAND_TCPCLOSE, [RESPONSE_OK]) then
  begin
    // 접속 해제..
    FContext.SetState(FContext.StateTCPClose);
  end
  else
  begin
    FContext.SetState(FContext.StateError);
    FContext.Command := COMMAND_TCPCLOSE;
  end

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
    // AT$TCPCLOSE / AT$TCPEXIT
  end
  else if IsGoodResponse(AMessage, COMMAND_TCPCLOSE, [RESPONSE_ERROR]) then
  begin
    FContext.Command := COMMAND_TCPEXIT;
  end
  else if IsGoodResponse(AMessage, COMMAND_TCPEXIT, [RESPONSE_ERROR]) then
  begin
    // 오류로 인한 TCP 연결 종료..
    FContext.SetState(FContext.StateNormal);
    FContext.TCPInfo.OnTCPDisconnected(nil);
  end
  else
  begin
    FContext.SetState(FContext.StateError);
    FContext.Command := COMMAND_TCPEXIT;
  end;

end;

{ TStateError }

procedure TStateError.Incoming(const AMessage: String);
begin
  inherited;

  if pos(RESPONSE_TCPCLOSED, AMessage) > 0 then
  begin
    FContext.Command := COMMAND_TCPEXIT;
  end
  else if pos(RESPONSE_OK, AMessage) > 0 then
  begin
    // 종료 중..
    // AT$TCPCLOSE / AT$TCPEXIT
  end
  else if IsGoodResponse(AMessage, COMMAND_TCPCLOSE, [RESPONSE_ERROR]) then
  begin
    FContext.Command := COMMAND_TCPEXIT;
  end
  else
  begin
    // 오류로 인한 TCP 연결 종료..
    FContext.SetState(FContext.StateNormal);
    FContext.TCPInfo.OnTCPDisconnected(nil);
  end;

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
  FStateError := TStateError.Create(Self);
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

  if Assigned(FStateError) then
    FStateError := nil;

  if Assigned(TCPInfo) then
    FreeAndNil(TCPInfo);

  FState := nil;

  inherited;
end;

end.
