{ ******************************************************* }
{ }
{ CR1000 데이터 수집 }
{ }
{ Copyright (C) 2008 (주)에이티맥스, isul }
{ }
{ ******************************************************* }

unit cr1000;

interface

uses SysUtils, StrUtils, Windows, SPBDLL;

const
  LOGGER_CR1000 = 3;

  RECORD_RECENT = -1;
  RECORD_ALL = 0;

  FN_GET_TABLENAMES = 0;

  RESULT_CODE_OK = 0;
  RESULT_CODE_OK_EX = 1;
  RESULT_CODE_TIME_OUT = -1;
  RESULT_CODE_PORT_CLOSED = -2;

  RESULT_MSG_OK_EX = 'Success but more data to collect.';
  RESULT_MSG_TIME_OUT = 'Communication timed out.';
  RESULT_MSG_PORT_CLOSED = 'Port is not open.';

type
  TCR1000 = class
  private
    Address: Integer;
    procedure SetLastError(ReturnCode: Integer; FuncType: Integer = -1);
    procedure Debug(const str: string); overload;
    procedure Debug(const Format: string; const Args: array of const); overload;
  public
    LastError: string;
    PortOpened: Boolean;
    IpPortOpened: Boolean;
    constructor Create;
    destructor Destroy; override;

    function OpenPort(ComPort, BaudRate: Integer): Boolean; overload;
    function OpenPort(Ip: string; Port: Integer): Boolean; overload;
    function ClosePort: Boolean;
    function GetClock(var ATime: PAnsiChar): Boolean;
    function SetClock(var ATime: PAnsiChar): Boolean;
    function GetData(Table, Recrd: Integer; out pData: PAnsiChar): Integer;
    function GetCommaData(Table, Recrd: Integer; out pData: PAnsiChar): Integer;
    function GetStatus: string;
    function GetTableNames(out Tables: PAnsiChar): Boolean;
    function UpdateFirmware(FileName: PAnsiChar; out pData: PAnsiChar): Integer;
    function Opened: Boolean;
  end;

implementation

{ TCR1000 }

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.Create
  작성자: isul
  작성일: 2008.02.11
  인  자: None
  결과값: None
  설  명: 생성자
  ------------------------------------------------------------------------------- }
constructor TCR1000.Create;
begin
  inherited;

  PortOpened := False;
  IpPortOpened := False;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.Destroy
  작성자: isul
  작성일: 2008.02.11
  인  자: None
  결과값: None
  설  명: 해제
  ------------------------------------------------------------------------------- }
destructor TCR1000.Destroy;
begin
  ClosePort;

  inherited;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.OpenPort
  작성자: isul
  작성일: 2008.02.11
  인  자: ComPort, BaudRate: Integer
  결과값: Boolean
  설  명: 시리얼 포트 열기
  ------------------------------------------------------------------------------- }
function TCR1000.OpenPort(ComPort, BaudRate: Integer): Boolean;
var
  Data: PAnsiChar;
  str: string;
  Len: Integer;
  Opened: Boolean;
begin
  Result := False;
  Opened := SPBDLL.OpenPort(ComPort, BaudRate) = 0;
  Debug('OpenPort() -> %s', [IfThen(Opened, '성공', '실패')]);
  if Opened then
  begin
    Result := GetAddress(LOGGER_CR1000, Data, Len) = 0;
    Debug('GetAddress(ComPort=%d, BaudRate=%d) -> %s',
      [ComPort, BaudRate, IfThen(Result, '성공', '실패')]);
    if Result then
    begin
      str := string(Data);
      Address := StrToIntDef(Copy(str, Pos('=', str) + 1, Length(str)), 1);
      Debug('Address -> %d', [Address]);
      PortOpened := True;
    end
    else if Opened then
    begin
      PortOpened := True;
      ClosePort;
    end;
  end;
end;

function TCR1000.OpenPort(Ip: string; Port: Integer): Boolean;
var
  Data: PAnsiChar;
  Len: Integer;
  Opened: Boolean;
  rlt: Integer;
begin
  Result := False;
  Opened := SPBDLL.OpenIPPort(PAnsiChar(Ip), Port) = 0;
  Debug('OpenIPPort(Ip=%s, Port=%d) -> %s',
    [Ip, Port, IfThen(Opened, '성공', '실패')]);
  if Opened then
  begin
    rlt := GetAddress(LOGGER_CR1000, Data, Len);
    Result := rlt = 0;
    Debug('GetAddress() -> %s', [IfThen(Result, '성공', '실패')]);
    if Result then
    begin
      Address := StrToIntDef(Copy(Data, Pos('=', Data) + 1, Length(Data)), 1);
      Debug('Address -> %d', [Address]);
      IpPortOpened := True;
    end
    else if Opened then
    begin
      IpPortOpened := True;
      SPBDLL.CloseIPPort;
    end;
  end;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.ClosePort
  작성자: isul
  작성일: 2008.02.11
  인  자: None
  결과값: Boolean
  설  명: 시리얼 포트 닫기
  ------------------------------------------------------------------------------- }
function TCR1000.ClosePort: Boolean;
begin
  if PortOpened then
  begin
    Debug('ClosePort()');
    Result := SPBDLL.ClosePort = 0;
    PortOpened := False;
  end
  else if IpPortOpened then
  begin
    Debug('ClosePort()');
    Result := SPBDLL.CloseIPPort = 0;
    IpPortOpened := False;
  end
  else
    Result := False;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.GetClock
  작성자: isul
  작성일: 2008.02.11
  인  자: Address: Integer; var ErrMsg: string
  결과값: Boolean
  설  명: 로거의 현재 시간 구하기
  ------------------------------------------------------------------------------- }
function TCR1000.GetClock(var ATime: PAnsiChar): Boolean;
var
  Len: Integer;
  ReturnCode: Integer;
begin
  ReturnCode := SPBDLL.GetClock(Address, LOGGER_CR1000, ATime, Len);
  SetLastError(ReturnCode);
  Result := ReturnCode = 0;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.SetClock
  작성자: isul
  작성일: 2008.02.11
  인  자: Address, Device: Integer; out pData: PAnsiChar; out pLgh: Integer; var ErrMsg: string
  결과값: Boolean
  설  명: PC와 로거 시간 동기화
  ------------------------------------------------------------------------------- }
function TCR1000.SetClock(var ATime: PAnsiChar): Boolean;
var
  Len: Integer;
  ReturnCode: Integer;
begin
  ReturnCode := SPBDLL.SetClock(Address, LOGGER_CR1000, ATime, Len);
  SetLastError(ReturnCode);
  Result := ReturnCode = 0;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.GetErrorMessage
  작성자: isul
  작성일: 2008.02.11
  인  자: ReturnCode: Integer
  결과값: string
  설  명: 에러 메시지 구하기
  ------------------------------------------------------------------------------- }
procedure TCR1000.SetLastError(ReturnCode: Integer; FuncType: Integer);
begin
  if ReturnCode = -1 then
    LastError := '통신 실패'
  else if ReturnCode = -2 then
    LastError := '시리얼 포트가 열리지 않았습니다.'
  else
    LastError := '';

  case FuncType of
    FN_GET_TABLENAMES:
      begin
        if ReturnCode = -2 then
          LastError := '데이터로거로부터 테이블 정보를 읽을 수 없습니다.';
      end;
  end;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.GetData
  작성자: isul
  작성일: 2008.02.11
  인  자: Address, Table, Recrd: Integer; out pData: PAnsiChar
  결과값: Integer
  설  명: 데이터 취득
  ------------------------------------------------------------------------------- }
function TCR1000.GetData(Table, Recrd: Integer; out pData: PAnsiChar): Integer;
var
  Len: Integer;
begin
  Result := SPBDLL.GetData(Address, LOGGER_CR1000, Table, Recrd, pData, Len);
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.GetCommaData
  작성자: isul
  작성일: 2008.02.11
  인  자: Address, Table, Recrd: Integer; out pData: PAnsiChar
  결과값: Integer
  설  명: 콤마로 분리된 데이터 형식으로 데이터 취득
  ------------------------------------------------------------------------------- }
function TCR1000.GetCommaData(Table, Recrd: Integer;
  out pData: PAnsiChar): Integer;
var
  Len: Integer;
begin
  Result := SPBDLL.GetCommaData(Address, LOGGER_CR1000, Table, Recrd,
    pData, Len);
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.GetStatus
  작성자: isul
  작성일: 2008.02.11
  인  자: None
  결과값: string
  설  명: 로거 상태 표시
  ------------------------------------------------------------------------------- }
function TCR1000.GetStatus: string;
var
  Data: PAnsiChar;
  Len: Integer;
  ReturnCode: Integer;
begin
  ReturnCode := SPBDLL.GetStatus(Address, LOGGER_CR1000, Data, Len);
  SetLastError(ReturnCode);
  Result := Data;
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.GetTableNames
  작성자: isul
  작성일: 2008.02.11
  인  자: out Tables: PAnsiChar
  결과값: Boolean
  설  명: 테이블명 구하기
  ------------------------------------------------------------------------------- }
function TCR1000.GetTableNames(out Tables: PAnsiChar): Boolean;
var
  Len: Integer;
  ReturnCode: Integer;
begin
  ReturnCode := SPBDLL.GetTableNames(Address, LOGGER_CR1000, Tables, Len);
  Result := ReturnCode = 0;
  if not Result then
    SetLastError(ReturnCode, FN_GET_TABLENAMES);
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.UpdateFirmware
  작성자: isul
  작성일: 2008.02.14
  인  자: FileName: PAnsiChar; out pData: PAnsiChar
  결과값: Integer
  설  명: 펌웨어 업데이트
  ------------------------------------------------------------------------------- }
function TCR1000.UpdateFirmware(FileName: PAnsiChar;
  out pData: PAnsiChar): Integer;
var
  Len: Integer;
begin
  Result := SPBDLL.File_Send(Address, LOGGER_CR1000, FileName, pData, Len);
end;

{ -------------------------------------------------------------------------------
  모듈명: TCR1000.Opened
  작성자: isul
  작성일: 2008.02.14
  인  자: None
  결과값: Boolean
  설  명: 시리얼 포트 오픈 여부
  ------------------------------------------------------------------------------- }
function TCR1000.Opened: Boolean;
begin
  Result := PortOpened;
end;

procedure TCR1000.Debug(const str: string);
begin
  Debug(str, []);
end;

procedure TCR1000.Debug(const Format: string; const Args: array of const);
var
  str: string;
begin
  FmtStr(str, Format, Args);
  OutputDebugString(PChar('::CR1000:: ' + str));
end;

end.
