unit JdcSM125;

interface

uses
  Classes, SysUtils, IdGlobal, Windows, JdcSM125Global;

const
  RESPONSE_MAX = 5000;
  DATA_MAX = 131160;

type
  TJdcSM125 = class
  private
    FConnected: Boolean;
    FDLLHandle: THandle;

    FSM125_Connect: TSM125_Connect;
    FSM125_Disconnect: TSM125_Disconnect;
    FSendCommand: TSendCommand;
    FDataRead: TDataRead;
    FSendCommandPD: TSendCommandPD;
    FDataReadPD: TDataReadPD;

    procedure LoadDLL;
    procedure FreeDLL;

    function GetErrorMsg(ACode: Integer): String;

    procedure CheckException(ACode: Integer);
    function GetConnected: Boolean;
  public
    constructor Create;

    procedure Connect(const AIP: String);
    procedure Disconnect;
    destructor Destroy; override;

    function GetCommandList: String;

    procedure GetData(const ACommand: String; var AData: TIdBytes);
    function ExcuteCommand(const ACommand: String; AParam: String = ''): String;

    property Connected: Boolean read GetConnected;
  end;

implementation

{ TJdcSM125 }

procedure TJdcSM125.Connect(const AIP: String);
begin
  CheckException(FSM125_Connect(PAnsiChar(AnsiString(AIP))));
  FConnected := true;
end;

constructor TJdcSM125.Create;
begin
  inherited;

  FConnected := false;
  LoadDLL;
end;

destructor TJdcSM125.Destroy;
begin
  FreeDLL;

  inherited;
end;

procedure TJdcSM125.CheckException(ACode: Integer);
begin
  if ACode <> 0 then
  begin
    raise Exception.Create(GetErrorMsg(ACode));
  end;
end;

procedure TJdcSM125.Disconnect;
begin
  if not FConnected then
    exit;

  FConnected := false;
  CheckException(FSM125_Disconnect);
end;

function TJdcSM125.GetCommandList: String;
begin
  result := TSM125Global.Obj.CommandList.Text;
end;

function TJdcSM125.GetConnected: Boolean;
begin
  result := FConnected;
end;

procedure TJdcSM125.GetData(const ACommand: String; var AData: TIdBytes);
var
  Size: Integer;
begin
  if TSM125Global.Obj.CommandList.IndexOf(ACommand) < IDX_PEAKS_DETECT then
  begin
    CheckException(FSendCommand(PAnsiChar(AnsiString(ACommand + #10))));
    SetLength(AData, DATA_MAX);
    CheckException(FDataRead(AData[0], Size));
  end
  else
  begin
    CheckException(FSendCommandPD(PAnsiChar(AnsiString(ACommand + #10))));
    SetLength(AData, DATA_MAX);
    CheckException(FDataReadPD(AData[0], Size));
  end;

  SetLength(AData, Size);
end;

function TJdcSM125.GetErrorMsg(ACode: Integer): String;
begin
  result := TSM125Global.Obj.ErrorMsg.Values[IntToStr(ACode)];

  if result = '' then
  begin
    result := 'Unknown Error. ' + IntToStr(ACode);
  end;
end;

procedure TJdcSM125.LoadDLL;
begin
  FDLLHandle := LoadLibrary(SM125_COMM_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  @FSM125_Connect := GetProcAddress(FDLLHandle, 'sm125_Connect');
  @FSM125_Disconnect := GetProcAddress(FDLLHandle, 'sm125_Disconnect');
  @FSendCommand := GetProcAddress(FDLLHandle, 'SendCommand');
  @FDataRead := GetProcAddress(FDLLHandle, 'DataRead');
  @FSendCommandPD := GetProcAddress(FDLLHandle, 'SendCommandPD');
  @FDataReadPD := GetProcAddress(FDLLHandle, 'DataReadPD');
end;

function TJdcSM125.ExcuteCommand(const ACommand: String; AParam: String = ''): String;
var
  Response: TIdBytes;
  Size: Integer;
begin

  if TSM125Global.Obj.CommandList.IndexOf(ACommand) = IDX_GET_DATA then // GET_DATA
    exit;
  if TSM125Global.Obj.CommandList.IndexOf(ACommand) = IDX_GET_PEAKS_AND_LEVELS then
    // GET_PEAKS_AND_LEVELS
    exit;

  if TSM125Global.Obj.CommandList.IndexOf(ACommand) < IDX_PEAKS_DETECT then
  begin
    CheckException(FSendCommand(PAnsiChar(AnsiString(ACommand + ' ' + AParam + #10))));
    SetLength(Response, RESPONSE_MAX);
    try
      CheckException(FDataRead(Response[0], Size));
      result := BytesToString(Response, 0, Size);
    finally
      SetLength(Response, 0);
    end;

  end
  else
  begin
    CheckException(FSendCommandPD(PAnsiChar(AnsiString(ACommand + ' ' + AParam + #10))));
    SetLength(Response, RESPONSE_MAX);
    try
      CheckException(FDataReadPD(Response[0], Size));
      result := BytesToString(Response, 0, Size);
    finally
      SetLength(Response, 0);
    end;

  end;

end;

procedure TJdcSM125.FreeDLL;
begin
  if FDLLHandle <> 0 then
  begin
    try
      FreeLibrary(FDLLHandle);
      FDLLHandle := 0;
    except
      on E: Exception do
    end;
  end;
end;

end.
