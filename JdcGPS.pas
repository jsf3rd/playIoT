unit JdcGPS;

interface

uses System.SysUtils, System.Classes, JdcGlobal, Data.DB, System.JSON, REST.JSON,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, JdcGlobal.DSCommon,
  System.Types, FireDAC.Stan.Param, CPort, IdGlobal, JdcLogging, System.DateUtils,
  JdcGlobal.ClassHelper, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient;

type
  TConnMode = (cmUSB, cmTCP);

  TGPSData = record
    Header: string;
    DateTime: TDateTime; // GPS Time
    latitude: Double;
    NS_Indicator: String;
    longitude: Double;
    EW_Indicator: string;
    altitude: Double;
    Speed: Double;
    Cog: Double;
    quality: Integer;
    PCTime: TDateTime;
    constructor Create(const AValue: string; const _PCTime: TDateTime);
    function GetPCTime: TDateTime;
    function ToString(): string;
    function IsValid(AMsec: Integer = 200): Boolean;
    function IsFixed: Boolean;
    function IsNoSignal: Boolean;

    function IsNorth: Boolean;
    function IsEast: Boolean;

    procedure Clear;
  end;

  TOnGPSData = procedure(const AData: TGPSData) of object;

  TGPS = class
  private
    FComLink: TComLink;
    FComPort: TComPort;

    FGPSTask: TThread;
    FTCPClient: TIdTCPClient;

    FBuffer: TIdBytes;
    FGPSMsg: TStrings;
    FSpeed: Double;

    FConnMode: TConnMode;
    FOnGPSData: TOnGPSData;

    FMsgCount: Integer;

    procedure OnRxBuf(Sender: TObject; const Buffer; Count: Integer);
    procedure SetConnInfo(const Value: TConnInfo);
    function GetConnInfo: TConnInfo;
    procedure ReadGPSMessage();
    procedure ParsePacket; overload;
    procedure ParsePacket(const AMsg: string); overload;
  public const
    GPS_NO_SIGNAL = 0;
    GPS_NORMAL = 1;
    GPS_DIFF = 2;
    GPS_FIXED = 4;
    GPS_FLOAT = 5;
    GPS_ASSISTED = 6;

    constructor Create(const AMode: TConnMode = cmUSB; AOnGPSData: TOnGPSData = nil; MsgCount: Integer = 2);
    destructor Destroy; override;

    procedure Open;
    procedure Close;

    procedure ShowSetupDialog;
    function Connected: Boolean;

    property ConnInfo: TConnInfo read GetConnInfo write SetConnInfo;
    property OnGPSData: TOnGPSData read FOnGPSData write FOnGPSData;
  end;

function QualityToStr(AValue: Integer): string;

const
  GPS_GGA = 'GGA';
  GPS_RMC = 'RMC';

implementation

function QualityToStr(AValue: Integer): string;
begin
  case AValue of
    TGPS.GPS_NO_SIGNAL:
      result := 'GPS수신불가';
    TGPS.GPS_NORMAL:
      result := '일반GPS';
    TGPS.GPS_DIFF:
      result := 'DGPS';
    TGPS.GPS_FIXED:
      result := 'Fixed RTK';
    TGPS.GPS_FLOAT:
      result := 'Float RTK';
  else
    result := 'ERROR(' + AValue.ToString + ')';
  end;
end;

{ TGPS }

procedure TGPSData.Clear;
begin
  Self.Header := '';
  Self.DateTime := 0;
  Self.latitude := 0;
  Self.longitude := 0;
  Self.Speed := 0;
  Self.quality := 0;
  Self.PCTime := 0;
end;

constructor TGPSData.Create(const AValue: string; const _PCTime: TDateTime);
var
  Msg: TStringList;
  Index: Integer;
begin
  Self.Clear;
  Self.PCTime := _PCTime;

  Msg := TStringList.Create;
  try
    Msg.CommaText := AValue;

    Index := 0;
    while Index < Msg.Count do
    begin
      if Pos(GPS_RMC, Msg.Strings[Index]) > 0 then
      begin
        Inc(Index);

        if Msg.Count < Index + 10 then
          Continue;

        Self.Header := GPS_RMC;

        // $GPRMC,hhmmss,status,latitude,N,longitude,E,spd,cog,ddmmyy,mv,mvE,mode*cs<CR><LF>
        // $GPRMC,083559.00,A,4717.11437,N,00833.91522,E,0.004,77.52,091202,,,A*57

        if Msg.Strings[Index + 1] <> 'A' then
          Self.quality := 0
        else
          Self.quality := 1;

        Self.DateTime := StrToDateTimeDef(FormatDateTime('YYYY-MM-DD ', Now) + Format('%s:%s:%s%s0',
          [copy(Msg.Strings[Index], 1, 2), copy(Msg.Strings[Index], 3, 2), copy(Msg.Strings[Index], 5, 2),
          copy(Msg.Strings[Index], 7, 3)]), _PCTime, DefaultFormatSettings);
        Self.latitude := ConvertDegree(Msg.Strings[Index + 2]);
        Self.NS_Indicator := Msg.Strings[Index + 3];
        Self.longitude := ConvertDegree(Msg.Strings[Index + 4]);
        Self.EW_Indicator := Msg.Strings[Index + 5];
        Self.altitude := 0;
        Self.Speed := StrToFloatDef(Msg.Strings[Index + 6], 0) * 1.852; // knots to km/h
        Self.Cog := StrToFloatDef(Msg.Strings[Index + 7], 0); // Course Over Ground
        Index := Index + 11;
      end;

      if Pos(GPS_GGA, Msg.Strings[Index]) > 0 then
      begin
        Inc(Index);

        if Msg.Count < Index + 14 then
          Continue;

        Self.Header := GPS_GGA;

        // $GPGGA,hhmmss.ss,Latitude,N,Longitude,E,FS,NoSV,HDOP,msl,m,Altref,m,DiffAge,DiffStation*cs<CR><LF>
        // $GPGGA,092725.00,4717.11399,N,00833.91590,E,1,8,1.01,499.6,M,48.0,M,,0*5B

        // $GNGGA,090331.12,3651.27214,N,12637.43041,E,4,12,0.57,62.5,M,18.1,M,,4095*7C
        // GNGGA,012952.85,3726.2240625,N,12707.3641959,E,4,09,1.4,34.6334,M,19.9003,M,0.9,0000*68

        Self.DateTime := StrToDateTimeDef(FormatDateTime('YYYY-MM-DD ', Now) + Format('%s:%s:%s%s0',
          [copy(Msg.Strings[Index], 1, 2), copy(Msg.Strings[Index], 3, 2), copy(Msg.Strings[Index], 5, 2),
          copy(Msg.Strings[Index], 7, 3)]), _PCTime, DefaultFormatSettings);
        Self.latitude := ConvertDegree(Msg.Strings[Index + 1]);
        Self.NS_Indicator := Msg.Strings[Index + 2];
        Self.longitude := ConvertDegree(Msg.Strings[Index + 3]);
        Self.EW_Indicator := Msg.Strings[Index + 4];
        Self.altitude := StrToFloatDef(Msg.Strings[Index + 8], 0);
        Self.Speed := 0;
        Self.Cog := 0;

        // 0: Fix not valid
        // 1: GPS fix
        // 2: Differential GPS fix (DGNSS), SBAS, OmniSTAR VBS, Beacon, RTX in GVBS mode
        // 3: Not applicable
        // 4: RTK Fixed, xFill
        // 5: RTK Float, OmniSTAR XP/HP, Location RTK, RTX
        // 6: INS Dead reckoning
        quality := StrToIntDef(Msg.Strings[Index + 5], 0);
        Index := Index + 15;
      end;
      Inc(Index);
    end;
  finally
    Msg.Free;
  end;
end;

function TGPSData.GetPCTime: TDateTime;
begin
  if Self.PCTime > 0 then
    result := Self.PCTime
  else
    result := Now;
end;

function TGPSData.IsEast: Boolean;
begin
  result := UpperCase(Self.NS_Indicator) = 'E';
end;

function TGPSData.IsNoSignal: Boolean;
begin
  result := IsValid(2000) and ((quality = TGPS.GPS_NO_SIGNAL) or (quality = TGPS.GPS_ASSISTED))
end;

function TGPSData.IsFixed: Boolean;
begin
  result := quality = TGPS.GPS_FIXED;
end;

function TGPSData.IsNorth: Boolean;
begin
  result := UpperCase(Self.NS_Indicator) = 'N';
end;

function TGPSData.IsValid(AMsec: Integer): Boolean;
begin
  result := MilliSecondsBetween(Now, PCTime) < AMsec;
end;

function TGPSData.ToString: string;
begin
  result := Format('%s,%s,%0.6f,%0.6f,%d,%0.2f', [Self.Header, Self.DateTime.FormatWithMSec, Self.latitude,
    Self.longitude, Self.quality, Self.Speed]);
end;

{ TGPS }

procedure TGPS.Close;
begin
  if Assigned(FComPort) then
    FComPort.Close;

  if Assigned(FGPSTask) then
  begin
    FGPSTask.Terminate;
    FGPSTask.WaitFor;
    FreeAndNil(FGPSTask);
  end;

  if Assigned(FTCPClient) then
    FTCPClient.Disconnect;

  TLogging.Obj.ApplicationMessage(msInfo, 'GPS', 'Disconnected');
end;

function TGPS.Connected: Boolean;
begin
  if Assigned(FComPort) then
    result := FComPort.Connected
  else
    result := FTCPClient.Connected;
end;

constructor TGPS.Create(const AMode: TConnMode; AOnGPSData: TOnGPSData; MsgCount: Integer);
begin
  FOnGPSData := AOnGPSData;
  FConnMode := AMode;
  FGPSMsg := TStringList.Create;
  FMsgCount := MsgCount;

  if FConnMode = cmUSB then
  begin
    FComPort := TComPort.Create(nil);
    FComLink := TComLink.Create;
    FComLink.OnRxBuf := OnRxBuf;
    FComPort.RegisterLink(FComLink);
  end
  else
  begin
    FTCPClient := TIdTCPClient.Create(nil);
    FTCPClient.ConnectTimeout := 500;
    FTCPClient.ReadTimeout := 100;
  end;
end;

destructor TGPS.Destroy;
begin
  if Connected then
    Close;

  if Assigned(FComPort) then
  begin
    try
      FComPort.UnRegisterLink(FComLink);
      FComLink.Free;
    except
      on E: Exception do
        raise Exception.Create('CloseGPS, E=' + E.Message);
    end;
  end;

  if Assigned(FGPSMsg) then
    FreeAndNilEx(FGPSMsg);

  if Assigned(FComPort) then
    FreeAndNil(FComPort);

  if Assigned(FTCPClient) then
    FreeAndNil(FTCPClient)
end;

function TGPS.GetConnInfo: TConnInfo;
begin
  if Assigned(FComPort) then
  begin
    result.StringValue := FComPort.Port;
    result.IntegerValue := BaudRateToStr(FComPort.BaudRate).ToInteger;
  end;

  if Assigned(FTCPClient) then
  begin
    result.StringValue := FTCPClient.Host;
    result.IntegerValue := FTCPClient.Port;
  end;

end;

procedure TGPS.OnRxBuf(Sender: TObject; const Buffer; Count: Integer);
var
  buff: TIdBytes;
begin
  SetLength(buff, Count);
  Move(Buffer, buff[0], Count);
  AppendBytes(FBuffer, buff);
  try
    ParsePacket;
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msError, 'ParsePacket', 'Count=%d,E=%d', [Count, E.Message]);
  end;
end;

procedure TGPS.Open;
var
  buff: string;
begin
  try
    FSpeed := 0;

    if FConnMode = cmUSB then
    begin
      FComPort.Open;

      // ReadStr를 호출하지 않으면 RxBuf, RxChar 이벤트가 발생하지 않음
      FComPort.ReadStr(buff, 1);
      FBuffer := ToBytes(buff);
    end
    else
    begin
      FTCPClient.Connect;
      FGPSTask := TThread.CreateAnonymousThread(
        procedure
        begin
          while not TThread.CurrentThread.CheckTerminated do
          begin
            Sleep(1);
            try
              ReadGPSMessage();
            except
              on E: Exception do
                TLogging.Obj.ApplicationMessage(msWarning, 'ReadGPSMessage', E.Message);
            end;
          end;
        end);
      FGPSTask.FreeOnTerminate := False;
      FGPSTask.Start;
    end;

    TLogging.Obj.ApplicationMessage(msInfo, 'GPS', 'Connected');
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msError, 'OpenGPS', E.Message);
  end;
end;

function ConvertDegree(Value: string): Double;
var
  frac: Double;
begin
  result := StrToFloatDef(Value, 0);
  if result = 0 then
    Exit;

  frac := result - Trunc(result / 100) * 100;
  result := Trunc(result / 100);
  result := result + frac / 60;
end;

procedure TGPS.ParsePacket;
begin
  if FBuffer[Length(FBuffer) - 1] <> $A then
    Exit;

  try
    ParsePacket(BytesToString(FBuffer));
  finally
    SetLength(FBuffer, 0);
  end;
end;

procedure TGPS.ParsePacket(const AMsg: string);
var
  MyMsg: string;
  GPS: TGPSData;

  PCTime: TDateTime;
begin
  PCTime := Now;
  FGPSMsg.Text := AMsg;

  if FGPSMsg.Count > FMsgCount then
    Exit;

  for MyMsg in FGPSMsg do
  begin
    GPS := TGPSData.Create(MyMsg, PCTime);
    if GPS.Header = GPS_GGA then
    begin
      GPS.Speed := FSpeed;
    end
    else if GPS.Header = GPS_RMC then
      FSpeed := GPS.Speed;

    if Assigned(FOnGPSData) then
      FOnGPSData(GPS);
  end;
end;

procedure TGPS.ReadGPSMessage;
var
  Msg: string;
begin
  if not FTCPClient.Connected then
    Exit;

  try
    Msg := FTCPClient.IOHandler.ReadLn();
    ParsePacket(Msg);
  except
    on E: Exception do
  end;

end;

procedure TGPS.SetConnInfo(const Value: TConnInfo);
begin
  if FConnMode = cmUSB then
  begin
    FComPort.Port := Value.StringValue;
    FComPort.BaudRate := StrToBaudRate(Value.IntegerValue.ToString);
  end
  else
  begin
    FTCPClient.Host := Value.StringValue;
    FTCPClient.Port := Value.IntegerValue;
  end;
end;

procedure TGPS.ShowSetupDialog;
begin
  FComPort.ShowSetupDialog;
end;

end.
