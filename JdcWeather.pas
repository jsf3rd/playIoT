unit JdcWeather;

interface

uses System.Classes, System.SysUtils, System.DateUtils, REST.Types, JdcGlobal.DSCommon,
  REST.Client, Data.Bind.Components, Data.Bind.ObjectScope, JdcGlobal.ClassHelper, REST.JSON,
  Data.DB, FireDAC.Comp.DataSet, FireDAC.Comp.Client, math;

type
  TwcResult = record
    status: Integer;
    message: string;
  end;

  TwcResponse = record
    result: TwcResult;
  end;

  TStationInfo = record
    stn_id: Integer; // ÁöÁ¡¹øÈ£
    lat: double; // À§µµ(degree)
    lon: double; // °æµµ(degree)
    stn_ko: string; // ÁöÁ¡¸í
    constructor Create(Value: TArray<String>);
  end;

  TWeatherInfo = record
    content: string;
    datetime: string; // ÃøÁ¤½Ã°£
    stn_id: Integer; // ÁöÁ¡¹øÈ£
    wd1: double; // 1ºÐ Æò±Õ Ç³Çâ(degress)
    ws1: double; // 1ºÐ Æò±Õ Ç³¼Ó(m/s)
    wds: double; // ÃÖ´ë ¼ø°£ Ç³Çâ(degress)
    wss: double; // ÃÖ´ë ¼ø°£ Ç³¼Ó(m/s)
    wd10: double; // 10ºÐ Æò±Õ Ç³Çâ(degress)
    ws10: double; // 10ºÐ Æò±Õ Ç³¼Ó(m/s)
    TA: double; // 1ºÐ Æò±Õ ±â¿Â(C)
    constructor Create(Value: String);
    function ToString: String;
  end;

  TWeatherAPI = Class
  private
    FAPIKey: string;

    FStationList: TArray<TStationInfo>;
    FRestClient: TRESTClient;
    FRestReqeust: TRESTRequest;
    FRestResponse: TRESTResponse;

    FBusy: Boolean;
    function ParseStationList(const Value: string): TArray<TStationInfo>;
    function ParseWeatherInfo(const Value: string): TWeatherInfo;
  public
    constructor Create(APIKey: string);
    destructor Destroy; override;

    function GetStationList(const ADateTime: string = ''): TArray<TStationInfo>;
    procedure UpdateStationTab(const Value: TArray<TStationInfo>; MemTab: TFDDataSet);

    procedure ClearStationList;
    function Active: Boolean;

    procedure DeleteStation(AID: Integer);
    function GetAWS2(const ADateTime: string; const AID: Integer): TWeatherInfo;
    function FindStation(const ALat: double; const ALon: double; MemTab: TFDDataSet)
      : TStationInfo; overload;
    function FindStation(const ALat: double; const ALon: double): TStationInfo; overload;
    function StationCount: Integer;

    property APIKey: string read FAPIKey write FAPIKey;
    property Busy: Boolean read FBusy;
  End;

implementation

{ TWeatherAPI }

function TWeatherAPI.Active: Boolean;
begin
  result := FAPIKey <> '';
end;

procedure TWeatherAPI.ClearStationList;
begin
  SetLength(FStationList, 0);
end;

constructor TWeatherAPI.Create(APIKey: string);
begin
  FAPIKey := APIKey;
  FRestClient := TRESTClient.Create('');
  FRestReqeust := TRESTRequest.Create(nil);
  FRestReqeust.ReadTimeout := 60000;
  FRestResponse := TRESTResponse.Create(nil);
  FRestReqeust.Client := FRestClient;
  FRestReqeust.Response := FRestResponse;
  FBusy := False;
end;

procedure TWeatherAPI.DeleteStation(AID: Integer);
var
  I: Integer;
begin
  for I := Low(FStationList) to High(FStationList) do
  begin
    if FStationList[I].stn_id = AID then
    begin
      Delete(FStationList, I, 1);
      Break;
    end;
  end;
end;

destructor TWeatherAPI.Destroy;
begin
  FRestClient.Free;
  FRestResponse.Free;
  FRestReqeust.Free;
  inherited;
end;

function TWeatherAPI.FindStation(const ALat, ALon: double): TStationInfo;
var
  dist, tmp: double;
  I: Integer;
begin
  result.stn_id := 0;
  if Length(FStationList) = 0 then
    Exit;

  dist := MaxInt;
  for I := Low(FStationList) to High(FStationList) do
  begin
    tmp := Sqrt(Power(ALat - FStationList[I].lat, 2) + Power(ALon - FStationList[I].lon, 2));

    if tmp < dist then
    begin
      dist := tmp;
      result := FStationList[I];
    end;
  end;
end;

function TWeatherAPI.FindStation(const ALat, ALon: double; MemTab: TFDDataSet): TStationInfo;
var
  Bookmark: TBookmark;
  dist, tmp: double;
begin
  result.stn_id := 0;
  if not Assigned(MemTab) then
    Exit;

  Bookmark := MemTab.GetBookmark;
  dist := MaxInt;

  MemTab.DisableControls;
  try
    MemTab.First;
    while not MemTab.Eof do
    begin
      tmp := Sqrt(Power(ALat - MemTab.FieldByName('lat').AsFloat, 2) +
        Power(ALon - MemTab.FieldByName('lon').AsFloat, 2));

      if tmp < dist then
      begin
        dist := tmp;
        result := MemTab.ToRecord<TStationInfo>;
      end;
      MemTab.Next;
    end;
    MemTab.Bookmark := Bookmark;
  finally
    MemTab.EnableControls;
  end;
end;

function TWeatherAPI.GetAWS2(const ADateTime: string; const AID: Integer): TWeatherInfo;
var
  Response: TwcResponse;
const
  URL = 'https://apihub.kma.go.kr/api/typ01/cgi-bin/url/nph-aws2_min';
begin
  FBusy := True;
  try
    FRestClient.BaseURL := URL;
    FRestReqeust.Params.AddItem('tm2', Trim(ADateTime));
    FRestReqeust.Params.AddItem('stn', AID.ToString);
    FRestReqeust.Params.AddItem('disp', '1');
    FRestReqeust.Params.AddItem('help', '0');
    FRestReqeust.Params.AddItem('authKey', FAPIKey);

    FRestReqeust.Execute;
    if FRestResponse.StatusCode = 200 then
      result := ParseWeatherInfo(FRestResponse.content)
    else
    begin
      Response := TJSON.JsonToRecord<TwcResponse>(FRestResponse.content);
      raise Exception.Create(Format('Status=%d,message=%s', [Response.result.status,
        Response.result.message]));
    end;
  finally
    FBusy := False;
  end;
end;

function TWeatherAPI.GetStationList(const ADateTime: string): TArray<TStationInfo>;
var
  Response: TwcResponse;
const
  URL = 'https://apihub.kma.go.kr/api/typ01/url/stn_inf.php';
begin
  FBusy := True;
  try
    FRestClient.BaseURL := URL;
    FRestReqeust.Params.AddItem('inf', 'AWS');
    FRestReqeust.Params.AddItem('authKey', FAPIKey);
    FRestReqeust.Params.AddItem('help', '0');
    FRestReqeust.Params.AddItem('tm', Trim(ADateTime));
    FRestReqeust.Execute;
    if FRestResponse.StatusCode = 200 then
    begin
      FStationList := ParseStationList(FRestResponse.content);
      result := FStationList;
    end
    else
    begin
      Response := TJSON.JsonToRecord<TwcResponse>(FRestResponse.content);
      raise Exception.Create(Format('Status=%d,message=%s', [Response.result.status,
        Response.result.message]));
    end;
  finally
    FBusy := False;
  end;
end;

function TWeatherAPI.ParseStationList(const Value: string): TArray<TStationInfo>;
var
  StationList: TStrings;
  MyData, tmp: string;
  StationInfo: TStationInfo;
begin
  SetLength(result, 0);
  StationList := TStringList.Create;
  try
    StationList.Text := Value;

    for MyData in StationList do
    begin
      tmp := MyData.Replace('  ', ' ').Replace('  ', ' ').Replace('  ', ' ');
      StationInfo := TStationInfo.Create(tmp.Split([' ']));
      if StationInfo.stn_id = 0 then
        Continue;

      result := result + [StationInfo];
    end;
  finally
    StationList.Free;
  end;
end;

function TWeatherAPI.ParseWeatherInfo(const Value: string): TWeatherInfo;
var
  StringList: TStrings;
  MyData: string;
begin
  result.stn_id := 0;
  StringList := TStringList.Create;
  try
    StringList.Text := Value;
    for MyData in StringList do
    begin
      result := TWeatherInfo.Create(MyData);
      if result.stn_id = 0 then
        Continue
      else
        Break;
    end;
  finally
    StringList.Free;
  end;
end;

function TWeatherAPI.StationCount: Integer;
begin
  result := Length(FStationList);
end;

procedure TWeatherAPI.UpdateStationTab(const Value: TArray<TStationInfo>; MemTab: TFDDataSet);
var
  StationInfo: TStationInfo;
begin
  MemTab.DisableControls;
  try
    for StationInfo in Value do
    begin
      if StationInfo.stn_id = 0 then
        Continue;

      MemTab.Append;
      MemTab.FieldByJSONObject(TJSON.RecordToJsonObject<TStationInfo>(StationInfo));
    end;
    MemTab.CommitUpdates;
    MemTab.First;
  finally
    MemTab.EnableControls;
  end;
end;

{ TStationInfo }

constructor TStationInfo.Create(Value: TArray<String>);
begin
  if Length(Value) < 10 then
  begin
    Self.stn_id := 0;
    Exit;
  end;

  Self.stn_id := StrToIntDef(Value[1], 0);
  Self.lon := StrToFloatDef(Value[2], 0);
  Self.lat := StrToFloatDef(Value[3], 0);
  Self.stn_ko := Value[9];
end;

{ TWeatherInfo }

constructor TWeatherInfo.Create(Value: String);
var
  Items: TArray<String>;
begin
  Self.stn_id := 0;

  Items := Value.Split([',']);
  if (Length(Items) < 9) then
    Exit;

  Self.stn_id := StrToIntDef(Items[1], 0);
  if Self.stn_id = 0 then
    Exit;

  Self.content := Value;
  Self.datetime := Items[0];
  Self.wd1 := StrToFloatDef(Items[2], 0);
  Self.ws1 := StrToFloatDef(Items[3], 0);
  Self.wds := StrToFloatDef(Items[4], 0);
  Self.wss := StrToFloatDef(Items[5], 0);
  Self.wd10 := StrToFloatDef(Items[6], 0);
  Self.ws10 := StrToFloatDef(Items[7], 0);
  Self.TA := StrToFloatDef(Items[8], 0);
end;

function TWeatherInfo.ToString: String;
begin
  result := Format('%s,%d,%0.1f', [Self.datetime, Self.stn_id, TA]);
end;

end.
