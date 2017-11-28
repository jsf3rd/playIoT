unit WIMProtocol;

interface

uses SysUtils, System.JSON, JdcGlobal.ClassHelper, REST.JSON, System.MaskUtils, JdcGlobal,
  System.Classes, System.Generics.Collections, JdcGlobal.DSCommon;

type
  TWIMHeader = record
  const
    HEADER_OPERATION = 'OPERATION';
    HEADER_LOOP_IN = 'LOOP_IN';
    HEADER_WIM_RAW = 'WIM_RAW';
    HEADER_WIM_RESULT = 'WIM_RESULT';
  end;

  TPoistion = record
    Left: Double;
    Right: Double;
    function ToString: string;
  end;

  TNoise = array [0 .. 3] of Integer;
  TNoiseValue = array [0 .. 3] of Double;
  TWeightAxle = array [0 .. 6] of Integer;
  TWeightWheel = array [0 .. 6] of TPoistion;
  TDataNo = array [0 .. 6] of Double;
  TWheelBase = array [0 .. 5] of Double;
  TWheelTrack = array [0 .. 6] of Double;
  TDistribution = array [0 .. 3] of Double;
  TWeightGroup = array [0 .. 2] of Integer;

  TError = record
    Init: Integer;
    Noise: TNoise;
  end;

  TOperation = record
    Header: string;
    Site_id: string;
    Way: string;
    Error: TError;
    function Equals(AValue: TOperation): boolean;
    function ToString: string;
  end;

  TErrorOffset = record
    Init_value: Double;
    Noise_Value: TNoiseValue;
  end;

  TLoopIn = record
    Header: string;
    Site_id: string;
    Way: string;
    LANE: Integer;
    Loop1_id: string;
    Loop1_date: string;
    Loop1_time: string;
    function ToString: string;
    function GetPacket: string;
  end;

  TRecordInfo = record
    Site_id: string;
    Way: string;
    LANE: Integer;
    Loop1_id: string;
    Loop1_date: string;
    Loop1_time: string;
    Loop2_date: string;
    Loop2_time: string;
    function ToString: string;
    function Loop1: TDateTime;
    function Loop2: TDateTime;
  end;

  TCode = record
    Contravention: string;
    Drive: string;
    Calculation: Double;
    function ToString: string;
  end;

  TTemperature = record
    WIM1: Double;
    WIM2: Double;
    Ambient1: Double;
    Ambient2: Double;
    Rack: Double;
  end;

  TWIMCommon = record
    Record_info: TRecordInfo;
    Code: TCode;
    Temperature: TTemperature;
  end;

  TSpecification = record
    Vehicle_kind: Integer;
    Axle_no: Integer;
    Axle_type: string;
    Wheel_type: string;
    Vehicle_length: Double;
    Vehicle_overhang: Double;
  end;

  TWIMFinal = record
    Speed: Double;
    Safefactor: Double;
    Weight_gross: Integer;
    Weight_axle: TWeightAxle;
    Weight_wheel: TWeightWheel;
    Distribution_average: TDistribution;
    Specification: TSpecification;
    Data_no: TDataNo;
    Wheel_base: TWheelBase;
    Wheel_track: TWheelTrack;
    Tire_position: TPoistion;
    Acceleration: Double;
    Weight_Group: TWeightGroup;
  end;

  TFactor = record
    Conversion: Double;
    Temperature: Double;
    Speed: Double;
    Vehicle: Double;
    Axle_type: Double;
  end;

  TWIMData = record
    Weight_gross: Integer;
    Factor: TFactor;
    Weight_axle: TWeightAxle;
    Weight_wheel: TWeightWheel;
    Distribution_average: TDistribution;
    Specification: TSpecification;
    Data_no: TDataNo;
    Wheel_base: TWheelBase;
    Wheel_track: TWheelTrack;
    Error_Offset: TErrorOffset;
    function ToJSONParams(AGUID: string; ANum: Integer): TJSONObject;
  end;

  TWIMRaw = record
    Header: string;
    Common: TWIMCommon;
    WIM_Final: TWIMFinal;
    WIM_1: TWIMData;
    WIM_2: TWIMData;
    function Assigned: boolean;
  end;

  TAVIPack = record
    vehicle_number: string;
    condition: string;
  end;

  TVMSPack = record
    condition: string;
  end;

  TWIMResult = record
    Header: string;
    Common: TWIMCommon;
    WIM_Final: TWIMFinal;
    WIM_1: TWIMData;
    WIM_2: TWIMData;
    AVI: TAVIPack;
    VMS: TVMSPack;
    function ToJSONParams(AGUID: string; SiteID: Integer): TJSONObject;
  end;

  TWIMBackup = record
    WIMResult: TWIMResult;
    FrontImage: string;
    SideImage: string;
    function GetWIMRaw: TWIMRaw;
    constructor Create(AResult: TWIMResult); overload;
    constructor Create(ARaw: TWIMRaw); overload;
  end;

implementation

{ TOperation }

function TOperation.Equals(AValue: TOperation): boolean;
var
  I: Integer;
begin
  Result := True;
  if self.Header <> AValue.Header then
    exit(false);

  if self.Site_id <> AValue.Site_id then
    exit(false);

  if self.Way <> AValue.Way then
    exit(false);

  if self.Error.Init <> AValue.Error.Init then
    exit(false);

  for I := Low(TNoiseValue) to High(TNoiseValue) do
  begin
    if self.Error.Noise[I] <> AValue.Error.Noise[I] then
      exit(false);
  end;
end;

function TOperation.ToString: string;
begin
  Result := Header + '_' + Site_id + '_' + Way + '_' + self.Error.Init.ToString;
end;

{ TWIMRaw }

function TWIMRaw.Assigned: boolean;
begin
  Result := not Header.IsEmpty;
end;

{ TRecordInfo }

function TRecordInfo.Loop1: TDateTime;
begin
  Result := StrToDateTimeDef(FormatMaskText('0000-00-00 00:00:00#000;0',
    Loop1_date + Loop1_time), 0, DefaultFormatSettings);
end;

function TRecordInfo.Loop2: TDateTime;
begin
  Result := StrToDateTimeDef(FormatMaskText('0000-00-00 00:00:00#000;0',
    Loop2_date + Loop2_time), 0, DefaultFormatSettings);
end;

function TRecordInfo.ToString: string;
begin
  Result := Site_id + '_' + Way + '_' + LANE.ToString + '_' + Loop1_id + '_' + Loop1_date + '_'
    + Loop1_time;
end;

{ TCode }

function TCode.ToString: string;
begin
  Result := self.Contravention + ',' + self.Drive + ',' + self.Calculation.ToString;
end;

{ TWIMResult }

function TWIMResult.ToJSONParams(AGUID: String; SiteID: Integer): TJSONObject;
var
  tmp: TJSONObject;
  MyPair: TJSONPair;
  I: Integer;

  WeightAxle: TJSONPair;
begin
  Result := TJSONObject.Create;

  Result.AddPair('data_id', AGUID);
  Result.AddPair('_site_id', SiteID);

  Result.AddPair('loop_in', self.Common.Record_info.Loop1.ToISO8601);
  Result.AddPair('loop_out', self.Common.Record_info.Loop2.ToISO8601);

  TDSCommon.AddJSONValue<TRecordInfo>(Result, self.Common.Record_info);
  TDSCommon.AddJSONValue<TCode>(Result, self.Common.Code);
  TDSCommon.AddJSONValue<TTemperature>(Result, self.Common.Temperature);
  TDSCommon.AddJSONValue<TSpecification>(Result, self.WIM_Final.Specification);
  TDSCommon.AddJSONArray<TWIMFinal>(Result, self.WIM_Final);

  WeightAxle := Result.RemovePair('Weight_axle');
  Result.AddPair(WeightAxle.JsonString.Value, WeightAxle.JsonValue.Value + ',' +
    self.WIM_Final.Weight_gross.ToString);
  WeightAxle.Free;

  for I := Low(self.WIM_Final.Weight_wheel) to High(self.WIM_Final.Weight_wheel) do
  begin
    Result.AddPair('weight_wheel_axle' + (I + 1).ToString,
      self.WIM_Final.Weight_wheel[I].ToString);
  end;

  tmp := TJson.RecordToJsonObject<TPoistion>(self.WIM_Final.Tire_position);
  for MyPair in tmp do
  begin
    Result.AddPair(MyPair.JsonString.Value + '_tire_position',
      MyPair.JsonValue.Clone as TJSONValue);
  end;
  tmp.Free;

  Result.AddPair('vehicle_number', self.AVI.vehicle_number);
  Result.AddPair('avi_result', self.AVI.condition);
  Result.AddPair('vms_result', self.VMS.condition);
end;

{ TPoistion }

function TPoistion.ToString: string;
begin
  Result := FloatToStr(Left) + ',' + FloatToStr(Right);
end;

{ TWIMData }

function TWIMData.ToJSONParams(AGUID: string; ANum: Integer): TJSONObject;
var
  I: Integer;
  WeightAxle: TJSONPair;
begin
  Result := TJSONObject.Create;

  Result.AddPair('data_id', AGUID);
  Result.AddPair('wim_num', ANum);

  TDSCommon.AddJSONValue<TFactor>(Result, self.Factor, 'correction_');
  TDSCommon.AddJSONValue<TSpecification>(Result, self.Specification);
  TDSCommon.AddJSONArray<TErrorOffset>(Result, self.Error_Offset);
  TDSCommon.AddJSONArray<TWIMData>(Result, self);

  WeightAxle := Result.RemovePair('Weight_axle');
  Result.AddPair(WeightAxle.JsonString.Value, WeightAxle.JsonValue.Value + ',' +
    self.Weight_gross.ToString);
  WeightAxle.Free;

  for I := Low(self.Weight_wheel) to High(self.Weight_wheel) do
  begin
    Result.AddPair('weight_wheel_axle' + (I + 1).ToString, self.Weight_wheel[I].ToString);
  end;
end;

{ TLoopIn }

function TLoopIn.GetPacket: string;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.Add(self.Header);
    StringList.Add(self.Site_id);
    StringList.Add(self.Way);
    StringList.Add(self.LANE.ToString);
    StringList.Add(self.Loop1_id);
    StringList.Add(self.Loop1_date);
    StringList.Add(self.Loop1_time);
    StringList.Add('2'); // SideImage 컷번호
    Result := StringList.CommaText;
  finally
    StringList.Free;
  end;
end;

function TLoopIn.ToString: string;
begin
  Result := self.Site_id + '_' + self.Way + '_' + self.LANE.ToString + '_' + self.Loop1_id +
    '_' + self.Loop1_date + '_' + self.Loop1_time;
end;

{ TWIMBackup }

constructor TWIMBackup.Create(AResult: TWIMResult);
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJson.RecordToJsonObject(AResult);
  self.WIMResult := TJson.JsonToRecord<TWIMResult>(JSONObject);
  JSONObject.Free;
end;

constructor TWIMBackup.Create(ARaw: TWIMRaw);
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJson.RecordToJsonObject(ARaw);
  self.WIMResult := TJson.JsonToRecord<TWIMResult>(JSONObject);
  JSONObject.Free;

  self.WIMResult.AVI.vehicle_number := '미수신';
  self.WIMResult.AVI.condition := 'AR04';
  self.WIMResult.VMS.condition := 'VR04';
  self.FrontImage := 'no_image';
  self.SideImage := 'no_image';
end;

function TWIMBackup.GetWIMRaw: TWIMRaw;
var
  JSONObject: TJSONObject;
begin
  JSONObject := TJson.RecordToJsonObject(self.WIMResult);
  Result := TJson.JsonToRecord<TWIMRaw>(JSONObject);
  JSONObject.Free;
end;

end.
