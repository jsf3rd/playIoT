unit WIMProtocol;

interface

uses SysUtils;

type
  TWIMHeader  = record
  const
    HEADER_OPERATION = 'OPERATION';
    HEADER_LOOP_IN = 'LOOP_IN';
    HEADER_WIM_RAW = 'WIM_RAW';
    HEADER_WIM_RESULT = 'WIM_RESULT';
  end;

  TPoistion = record
    Left: Double;
    Right: Double;
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
  end;

  TCode = record
    Contravention: string;
    Drive: string;
    Calculation: Double;
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

{ TWIMRaw }

function TWIMRaw.Assigned: boolean;
begin
  Result := not Header.IsEmpty;
end;

end.
