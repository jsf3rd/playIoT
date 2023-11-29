unit JdcMVS.Hik;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  JdcMVS.Abstract, System.Math;

type
  TJdcMVSHik = class(TJdcMVSAbstract)
  private
  protected
    function GetVendorName: string; override;
  public
    constructor Create(AModel: string); override;

    procedure SetAutoConfig(GainAuto: MV_CAM_GAIN_MODE = MV_GAIN_MODE_OFF;
      ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE = MV_EXPOSURE_AUTO_MODE_CONTINUOUS); override;
    procedure SetGain(AValue: Double); override;
    function SetAcquisitionLineRate(AValue: Double): Integer; override;
    function SetAutoExposureTimeUpperLimit(AValue: Double): Integer; override;

    function GetAcquisitionLineRate: Double; override;
    function GetExposureTime: Double; override;

  const
    VENDOR_NAME = 'Hikrobot';
  end;

implementation

const
  GAIN_RAW = 'Gain';
  ACQUISITION_LINERATE = 'AcquisitionLineRate';
  ADC_GAIN_ENABLE = 'ADCGainEnable';
  ACQUISITION_LINERATE_ENABLE = 'AcquisitionLineRateEnable';
  AUTO_EXPOSURETIME_UPPER_LIMIT = 'AutoExposureTimeUpperLimit';
  EXPOSURE_TIME = 'ExposureTime';

  { TJdcMVSHik }

constructor TJdcMVSHik.Create(AModel: string);
begin
  inherited;

  FCameraVendor := cvHik;
end;

function TJdcMVSHik.GetAcquisitionLineRate: Double;
var
  IntValue: Cardinal;
begin
  GetIntValue(m_hDevHandle, ACQUISITION_LINERATE, @IntValue);
  result := IntValue;
end;

function TJdcMVSHik.GetExposureTime: Double;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, EXPOSURE_TIME, @FloatValue);
  result := FloatValue;
end;

function TJdcMVSHik.GetVendorName: string;
begin
  result := VENDOR_NAME;
end;

function TJdcMVSHik.SetAcquisitionLineRate(AValue: Double): Integer;
begin
  result := _SetIntValue(ACQUISITION_LINERATE, Trunc(AValue));
end;

procedure TJdcMVSHik.SetAutoConfig(GainAuto: MV_CAM_GAIN_MODE;
  ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE);
begin
  inherited;

  if FCameraType = ctLinescan then
  begin
    _SetBoolValue(ADC_GAIN_ENABLE, True);
    _SetBoolValue(ACQUISITION_LINERATE_ENABLE, True, msInfo);
  end;
end;

function TJdcMVSHik.SetAutoExposureTimeUpperLimit(AValue: Double): Integer;
begin
  result := _SetIntValue(AUTO_EXPOSURETIME_UPPER_LIMIT, Trunc(AValue));
end;

procedure TJdcMVSHik.SetGain(AValue: Double);
var
  FloatValue: Single;
begin
  if AValue = 0 then
    FloatValue := HIK_GAIN_MAX
  else
    FloatValue := Min(AValue, HIK_GAIN_MAX);

  _SetFloatValue(GAIN_RAW, FloatValue);
  m_nRet := GetFloatValue(m_hDevHandle, GAIN_RAW, @FloatValue);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, GAIN_RAW, '[%s] %0.2f', [GetCameraName, FloatValue]);
end;

end.
