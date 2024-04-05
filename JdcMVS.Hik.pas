unit JdcMVS.Hik;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  JdcMVS.Abstract, System.Math;

type
  TJdcMVSHik = class(TJdcMVSAbstract)
  private
  protected
    function GetVendorName: string; override;

    function GetAcquisitionLineRate: Integer; override;
    function GetExposureTime: Integer; override;
    function GetAutoExposureTimeUpperLimit: Integer; override;

    function SetExposureTime(const AValue: Double): Integer; override;
    function SetAcquisitionLineRate(const AValue: Double): Integer; override;
  public
    constructor Create(const AModel: string; const AType: MV_SAVE_IMAGE_TYPE;
      const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
      const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE); override;

    procedure SetAutoConfig(const GainAuto: MV_CAM_GAIN_MODE = MV_GAIN_MODE_OFF;
      const ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE = MV_EXPOSURE_AUTO_MODE_CONTINUOUS); override;
    procedure SetGain(const AValue: Double); override;
    function SetAutoExposureTimeUpperLimit(const AValue: Double): Integer; override;

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

constructor TJdcMVSHik.Create(const AModel: string; const AType: MV_SAVE_IMAGE_TYPE;
  const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
  const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE);
begin
  inherited;

  FCameraVendor := cvHik;
end;

function TJdcMVSHik.GetAcquisitionLineRate: Integer;
var
  IntValue: Cardinal;
begin
  GetIntValue(m_hDevHandle, ACQUISITION_LINERATE, @IntValue);
  result := IntValue;
end;

function TJdcMVSHik.GetAutoExposureTimeUpperLimit: Integer;
var
  IntValue: Cardinal;
begin
  GetIntValue(m_hDevHandle, AUTO_EXPOSURETIME_UPPER_LIMIT, @IntValue);
  result := IntValue;
end;

function TJdcMVSHik.GetExposureTime: Integer;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, EXPOSURE_TIME, @FloatValue);
  result := Trunc(FloatValue);
end;

function TJdcMVSHik.GetVendorName: string;
begin
  result := VENDOR_NAME;
end;

function TJdcMVSHik.SetAcquisitionLineRate(const AValue: Double): Integer;
begin
  if FCameraType = ctLinescan then
    result := _SetIntValue(ACQUISITION_LINERATE, Trunc(AValue))
  else
    result := MV_E_SUPPORT;
end;

procedure TJdcMVSHik.SetAutoConfig(const GainAuto: MV_CAM_GAIN_MODE;
  const ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE);
begin
  inherited;

  if FCameraType = ctLinescan then
  begin
    _SetBoolValue(ADC_GAIN_ENABLE, True);
    _SetBoolValue(ACQUISITION_LINERATE_ENABLE, True, msInfo);
  end;
end;

function TJdcMVSHik.SetAutoExposureTimeUpperLimit(const AValue: Double): Integer;
begin
  if FExposureMode = MV_EXPOSURE_AUTO_MODE_CONTINUOUS then
    result := _SetIntValue(AUTO_EXPOSURETIME_UPPER_LIMIT, Trunc(AValue))
  else
    result := MV_E_SUPPORT;
end;

function TJdcMVSHik.SetExposureTime(const AValue: Double): Integer;
begin
  if FExposureMode = MV_EXPOSURE_AUTO_MODE_OFF then
    result := _SetFloatValue(EXPOSURE_TIME, AValue)
  else
    result := MV_E_SUPPORT;
end;

procedure TJdcMVSHik.SetGain(const AValue: Double);
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
