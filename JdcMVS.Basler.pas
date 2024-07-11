unit JdcMVS.Basler;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  JdcMVS.Abstract, System.Math;

type
  TJdcMVSBasler = class(TJdcMVSAbstract)
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
    procedure SetGain(const AValue: Double); override;
    function SetAutoExposureTimeUpperLimit(const AValue: Double): Integer; override;

  const
    VENDOR_NAME = 'Basler';
  end;

implementation

const
  GAIN_RAW = 'GainRaw';
  ACQUISITION_LINERATE = 'AcquisitionLineRateAbs';
  AUTO_EXPOSURETIME_UPPER_LIMIT = 'AutoExposureTimeAbsUpperLimit';
  EXPOSURE_TIME = 'ExposureTimeAbs';

  { TJdcMVSBasler }

constructor TJdcMVSBasler.Create(const AModel: string; const AType: MV_SAVE_IMAGE_TYPE;
  const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
  const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE);
begin
  inherited;
  FCameraVendor := cvBasler;
end;

function TJdcMVSBasler.GetAcquisitionLineRate: Integer;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, ACQUISITION_LINERATE, @FloatValue);
  result := Trunc(FloatValue);
end;

function TJdcMVSBasler.GetAutoExposureTimeUpperLimit: Integer;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, AUTO_EXPOSURETIME_UPPER_LIMIT, @FloatValue);
  result := Trunc(FloatValue);
end;

function TJdcMVSBasler.GetExposureTime: Integer;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, EXPOSURE_TIME, @FloatValue);
  result := Trunc(FloatValue);
end;

function TJdcMVSBasler.GetVendorName: string;
begin
  result := VENDOR_NAME;
end;

function TJdcMVSBasler.SetAcquisitionLineRate(const AValue: Double): Integer;
begin
  if FCameraType = ctLinescan then
    result := _SetFloatValue(ACQUISITION_LINERATE, AValue)
  else
    result := MV_E_SUPPORT;
end;

function TJdcMVSBasler.SetAutoExposureTimeUpperLimit(const AValue: Double): Integer;
begin
  if FExposureMode = MV_EXPOSURE_AUTO_MODE_CONTINUOUS then
    result := _SetFloatValue(AUTO_EXPOSURETIME_UPPER_LIMIT, AValue)
  else
    result := MV_E_SUPPORT;
end;

function TJdcMVSBasler.SetExposureTime(const AValue: Double): Integer;
begin
  if FExposureMode = MV_EXPOSURE_AUTO_MODE_OFF then
    result := _SetFloatValue(EXPOSURE_TIME, AValue)
  else
    result := MV_E_SUPPORT;
end;

procedure TJdcMVSBasler.SetGain(const AValue: Double);
var
  IntValue: Integer;
begin
  IntValue := Min(Trunc(AValue), BASLER_GAIN_MAX);
  _SetIntValue(GAIN_RAW, IntValue);
  m_nRet := GetIntValue(m_hDevHandle, GAIN_RAW, @IntValue);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, GAIN_RAW, '[%s] %d', [GetCameraName, IntValue]);
end;

end.
