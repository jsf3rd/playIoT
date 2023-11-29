unit JdcMVS.Basler;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  JdcMVS.Abstract, System.Math;

type
  TJdcMVSBasler = class(TJdcMVSAbstract)
  private
  protected
    function GetVendorName: string; override;
  public
    constructor Create(AModel: string); override;
    procedure SetGain(AValue: Double); override;
    function SetAcquisitionLineRate(AValue: Double): Integer; override;
    function SetAutoExposureTimeUpperLimit(AValue: Double): Integer; override;

    function GetAcquisitionLineRate: Double; override;
    function GetExposureTime: Double; override;

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

constructor TJdcMVSBasler.Create(AModel: string);
begin
  inherited;
  FCameraVendor := cvBasler;
end;

function TJdcMVSBasler.GetAcquisitionLineRate: Double;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, ACQUISITION_LINERATE, @FloatValue);
  result := FloatValue;
end;

function TJdcMVSBasler.GetExposureTime: Double;
var
  FloatValue: Single;
begin
  GetFloatValue(m_hDevHandle, EXPOSURE_TIME, @FloatValue);
  result := FloatValue;
end;

function TJdcMVSBasler.GetVendorName: string;
begin
  result := VENDOR_NAME;
end;

function TJdcMVSBasler.SetAcquisitionLineRate(AValue: Double): Integer;
begin
  result := _SetFloatValue(ACQUISITION_LINERATE, AValue);
end;

function TJdcMVSBasler.SetAutoExposureTimeUpperLimit(AValue: Double): Integer;
begin
  result := _SetFloatValue(AUTO_EXPOSURETIME_UPPER_LIMIT, AValue);
end;

procedure TJdcMVSBasler.SetGain(AValue: Double);
var
  IntValue: Integer;
begin
  if AValue = 0 then
    IntValue := BASLER_GAIN_MAX
  else
    IntValue := Min(Trunc(AValue), BASLER_GAIN_MAX);

  _SetIntValue(GAIN_RAW, IntValue);
  m_nRet := GetIntValue(m_hDevHandle, GAIN_RAW, @IntValue);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, GAIN_RAW, '[%s] %d', [GetCameraName, IntValue]);
end;

end.
