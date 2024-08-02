unit JdcMVS.Abstract;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  System.Types, Winapi.Windows, System.AnsiStrings, JdcGPS, Vcl.Graphics, System.TypInfo,
  System.Math, Vcl.Imaging.jpeg, JdcView, JdcRoadMark, DateUtils, JdcMvImage;

type
  EMV_E_NETER = class(Exception);

  TCameraVendor = (cvHik, cvBasler);
  TCameraType = (ctLinescan, ctArea);

  TOnMV_Image = procedure(const ACount: Integer; const AImage: TJdcMvImage; const ATime: TDateTime) of object;

  TOnMV_ImageCallback = procedure(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX; pUser: Pointer)
    of object;

  TNameImage = record
    Name: string;
    Image: TStream;
  end;

  TRawData = record
    ImageTime: TDateTime;
    GPS: TGPSData;
    ImageCount: Integer;
    Bitmap: TBitmap; // 영상 처리용 Bitmap
    jpeg: TStream; // 이미지 저장용 MemoryStream
    RoadMark: TRoadMark;
    LaneInfo: TLaneInfo;
    AirTemp: Double; // 외기온도

    procedure ImageFree;
    function GetFileName(const VehicleCode: string; const ImageType: string; const ARoadMark: TRoadMark;
      AExt: string = '.jpg'): string;
  end;

  TJdcMVSAbstract = class
  strict private
    FImageCount: UInt32;

    FSensorSize: Double;
    FFocalLength: Integer;
    FWorkingDist: Integer;
    FImageWidth: Integer;

    FResolution: Double;
    FMinLineRate: Integer;
    FMaxLineRate: Integer;

    FOldLineRateFactor: Double;
    FCurrentLineRateFactor: Double;

    FOldSpeed: Double;
    FCurrentSpeed: Double;

    FImageHeight: Integer;

    FOnMV_Image: TOnMV_Image;
    FSaveImageType: MV_SAVE_IMAGE_TYPE;
    FImageFlip: MV_IMG_FLIP_TYPE;
    FImageRotate: MV_IMG_ROTATION_ANGLE;

    FLiveWindow: HWND;
    FDisplayWindow: HWND;

    FStopProcedure: Integer;

    procedure InitBuffer;
    function CalcResolution: Double;
  protected
    FCameraVendor: TCameraVendor;
    FCameraType: TCameraType;
    FCameraModel: string;
    FExposureMode: MV_CAM_EXPOSURE_AUTO_MODE;

    m_nRet: Integer; // en:Error code

    m_hDevHandle: PPointer;
    m_nBufSizeForSaveImage: Cardinal;
    m_pBufForDriver: PAnsiChar;
    m_nBufSizeForDriver: Cardinal;
    m_pBufForFlip: PAnsiChar;
    m_nBufSizeForFlip: Cardinal;
    m_pBufForRotate: PAnsiChar;
    m_nBufSizeForRotate: Cardinal;

    function GetAcquisitionLineRate: Integer; virtual; abstract;
    function GetExposureTime: Integer; virtual; abstract;
    function GetAutoExposureTimeUpperLimit: Integer; virtual; abstract;

    function SetExposureTime(const AValue: Double): Integer; virtual; abstract;
    function SetAcquisitionLineRate(const AValue: Double): Integer; virtual; abstract;

    function _SetIntValue(const Name: string; const Value: Cardinal;
      const MsgType: TMessageType = msSystem): Integer;
    function _SetBoolValue(const Name: string; const Value: Bool;
      const MsgType: TMessageType = msSystem): Integer;
    function _SetFloatValue(const Name: string; const Value: Single;
      const MsgType: TMessageType = msSystem): Integer;
    function _SetEnumValue(const Name: string; const Value: Cardinal; const TypeInfo: PTypeInfo;
      const MsgType: TMessageType = msSystem): Integer;

    function FlipImage(const AParam: MV_IMG_PARAM_BASE; const FlipType: MV_IMG_FLIP_TYPE)
      : MV_CC_FLIP_IMAGE_PARAM;
    function RotateImage(const AParam: MV_IMG_PARAM_BASE; const RotateAngle: MV_IMG_ROTATION_ANGLE)
      : MV_CC_ROTATE_IMAGE_PARAM;
    function SaveImageEx2(const AParam: MV_IMG_PARAM_BASE; const AImageType: MV_SAVE_IMAGE_TYPE)
      : MV_SAVE_IMAGE_PARAM_EX;

    function BufToImage(const AParam: MV_IMG_PARAM_BASE): TJdcMvImage;

    function GetCameraName: string;
    function GetVendorName: string; virtual; abstract;
  public
    class function CreateCamera(const AVendor: string; const AModel: string; const ACamType: TCameraType;
      const AImageType: MV_SAVE_IMAGE_TYPE = MV_Image_Jpeg; const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
      const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE): TJdcMVSAbstract;

    constructor Create(const AModel: string; const AType: MV_SAVE_IMAGE_TYPE;
      const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
      const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE); virtual;
    destructor Destroy; override;

    procedure Open(AHandle: PPointer);
    procedure Close;

    procedure CheckSpeedChanged;

    procedure SetPacketSize(const InterPacketDelay: Integer = 400);
    procedure SetAutoConfig(const GainAuto: MV_CAM_GAIN_MODE = MV_GAIN_MODE_OFF;
      const ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE = MV_EXPOSURE_AUTO_MODE_CONTINUOUS); virtual;
    procedure SetGain(const AValue: Double); virtual; abstract;
    procedure SetDigitalShift(const AValue: Double); virtual;
    procedure SetPreAMPGain(const AValue: MV_PREAMP_GAIN = MV_PREAMP_GAIN_1000X); virtual;

    procedure SetImageFormat(const AHeight: Integer; const AWidth: Integer; const AOffsetX: Integer;
      const AReverseX: Boolean);

    function SetAutoExposureTimeUpperLimit(const AValue: Double): Integer; virtual; abstract;

    procedure StartGrabbing; virtual;
    procedure StopGrab;

    procedure SetDisplayWindow(AHandle: HWND);
    procedure SetLiveWindow(AHandle: HWND);

    function GetOneFrame(out ATime: TDateTime; const TimeOut: Integer = 1000): TJdcMvImage; overload;

    procedure ImageCallBack(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX);

    property SensorSize: Double read FSensorSize write FSensorSize;
    property FocalLength: Integer read FFocalLength write FFocalLength;
    property WorkingDist: Integer read FWorkingDist write FWorkingDist;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property ImageHeight: Integer read FImageHeight write FImageHeight;
    property CurrentLineRateFactor: Double read FCurrentLineRateFactor write FCurrentLineRateFactor;
    property CurrentSpeed: Double read FCurrentSpeed write FCurrentSpeed;
    property CameraType: TCameraType read FCameraType write FCameraType;
    property ImageCount: UInt32 read FImageCount;
    property SaveImageType: MV_SAVE_IMAGE_TYPE read FSaveImageType;
    property ImageFlip: MV_IMG_FLIP_TYPE read FImageFlip;
    property ImageRotate: MV_IMG_ROTATION_ANGLE read FImageRotate;
    property Resolution: Double read FResolution;
    property OnMV_Image: TOnMV_Image read FOnMV_Image write FOnMV_Image;
    property CameraModel: String read FCameraModel;
  end;

const
  HIK_GAIN_MAX = 11;
  BASLER_GAIN_MAX = 2000;
  LOW_LIMIT_SPEED = 20;

  HIK_DIGITAL_SHIFT_MAX_AREA = 6;
  HIK_DIGITAL_SHIFT_MAX_LINESCAN = 23;

  CAMERA_NORMAL = 0;
  CAMERA_ACTIVE = 1;
  CAMERA_STOP_QUERY = 2;
  CAMERA_STOP_OK = 3;

implementation

uses JdcMVS.Basler, JdcMVS.Hik;

const
  GAIN_AUTO = 'GainAuto';
  EXPOSURE_AUTO = 'ExposureAuto';
  REVERSE_X = 'ReverseX';
  IMAGE_HEIGHT = 'Height';
  IMAGE_WIDTH = 'Width';
  OFFSET_X = 'OffsetX';
  GEV_SCPS_PACKET_SIZE = 'GevSCPSPacketSize';
  GEV_SCPD = 'GevSCPD';
  { TJdcMVSAbstract }

function TJdcMVSAbstract.BufToImage(const AParam: MV_IMG_PARAM_BASE): TJdcMvImage;
var
  MyParam: MV_IMG_PARAM_BASE;
  stDisplayInof: MV_DISPLAY_FRAME_INFO;
  stSaveParam: MV_SAVE_IMAGE_PARAM_EX;
begin
  MyParam := AParam;
  if (FImageFlip = MV_FLIP_VERTICAL) or (FImageFlip = MV_FLIP_BOTH) then
  begin
    MyParam := FlipImage(MyParam, MV_FLIP_VERTICAL).ToParamBase;
  end;

  if (FImageFlip = MV_FLIP_HORIZONTAL) or (FImageFlip = MV_FLIP_BOTH) then
    MyParam := FlipImage(MyParam, MV_FLIP_HORIZONTAL).ToParamBase;

  // Rotate
  if FImageRotate <> MV_IMAGE_ROTATE_NONE then
    MyParam := RotateImage(MyParam, FImageRotate).ToParamBase;

  if FLiveWindow > 0 then
  begin
    stDisplayInof := MyParam.ToDisplayInfo(FLiveWindow);
    MV_CC_DisplayOneFrame(m_hDevHandle^, stDisplayInof);
  end;

  result := TJdcMvImage.Create;

  // Save Jpeg
  stSaveParam := SaveImageEx2(MyParam, MV_Image_Jpeg);
  try
    result.WriteJpeg(stSaveParam.pImageBuffer^, stSaveParam.nImageLen);
  finally
    System.AnsiStrings.StrDispose(stSaveParam.pImageBuffer);
  end;

  // Save Bmp
  stSaveParam := SaveImageEx2(MyParam, MV_Image_Bmp);
  try
    result.WriteBitmap(stSaveParam.pImageBuffer^, stSaveParam.nImageLen);
  finally
    System.AnsiStrings.StrDispose(stSaveParam.pImageBuffer);
  end;
end;

function TJdcMVSAbstract.CalcResolution: Double;
var
  Angle, PixelSize, Magnification: Double;
begin
  // 화각
  Angle := FSensorSize * FWorkingDist / FFocalLength;

  // 광학배율
  Magnification := FSensorSize / Angle;

  // 픽셀크기
  PixelSize := FSensorSize * 1000 / FImageWidth;

  result := PixelSize / Magnification;
  TLogging.Obj.ApplicationMessage(msInfo, 'PixelResolution',
    '[%s] %0.4fmm,Angle=%0.4f,Magnific=%0.4f,PixelSize=%0.4f', [FCameraModel, result / 1000, Angle,
    Magnification, PixelSize]);
end;

procedure TJdcMVSAbstract.CheckSpeedChanged;
var
  LineRate, ExpTime, tmp: Integer;
  rlt: Integer;
const
  EXPTIME_OFFSET = 7;
begin
  if FCameraType <> ctLinescan then
    Exit;

  if FResolution = 0 then
    Exit;

  if (FOldSpeed = FCurrentSpeed) and (FOldLineRateFactor = FCurrentLineRateFactor) then
    Exit;

  FOldSpeed := FCurrentSpeed;
  FOldLineRateFactor := FCurrentLineRateFactor;

  tmp := Trunc(FOldSpeed * Power(10, 9) / 3600 / FResolution);
  LineRate := Min(tmp, FMaxLineRate); // 150km/h
  LineRate := Max(LineRate, FMinLineRate); // 10km/h

  LineRate := Trunc(LineRate + (LineRate * FOldLineRateFactor));
  // TLogging.Obj.ApplicationMessage(msDebug, 'LineRate',
  // 'Speed=%0.5f,factor=%0.3f,ori=%d,LineRate=%d', [FOldSpeed, FOldLineRateFactor, tmp, LineRate]);

  if FCameraType = ctLinescan then
  begin
    rlt := SetAcquisitionLineRate(LineRate);
    if rlt = Integer(MV_E_NETER) then
      raise EMV_E_NETER.Create('Network error')
    else if rlt <> MV_OK then
      Exit
  end;

  ExpTime := Trunc(1 / LineRate * Power(10, 6) - EXPTIME_OFFSET);
  if FExposureMode = MV_EXPOSURE_AUTO_MODE_CONTINUOUS then
    SetAutoExposureTimeUpperLimit(ExpTime)
  else
    SetExposureTime(ExpTime);
end;

procedure TJdcMVSAbstract.Close;
begin
  try
    m_nRet := MV_CC_CloseDevice(m_hDevHandle^);
    m_nRet := MV_CC_DestroyHandle(m_hDevHandle^);
    TLogging.Obj.ApplicationMessage(msDebug, 'Camera', '[%s] Closed', [FCameraModel]);
  finally
    m_hDevHandle := nil;
  end;
end;

constructor TJdcMVSAbstract.Create(const AModel: string; const AType: MV_SAVE_IMAGE_TYPE;
  const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE; const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE);
begin
  FLiveWindow := 0;
  FDisplayWindow := 0;
  FCameraModel := AModel;
  FImageWidth := 2048;
  FImageHeight := 1024;

  FOldLineRateFactor := 1;
  FOldSpeed := 0;

  m_hDevHandle := nil;
  m_nBufSizeForDriver := 0;
  m_nBufSizeForSaveImage := 0;

  FSaveImageType := AType;
  FImageFlip := AFlip;
  FImageRotate := ARotate;
  FOnMV_Image := nil;

  FStopProcedure := CAMERA_NORMAL;

  TLogging.Obj.ApplicationMessage(msInfo, 'SaveImageType', '[%s] %s',
    [FCameraModel, GetEnumName(TypeInfo(MV_SAVE_IMAGE_TYPE), Integer(AType))]);
  TLogging.Obj.ApplicationMessage(msInfo, 'ImageFlip', '[%s] %s',
    [FCameraModel, GetEnumName(TypeInfo(MV_IMG_FLIP_TYPE), Integer(AFlip))]);
  TLogging.Obj.ApplicationMessage(msInfo, 'ImageRotate', '[%s] %s',
    [FCameraModel, GetEnumName(TypeInfo(MV_IMG_ROTATION_ANGLE), Integer(ARotate))]);
end;

class function TJdcMVSAbstract.CreateCamera(const AVendor: string; const AModel: string;
  const ACamType: TCameraType; const AImageType: MV_SAVE_IMAGE_TYPE = MV_Image_Jpeg;
  const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE; const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE)
  : TJdcMVSAbstract;
begin
  if AVendor = TJdcMVSHik.VENDOR_NAME then
    result := TJdcMVSHik.Create(AModel, AImageType, AFlip, ARotate)
  else if AVendor = TJdcMVSBasler.VENDOR_NAME then
    result := TJdcMVSBasler.Create(AModel, AImageType, AFlip, ARotate)
  else
    raise Exception.Create('Unknown Camera Vendor [' + AVendor + ']');

  result.CameraType := ACamType;
end;

destructor TJdcMVSAbstract.Destroy;
begin
  StopGrab;

  if Assigned(m_pBufForFlip) then
    System.AnsiStrings.StrDispose(m_pBufForFlip);

  if Assigned(m_pBufForDriver) then
    System.AnsiStrings.StrDispose(m_pBufForDriver);
end;

function TJdcMVSAbstract.FlipImage(const AParam: MV_IMG_PARAM_BASE; const FlipType: MV_IMG_FLIP_TYPE)
  : MV_CC_FLIP_IMAGE_PARAM;
begin
  if nil = m_pBufForFlip then
  begin
    m_nBufSizeForFlip := AParam.nDataLen;
    m_pBufForFlip := AnsiStrAlloc(m_nBufSizeForFlip);
    if (Nil = m_pBufForFlip) or (not(System.AnsiStrings.StrBufSize(m_pBufForFlip) > 0)) then
    begin
      TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
        'malloc m_pBufForFlip failed, run out of memory.' + IntToStr(m_nBufSizeForFlip) + ', ' +
        FCameraModel);
      Exit
    end;
  end;

  ZeroMemory(@result, sizeof(MV_CC_FLIP_IMAGE_PARAM));
  result.enPixelType := AParam.enPixelType;
  result.nWidth := AParam.nWidth; // en:Width
  result.nHeight := AParam.nHeight; // en:Height
  result.pSrcData := AParam.pData;
  result.nSrcDataLen := AParam.nDataLen;

  result.pDstBuf := m_pBufForFlip;
  result.nDstBufSize := m_nBufSizeForFlip;
  result.enFlipType := FlipType;

  m_nRet := MV_CC_FlipImage(m_hDevHandle^, @result);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_FlipImage - 0x%x, %s',
      [m_nRet, GetEnumName(TypeInfo(MV_IMG_FLIP_TYPE), Integer(FlipType))]));
end;

function TJdcMVSAbstract.GetCameraName: string;
begin
  result := GetVendorName + ' ' + FCameraModel;
end;

procedure TJdcMVSAbstract.ImageCallBack(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX);
Var
  Image: TJdcMvImage;
  DateTime: TDateTime;
begin
  if not Assigned(m_hDevHandle) then
    Exit;

  // ImageCallBack이 설정된경우 StopGrab에서
  // CAMERA_STOP_OK가 설정될때 까지 대기해야한다.
  if FStopProcedure <> CAMERA_ACTIVE then
  begin
    FStopProcedure := CAMERA_STOP_OK;
    Exit;
  end;

  DateTime := Now;
  FImageCount := FImageCount + 1;
  try
    Image := BufToImage(pFrameInfo.ToParamBase(pData));
  except
    on E: Exception do
    begin
      TLogging.Obj.ApplicationMessage(msError, 'BufToImage', '[%s] %s', [Self.GetCameraName, E.Message]);
      Exit;
    end;
  end;

  try
    if Assigned(FOnMV_Image) then
      FOnMV_Image(FImageCount, Image, DateTime);
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msError, 'OnImageCallBack', '[%s] %s', [Self.GetCameraName, E.Message]);
  end;

  Image.Free;
end;

procedure TJdcMVSAbstract.InitBuffer;
var
  nRecvBufSize: Cardinal;
begin
  nRecvBufSize := 0;
  m_nRet := GetIntValue(m_hDevHandle, 'PayloadSize', @nRecvBufSize);
  if m_nRet <> MV_OK then
  begin
    TLogging.Obj.ApplicationMessage(msWarning, 'GetPayloadSizeFail', '[%s] 0x%x', [FCameraModel, m_nRet]);
    Exit
  end;
  m_nBufSizeForDriver := nRecvBufSize + 2048;
  m_pBufForDriver := AnsiStrAlloc(m_nBufSizeForDriver);

  if (Nil = m_pBufForDriver) or (not(System.AnsiStrings.StrBufSize(m_pBufForDriver) > 0)) then
  begin
    TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
      'malloc m_pBufForDriver failed, run out of memory.' + IntToStr(m_nBufSizeForDriver) + ', ' +
      FCameraModel);
    raise Exception.Create('MallocDriverFail');
  end
  else
    TLogging.Obj.ApplicationMessage(msInfo, 'PayloadSize', '[%s] %d', [FCameraModel, m_nBufSizeForDriver]);
end;

procedure TJdcMVSAbstract.Open(AHandle: PPointer);
begin
  m_hDevHandle := AHandle;

  FImageCount := 0;

  if FCameraType = ctLinescan then
  begin
    FResolution := CalcResolution;
    if FResolution = 0 then
      raise Exception.Create('Resolution Error[0]');

    FMinLineRate := Trunc(10 * Power(10, 9) / 3600 / FResolution); // 10km/h
    FMaxLineRate := Trunc(150 * Power(10, 9) / 3600 / FResolution); // 150km/h
  end;
end;

function TJdcMVSAbstract.RotateImage(const AParam: MV_IMG_PARAM_BASE;
  const RotateAngle: MV_IMG_ROTATION_ANGLE): MV_CC_ROTATE_IMAGE_PARAM;
begin
  if nil = m_pBufForRotate then
  begin
    m_nBufSizeForRotate := AParam.nDataLen;
    m_pBufForRotate := AnsiStrAlloc(m_nBufSizeForRotate);
    if (Nil = m_pBufForRotate) or (not(System.AnsiStrings.StrBufSize(m_pBufForRotate) > 0)) then
    begin
      TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
        'malloc m_pBufForRotate failed, run out of memory.' + IntToStr(m_nBufSizeForRotate) + ', ' +
        FCameraModel);
      Exit
    end;
  end;

  ZeroMemory(@result, sizeof(MV_CC_ROTATE_IMAGE_PARAM));
  result.enPixelType := AParam.enPixelType;
  result.nWidth := AParam.nWidth; // en:Width
  result.nHeight := AParam.nHeight; // en:Height
  result.pSrcData := AParam.pData;
  result.nSrcDataLen := AParam.nDataLen;

  result.pDstBuf := m_pBufForRotate;
  result.nDstBufSize := m_nBufSizeForRotate;
  result.enRotationAngle := RotateAngle;

  m_nRet := MV_CC_RotateImage(m_hDevHandle^, @result);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_RotateImage - 0x%x, %s',
      [m_nRet, GetEnumName(TypeInfo(MV_IMG_ROTATION_ANGLE), Integer(RotateAngle))]));
end;

function TJdcMVSAbstract.GetOneFrame(out ATime: TDateTime; const TimeOut: Integer): TJdcMvImage;
Var
  stOutFrame: MV_FRAME_OUT_INFO_EX;
begin
  result := nil;

  FImageCount := FImageCount + 1;

  CheckSpeedChanged; // 속도변화 확인
  ZeroMemory(@stOutFrame, sizeof(MV_FRAME_OUT_INFO_EX));
  m_nRet := MV_CC_GetOneFrameTimeout(m_hDevHandle^, m_pBufForDriver, m_nBufSizeForDriver,
    @stOutFrame, TimeOut);
  if m_nRet <> MV_OK then
  begin
    // raise Exception.Create(Format('GetOneFrame,[%s] Code=0x%x', [FCameraModel, m_nRet]));
    ATime := 0;
    Exit;
  end
  else
    ATime := Now;

  result := BufToImage(stOutFrame.ToParamBase(m_pBufForDriver));
end;

function TJdcMVSAbstract.SaveImageEx2(const AParam: MV_IMG_PARAM_BASE; const AImageType: MV_SAVE_IMAGE_TYPE)
  : MV_SAVE_IMAGE_PARAM_EX;
var
  pBufForSaveImage: PAnsiChar;
begin
  if m_nBufSizeForSaveImage = 0 then
  begin
    // en:BMP image size: width * height * 3 + 2048 (Reserved BMP header size)
    m_nBufSizeForSaveImage := AParam.nWidth * AParam.nHeight * 3 + 2048;
    TLogging.Obj.ApplicationMessage(msInfo, 'SaveImageSize', '[%s] %d',
      [FCameraModel, m_nBufSizeForSaveImage]);
  end;

  pBufForSaveImage := AnsiStrAlloc(m_nBufSizeForSaveImage);
  // en:Set camera parameter
  ZeroMemory(@result, sizeof(MV_SAVE_IMAGE_PARAM_EX));
  result.enImageType := AImageType;
  // en:Image format to save
  result.enPixelType := AParam.enPixelType; // en:Camera pixel type
  result.nWidth := AParam.nWidth; // en:Width
  result.nHeight := AParam.nHeight; // en:Height
  result.pData := AParam.pData;
  result.nDataLen := AParam.nDataLen;

  result.pImageBuffer := pBufForSaveImage;
  result.nBufferSize := m_nBufSizeForSaveImage;
  // en:Buffer node size

  if AImageType = MV_Image_Jpeg then
    result.nJpgQuality := 70;

  // en:jpg encoding, only valid when saving as Jpg. SDK ignore this parameter when saving as BMP
  m_nRet := MV_CC_SaveImageEx2(m_hDevHandle^, @result);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_SaveImageEx2 - 0x%x', [m_nRet]));
end;

procedure TJdcMVSAbstract.SetAutoConfig(const GainAuto: MV_CAM_GAIN_MODE;
  const ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE);
begin
  FExposureMode := ExposureAuto;
  _SetEnumValue(EXPOSURE_AUTO, Integer(FExposureMode), TypeInfo(MV_CAM_EXPOSURE_AUTO_MODE), msInfo);

  // Hik중에서는 MV-CL021-40GM 모델만
  if ((FCameraModel = LINE_SCAN_2K_1PX)) or (FCameraVendor = cvBasler) then
    _SetEnumValue(GAIN_AUTO, Integer(GainAuto), TypeInfo(MV_CAM_GAIN_MODE), msInfo);
end;

procedure TJdcMVSAbstract.SetDigitalShift(const AValue: Double);
var
  FloatValue: Single;
begin
  _SetBoolValue(TJdcMVSHik.DIGITAL_SHIFT_ENABLE, True, msInfo);

  FloatValue := AValue;
  _SetFloatValue(TJdcMVSHik.DIGITAL_SHIFT, FloatValue);
  m_nRet := GetFloatValue(m_hDevHandle, TJdcMVSHik.DIGITAL_SHIFT, @FloatValue);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, TJdcMVSHik.DIGITAL_SHIFT, '[%s] %0.2f',
      [GetCameraName, FloatValue]);
end;

procedure TJdcMVSAbstract.SetDisplayWindow(AHandle: HWND);
begin
  if FDisplayWindow = AHandle then
    Exit;

  FDisplayWindow := AHandle;
  m_nRet := MV_CC_Display(m_hDevHandle^, FDisplayWindow);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_Display - 0x%x', [m_nRet]));
end;

procedure TJdcMVSAbstract.SetImageFormat(const AHeight: Integer; const AWidth: Integer;
  const AOffsetX: Integer; const AReverseX: Boolean);
begin
  _SetBoolValue(REVERSE_X, AReverseX, msInfo);
  _SetIntValue(IMAGE_HEIGHT, AHeight, msInfo);
  _SetIntValue(OFFSET_X, 0); // Offset 초기화
  _SetIntValue(IMAGE_WIDTH, AWidth, msInfo);
  _SetIntValue(OFFSET_X, AOffsetX, msInfo);
end;

procedure TJdcMVSAbstract.SetLiveWindow(AHandle: HWND);
begin
  if (FLiveWindow = AHandle) then
    Exit;
  FLiveWindow := AHandle;
end;

procedure TJdcMVSAbstract.SetPacketSize(const InterPacketDelay: Integer);
var
  nPacketSize: Integer;
begin
  nPacketSize := MV_CC_GetOptimalPacketSize(m_hDevHandle^);
  if nPacketSize > 0 then
    _SetIntValue(GEV_SCPS_PACKET_SIZE, nPacketSize, msInfo);
  _SetIntValue(GEV_SCPD, InterPacketDelay, msInfo);
end;

procedure TJdcMVSAbstract.SetPreAMPGain(const AValue: MV_PREAMP_GAIN);
var
  Value: Integer;
begin
  case AValue of
    MV_PREAMP_GAIN_1000X:
      Value := 1000;
    MV_PREAMP_GAIN_1400X:
      Value := 1400;
    MV_PREAMP_GAIN_1600X:
      Value := 1600;
    MV_PREAMP_GAIN_2400X:
      Value := 2400;
    MV_PREAMP_GAIN_3200X:
      Value := 3200;
  else
    Value := 2400;
  end;

  m_nRet := SetEnumValue(m_hDevHandle, TJdcMVSHik.PREAMP_GAIN, Value);
  if m_nRet = MV_OK then
  begin
    TLogging.Obj.ApplicationMessage(msInfo, TJdcMVSHik.PREAMP_GAIN, '[%s] %s',
      [FCameraModel, GetEnumName(TypeInfo(MV_PREAMP_GAIN), Integer(AValue))])
  end
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + TJdcMVSHik.PREAMP_GAIN,
      '[%s] ErrorCode=0x%x,Value=%s', [FCameraModel, m_nRet, GetEnumName(TypeInfo(MV_PREAMP_GAIN),
      Integer(AValue))])

end;

procedure ProcImageCallBack(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX; pUser: Pointer)stdcall;
begin
  try
    TJdcMVSAbstract(pUser).ImageCallBack(pData, pFrameInfo);
  except
    on E: Exception do
      TLogging.Obj.ApplicationMessage(msError, 'OnMVImageCallback', '[%s] %s',
        [TJdcMVSAbstract(pUser).FCameraModel, E.Message]);
  end;
end;

procedure TJdcMVSAbstract.StartGrabbing;
begin
  FImageCount := 0;

  if Assigned(FOnMV_Image) then
  begin
    m_nRet := MV_CC_RegisterImageCallBackEx(m_hDevHandle^, ProcImageCallBack, Self);
    if m_nRet = MV_OK then
      TLogging.Obj.ApplicationMessage(msInfo, 'ImageCallBackEx', '[%s] Registered', [FCameraModel])
    else
      TLogging.Obj.ApplicationMessage(msWarning, 'RegisterImageCallBackEx', '[%s] Code=0x%x',
        [FCameraModel, m_nRet]);
  end
  else
    InitBuffer;

  CheckSpeedChanged;
  m_nRet := MV_CC_StartGrabbing(m_hDevHandle^);
  if m_nRet = MV_OK then
  begin
    FStopProcedure := CAMERA_ACTIVE;
    TLogging.Obj.ApplicationMessage(msInfo, 'Grabbing', '[%s] Started!', [FCameraModel]);
  end
  else
    raise Exception.Create(Format('StartGrabbingFail,[%s] Code=0x%x', [FCameraModel, m_nRet]))
end;

procedure TJdcMVSAbstract.StopGrab;
var
  counter: Integer;
begin
  if not Assigned(m_hDevHandle) then
    Exit;

  // ImageCallBack이 등록된경우
  if Assigned(FOnMV_Image) then
  begin
    // CAMERA_STOP_QUERY를 설정하고
    FStopProcedure := CAMERA_STOP_QUERY;
    counter := 0;

    // CAMERA_STOP_OK까지 대기, 최대 3초
    while (FStopProcedure = CAMERA_STOP_QUERY) and (counter < 300) do
    begin
      sleep(10);
      inc(counter);
    end;
  end;

  m_nRet := MV_CC_StopGrabbing(m_hDevHandle^);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, 'StopGrbbing', '[%s] ImageCount=%d', [FCameraModel, FImageCount])
  else
    TLogging.Obj.ApplicationMessage(msError, 'StopGrabbingFail', '[%s] Code=0x%x', [FCameraModel, m_nRet]);
end;

function TJdcMVSAbstract._SetBoolValue(const Name: string; const Value: Bool;
  const MsgType: TMessageType): Integer;
begin
  result := SetBoolValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%s',
      [FCameraModel, result, BoolToStr(Value)])
  else if MsgType <> msSystem then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %s', [FCameraModel, BoolToStr(Value)])

end;

function TJdcMVSAbstract._SetEnumValue(const Name: string; const Value: Cardinal; const TypeInfo: PTypeInfo;
  const MsgType: TMessageType): Integer;
begin
  result := SetEnumValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%s',
      [FCameraModel, result, GetEnumName(TypeInfo, Value)])
  else if MsgType <> msSystem then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %s', [FCameraModel, GetEnumName(TypeInfo, Value)])
end;

function TJdcMVSAbstract._SetFloatValue(const Name: string; const Value: Single;
  const MsgType: TMessageType): Integer;
begin
  result := SetFloatValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%0.2f',
      [FCameraModel, result, Value])
  else if MsgType <> msSystem then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %0.2f', [FCameraModel, Value])
end;

function TJdcMVSAbstract._SetIntValue(const Name: string; const Value: Cardinal;
  const MsgType: TMessageType): Integer;
begin
  result := SetIntValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%d',
      [FCameraModel, result, Value])
  else if MsgType <> msSystem then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %d', [FCameraModel, Value])
end;

{ TRawData }

function TRawData.GetFileName(const VehicleCode: string; const ImageType: string; const ARoadMark: TRoadMark;
  AExt: string): string;
var
  lat, lon: string;
  AirTempName: string;
begin
  // 20230927_095317.857_002468_37.4728944_127.1259029_4_1000_S_009.42_02_03_075_21_095317.811_N00225_UD000_V.jpg
  lat := FormatFloat('000.0000000', GPS.latitude);
  lon := FormatFloat('000.0000000', GPS.longitude);

  if AirTemp < 0 then
    AirTempName := Format('%0.2d', [Round(AirTemp)])
  else
    AirTempName := Format('%0.3d', [Round(AirTemp)]);

  result := FormatDateTime('YYYYMMDD_HHNNSS.ZZZ_', ImageTime) + Format('%0.6d_%s_%s_%d_%s_%s_%s_%0.2d_%0.2d',
    [ImageCount, lat, lon, GPS.Quality, ARoadMark.road_code, ARoadMark.GetRoadDirection,
    ARoadMark.GetMarkName, LaneInfo.Lane, LaneInfo.Total]);
  result := result + Format('_%0.3d_%s_%s_%s_%s_%s.jpg', [Round(GPS.Speed), AirTempName,
    FormatDateTime('HHNNSS.ZZZ', GPS.PCTime), ARoadMark.GetBranchCode, VehicleCode, ImageType, AExt]);
end;

procedure TRawData.ImageFree;
begin
  FreeAndNilEx(Self.Bitmap);
  FreeAndNilEx(Self.jpeg);
end;

end.
