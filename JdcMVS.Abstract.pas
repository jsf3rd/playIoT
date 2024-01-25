unit JdcMVS.Abstract;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  System.Types, Winapi.Windows, System.AnsiStrings, JdcGPS, Vcl.Graphics, System.TypInfo,
  System.Math, Vcl.Imaging.jpeg, JdcView;

type
  TCameraVendor = (cvHik, cvBasler);
  TCameraType = (ctLinescan, ctArea);

  TGraphicClass = class of TGraphic;
  TOnGraphic = procedure(const ACount: Integer; const AGraphic: TGraphic; const ATime: TDateTime)
    of object;

  TOnMVImageCallback = procedure(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX)
    of object;

  TNameImage = record
    Name: string;
    Image: TStream;
  end;

  TRawData = record
    ImageTime: TDateTime;
    GPS: TGPSData;
    ImageCount: Integer;
    ImageObj: TJPEGImage;
  end;

  TJdcMVSAbstract = class
  private
    FImageCount: UInt32;

    FSensorSize: Double;
    FFocalLength: Integer;
    FWorkingDist: Integer;
    FImageWidth: Integer;

    FResolution: Double;

    FOldLineRateFactor: Double;
    FCurrentLineRateFactor: Double;

    FOldSpeed: Double;
    FCurrentSpeed: Double;

    FImageHeight: Integer;

    FOnImageCallBack: TOnGraphic;
    FSaveImageType: MV_SAVE_IMAGE_TYPE;
    FImageFlip: MV_IMG_FLIP_TYPE;
    FImageRotate: MV_IMG_ROTATION_ANGLE;

    procedure InitBuffer;
    function CalcResolution: Double;
    procedure ImageCallBack(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX);
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

    function SaveImageEx2(const AParam: MV_IMG_PARAM_BASE): MV_SAVE_IMAGE_PARAM_EX;

    function GetCameraName: string;
    function GetVendorName: string; virtual; abstract;

    function BufToImage(const AParam: MV_IMG_PARAM_BASE): TGraphic;
  public
    class function CreateCamera(const AVendor: string; const AModel: string;
      const ACamType: TCameraType; const AImageType: MV_SAVE_IMAGE_TYPE = MV_Image_Jpeg;
      const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
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
    function SetAcquisitionLineRate(const AValue: Double): Integer; virtual; abstract;
    function SetAutoExposureTimeUpperLimit(const AValue: Double): Integer; virtual; abstract;
    function SetExposureTime(const AValue: Double): Integer; virtual; abstract;

    procedure SetImageFormat(const AHeight: Integer; const AWidth: Integer; const AOffsetX: Integer;
      const AReverseX: Boolean);

    function GetAcquisitionLineRate: Integer; virtual; abstract;
    function GetExposureTime: Integer; virtual; abstract;
    function GetAutoExposureTimeUpperLimit: Integer; virtual; abstract;

    procedure StartGrabbing; virtual;
    procedure StopGrab;

    procedure SetDisplayWindow(AHandle: HWND);

    function GetOneFrame(out ATime: TDateTime; const TimeOut: Integer = 1000): TGraphic; overload;

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
    property OnImageCallBack: TOnGraphic read FOnImageCallBack write FOnImageCallBack;
  end;

const
  HIK_GAIN_MAX = 11.99;
  BASLER_GAIN_MAX = 2047;
  LOW_LIMIT_SPEED = 20;

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

var
  OnMVImageCallback: TOnMVImageCallback;

function TJdcMVSAbstract.BufToImage(const AParam: MV_IMG_PARAM_BASE): TGraphic;
var
  MyParam: MV_IMG_PARAM_BASE;
  stSaveParam: MV_SAVE_IMAGE_PARAM_EX;
  Stream: TStream;
begin
  MyParam := AParam;
  if (FImageFlip = MV_FLIP_VERTICAL) or (FImageFlip = MV_FLIP_BOTH) then
    MyParam := FlipImage(MyParam, MV_FLIP_VERTICAL).ToParamBase;

  if (FImageFlip = MV_FLIP_HORIZONTAL) or (FImageFlip = MV_FLIP_BOTH) then
    MyParam := FlipImage(MyParam, MV_FLIP_HORIZONTAL).ToParamBase;

  // Rotate
  if FImageRotate <> MV_IMAGE_ROTATE_NONE then
    MyParam := RotateImage(MyParam, FImageRotate).ToParamBase;

  // Save Image
  stSaveParam := SaveImageEx2(MyParam);
  try
    Stream := TMemoryStream.Create;
    try
      Stream.Write(stSaveParam.pImageBuffer^, stSaveParam.nImageLen);
      Stream.Position := 0;
      if FSaveImageType = MV_Image_Bmp then
        result := TBitmap.Create
      else if FSaveImageType = MV_Image_Jpeg then
        result := TJPEGImage.Create
      else
        raise Exception.Create('Not surpported image types');
      result.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
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
  TLogging.Obj.ApplicationMessage(msInfo, 'PixelResolution', '%0.4fmm', [result / 1000]);
end;

procedure TJdcMVSAbstract.CheckSpeedChanged;
var
  LineRate, ExpTime, tmp: Integer;
  rlt: Integer;
const
  EXPTIME_OFFSET = 7;
begin

  if FResolution = 0 then
    exit;

  if (FOldSpeed = FCurrentSpeed) and (FOldLineRateFactor = FCurrentLineRateFactor) then
    exit;

  FOldSpeed := FCurrentSpeed;
  FOldLineRateFactor := FCurrentLineRateFactor;

  tmp := Trunc(FOldSpeed * Power(10, 9) / 3600 / FResolution);
  LineRate := Min(tmp, 20000);
  LineRate := Max(LineRate, 100);

  LineRate := Trunc(LineRate + (LineRate * FOldLineRateFactor));
  // TLogging.Obj.ApplicationMessage(msDebug, 'LineRate',
  // 'Speed=%0.5f,factor=%0.3f,ori=%d,LineRate=%d', [FOldSpeed, FOldLineRateFactor, tmp, LineRate]);
  rlt := SetAcquisitionLineRate(LineRate);
  if rlt <> MV_OK then
    exit;

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
    TLogging.Obj.ApplicationMessage(msDebug, 'Camera', 'Closed, ' + GetCameraName);
  finally
    m_hDevHandle := nil;
  end;
end;

constructor TJdcMVSAbstract.Create(const AModel: string; const AType: MV_SAVE_IMAGE_TYPE;
  const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
  const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE);
begin
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
  OnMVImageCallback := ImageCallBack;

  TLogging.Obj.ApplicationMessage(msInfo, 'SaveImageType', '[%s] %s',
    [GetCameraName, GetEnumName(TypeInfo(MV_SAVE_IMAGE_TYPE), Integer(AType))]);
  TLogging.Obj.ApplicationMessage(msInfo, 'ImageFlip', '[%s] %s',
    [GetCameraName, GetEnumName(TypeInfo(MV_IMG_FLIP_TYPE), Integer(AFlip))]);
  TLogging.Obj.ApplicationMessage(msInfo, 'ImageRotate', '[%s] %s',
    [GetCameraName, GetEnumName(TypeInfo(MV_IMG_ROTATION_ANGLE), Integer(ARotate))]);
end;

class function TJdcMVSAbstract.CreateCamera(const AVendor: string; const AModel: string;
  const ACamType: TCameraType; const AImageType: MV_SAVE_IMAGE_TYPE = MV_Image_Jpeg;
  const AFlip: MV_IMG_FLIP_TYPE = MV_FLIP_NONE;
  const ARotate: MV_IMG_ROTATION_ANGLE = MV_IMAGE_ROTATE_NONE): TJdcMVSAbstract;
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

function TJdcMVSAbstract.FlipImage(const AParam: MV_IMG_PARAM_BASE;
  const FlipType: MV_IMG_FLIP_TYPE): MV_CC_FLIP_IMAGE_PARAM;
begin
  if nil = m_pBufForFlip then
  begin
    m_nBufSizeForFlip := AParam.nDataLen;
    m_pBufForFlip := AnsiStrAlloc(m_nBufSizeForFlip);
    if (Nil = m_pBufForFlip) or (not(System.AnsiStrings.StrBufSize(m_pBufForFlip) > 0)) then
    begin
      TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
        'malloc m_pBufForFlip failed, run out of memory.' + IntToStr(m_nBufSizeForFlip) + ', ' +
        GetCameraName);
      exit
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
    raise Exception.Create(Format('MV_CC_FlipImage - 0x%x', [m_nRet]));
end;

function TJdcMVSAbstract.GetCameraName: string;
begin
  result := GetVendorName + ' ' + FCameraModel;
end;

procedure TJdcMVSAbstract.ImageCallBack(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX);
Var
  Image: TGraphic;
  DateTime: TDateTime;
begin
  DateTime := Now;
  FImageCount := FImageCount + 1;
  Image := BufToImage(pFrameInfo.ToParamBase(pData));
  OnImageCallBack(FImageCount, Image, DateTime);
  if FCameraType = ctLinescan then
    CheckSpeedChanged;
end;

procedure TJdcMVSAbstract.InitBuffer;
var
  nRecvBufSize: Cardinal;
begin
  nRecvBufSize := 0;
  m_nRet := GetIntValue(m_hDevHandle, 'PayloadSize', @nRecvBufSize);
  if m_nRet <> MV_OK then
  begin
    TLogging.Obj.ApplicationMessage(msWarning, 'GetPayloadSizeFail', '[%s] 0x%x',
      [GetCameraName, m_nRet]);
    exit
  end;
  m_nBufSizeForDriver := nRecvBufSize + 2048;
  m_pBufForDriver := AnsiStrAlloc(m_nBufSizeForDriver);

  if (Nil = m_pBufForDriver) or (not(System.AnsiStrings.StrBufSize(m_pBufForDriver) > 0)) then
  begin
    TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
      'malloc m_pBufForDriver failed, run out of memory.' + IntToStr(m_nBufSizeForDriver) + ', ' +
      GetCameraName);
    raise Exception.Create('MallocDriverFail');
  end
  else
    TLogging.Obj.ApplicationMessage(msInfo, 'PayloadSize', '[%s] %d',
      [GetCameraName, m_nBufSizeForDriver]);
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
        GetCameraName);
      exit
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
    raise Exception.Create(Format('MV_CC_RotateImage - 0x%x', [m_nRet]));
end;

function TJdcMVSAbstract.GetOneFrame(out ATime: TDateTime; const TimeOut: Integer): TGraphic;
Var
  stOutFrame: MV_FRAME_OUT_INFO_EX;
begin
  result := nil;
  FImageCount := FImageCount + 1;

  if FCameraType = ctLinescan then
    CheckSpeedChanged; // 속도변화 확인

  ZeroMemory(@stOutFrame, sizeof(MV_FRAME_OUT_INFO_EX));
  m_nRet := MV_CC_GetOneFrameTimeout(m_hDevHandle^, m_pBufForDriver, m_nBufSizeForDriver,
    @stOutFrame, TimeOut);
  if m_nRet <> MV_OK then
  begin
    // raise Exception.Create(Format('GetOneFrame,[%s] Code=0x%x', [GetCameraName, m_nRet]));
    ATime := 0;
    exit;
  end
  else
    ATime := Now;

  result := BufToImage(stOutFrame.ToParamBase(m_pBufForDriver));
end;

function TJdcMVSAbstract.SaveImageEx2(const AParam: MV_IMG_PARAM_BASE): MV_SAVE_IMAGE_PARAM_EX;
var
  pBufForSaveImage: PAnsiChar;
begin
  if m_nBufSizeForSaveImage = 0 then
  begin
    // en:BMP image size: width * height * 3 + 2048 (Reserved BMP header size)
    m_nBufSizeForSaveImage := AParam.nWidth * AParam.nHeight * 3 + 2048;
    TLogging.Obj.ApplicationMessage(msDebug, 'SaveImageSize', '[%s] %d',
      [GetCameraName, m_nBufSizeForSaveImage]);
  end;

  pBufForSaveImage := AnsiStrAlloc(m_nBufSizeForSaveImage);
  // en:Set camera parameter
  ZeroMemory(@result, sizeof(MV_SAVE_IMAGE_PARAM_EX));
  result.enImageType := FSaveImageType;
  // en:Image format to save
  result.enPixelType := AParam.enPixelType; // en:Camera pixel type
  result.nWidth := AParam.nWidth; // en:Width
  result.nHeight := AParam.nHeight; // en:Height
  result.pData := AParam.pData;
  result.nDataLen := AParam.nDataLen;

  result.pImageBuffer := pBufForSaveImage;
  result.nBufferSize := m_nBufSizeForSaveImage;
  // en:Buffer node size

  if FSaveImageType = MV_Image_Jpeg then
    result.nJpgQuality := 80;

  // en:jpg encoding, only valid when saving as Jpg. SDK ignore this parameter when saving as BMP
  m_nRet := MV_CC_SaveImageEx2(m_hDevHandle^, @result);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_SaveImageEx2 - 0x%x', [m_nRet]));
end;

procedure TJdcMVSAbstract.SetAutoConfig(const GainAuto: MV_CAM_GAIN_MODE;
  const ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE);
begin
  FExposureMode := ExposureAuto;
  _SetEnumValue(GAIN_AUTO, Integer(GainAuto), TypeInfo(MV_CAM_GAIN_MODE), msInfo);
  _SetEnumValue(EXPOSURE_AUTO, Integer(FExposureMode), TypeInfo(MV_CAM_EXPOSURE_AUTO_MODE), msInfo);
end;

procedure TJdcMVSAbstract.SetDisplayWindow(AHandle: HWND);
begin
  m_nRet := MV_CC_Display(m_hDevHandle^, AHandle);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_Display - 0x%x', [m_nRet]));
end;

procedure TJdcMVSAbstract.SetImageFormat(const AHeight: Integer; const AWidth: Integer;
  const AOffsetX: Integer; const AReverseX: Boolean);
begin
  _SetBoolValue(REVERSE_X, AReverseX, msInfo);
  _SetIntValue(IMAGE_HEIGHT, AHeight);
  _SetIntValue(OFFSET_X, 0); // Offset 초기화
  _SetIntValue(IMAGE_WIDTH, AWidth);
  _SetIntValue(OFFSET_X, AOffsetX);
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

procedure ProcImageCallBack(const pData: PAnsiChar; pFrameInfo: PMV_FRAME_OUT_INFO_EX;
  Var pUser: Pointer)stdcall;
begin
  OnMVImageCallback(pData, pFrameInfo);
end;

procedure TJdcMVSAbstract.StartGrabbing;
begin
  FImageCount := 0;

  if Assigned(FOnImageCallBack) then
  begin
    m_nRet := MV_CC_RegisterImageCallBackEx(m_hDevHandle^, ProcImageCallBack, m_hDevHandle^);
    if m_nRet <> MV_OK then
      TLogging.Obj.ApplicationMessage(msWarning, 'RegisterImageCallBackEx', '[%s] Code=0x%x',
        [GetCameraName, m_nRet]);
  end
  else
    InitBuffer;

  m_nRet := MV_CC_StartGrabbing(m_hDevHandle^);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('StartGrabbingFail,[%s] Code=0x%x', [GetCameraName, m_nRet]));
end;

procedure TJdcMVSAbstract.StopGrab;
begin
  if not Assigned(m_hDevHandle) then
    exit;

  m_nRet := MV_CC_StopGrabbing(m_hDevHandle^);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, 'StopGrbbing', '[%s] ImageCount=%d',
      [GetCameraName, FImageCount])
  else
    TLogging.Obj.ApplicationMessage(msError, 'StopGrabbingFail', '[%s] Code=0x%x',
      [GetCameraName, m_nRet]);
end;

function TJdcMVSAbstract._SetBoolValue(const Name: string; const Value: Bool;
  const MsgType: TMessageType): Integer;
begin
  result := SetBoolValue(m_hDevHandle, Name, Value);
  if result = MV_OK then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %s', [GetCameraName, BoolToStr(Value)])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%s',
      [GetCameraName, result, BoolToStr(Value)])
end;

function TJdcMVSAbstract._SetEnumValue(const Name: string; const Value: Cardinal;
  const TypeInfo: PTypeInfo; const MsgType: TMessageType): Integer;
begin
  result := SetEnumValue(m_hDevHandle, Name, Value);
  if result = MV_OK then
  begin
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %s',
      [GetCameraName, GetEnumName(TypeInfo, Value)])
  end
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%d',
      [GetCameraName, result, Value])
end;

function TJdcMVSAbstract._SetFloatValue(const Name: string; const Value: Single;
  const MsgType: TMessageType): Integer;
begin
  result := SetFloatValue(m_hDevHandle, Name, Value);
  if result = MV_OK then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %0.2f', [GetCameraName, Value])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%0.2f',
      [GetCameraName, result, Value])
end;

function TJdcMVSAbstract._SetIntValue(const Name: string; const Value: Cardinal;
  const MsgType: TMessageType): Integer;
begin
  result := SetIntValue(m_hDevHandle, Name, Value);
  if result = MV_OK then
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %d', [GetCameraName, Value])
  else
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%d',
      [GetCameraName, result, Value])
end;

end.
