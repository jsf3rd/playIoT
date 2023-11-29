unit JdcMVS.Abstract;

interface

uses System.SysUtils, System.Classes, CameraParamsUnit, ToolFunctionUnit, JdcGlobal, JdcLogging,
  System.Types, Winapi.Windows, System.AnsiStrings, JdcGPS, Vcl.Graphics, System.TypInfo,
  System.Math, Vcl.Imaging.jpeg, JdcView;

type
  TCameraVendor = (cvHik, cvBasler);
  TCameraType = (ctLinescan, ctArea);

  TGraphicClass = class of TGraphic;
  TOnBitmap = procedure(const ACount: Integer; const AImage: TBitmap; const ATime: TDateTime)
    of object;
  TOnJpeg = procedure(const ACount: Integer; const AImage: TJPEGImage; const ATime: TDateTime)
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
    FLineRateFactor: Double;

    FOldSpeed: Double;
    FImageHeight: Integer;

    function InitBuffer: Integer;
    function CalcResolution: Double;
    procedure CheckSpeedChanged(ASpeed: Double);
    procedure SetLineRateFactor(const Value: Double);
  protected
    FCameraVendor: TCameraVendor;
    FCameraType: TCameraType;
    FCameraModel: string;

    m_nRet: Integer; // en:Error code

    m_hDevHandle: PPointer;
    m_nBufSizeForSaveImage: Cardinal;
    m_pBufForDriver: PAnsiChar;
    m_nBufSizeForDriver: Cardinal;
    m_pBufForFlip: PAnsiChar;
    m_nBufSizeForFlip: Cardinal;

    function _SetIntValue(Name: string; Value: Cardinal; MsgType: TMessageType = msSystem): Integer;
    function _SetBoolValue(Name: string; Value: Bool; MsgType: TMessageType = msSystem): Integer;
    function _SetFloatValue(Name: string; Value: Single; MsgType: TMessageType = msSystem): Integer;
    function _SetEnumValue(Name: string; Value: Cardinal; TypeInfo: PTypeInfo;
      MsgType: TMessageType = msSystem): Integer;

    function FlipImage(stOutFrame: MV_FRAME_OUT_INFO_EX; FlipType: MV_IMG_FLIP_TYPE)
      : MV_CC_FLIP_IMAGE_PARAM;
    function SaveImageEx2(stFlipParam: MV_CC_FLIP_IMAGE_PARAM; ImageType: MV_SAVE_IAMGE_TYPE)
      : MV_SAVE_IMAGE_PARAM_EX; overload;
    function SaveImageEx2(stOutFrame: MV_FRAME_OUT_INFO_EX; ImageType: MV_SAVE_IAMGE_TYPE)
      : MV_SAVE_IMAGE_PARAM_EX; overload;

    function GetCameraName: string;
    function GetVendorName: string; virtual; abstract;
  public
    class function CreateCamera(AVendor, AModel: string; AType: TCameraType): TJdcMVSAbstract;

    constructor Create(AModel: string); virtual;
    destructor Destroy; override;

    procedure Open(AHandle: PPointer);
    procedure Close;
    procedure SetPacketSize(InterPacketDelay: Integer = 400);
    procedure SetAutoConfig(GainAuto: MV_CAM_GAIN_MODE = MV_GAIN_MODE_OFF;
      ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE = MV_EXPOSURE_AUTO_MODE_CONTINUOUS); virtual;
    procedure SetGain(AValue: Double); virtual; abstract;
    function SetAcquisitionLineRate(AValue: Double): Integer; virtual; abstract;
    function SetAutoExposureTimeUpperLimit(AValue: Double): Integer; virtual; abstract;
    procedure SetImageFormat(AHeight: Integer; AWidth: Integer; AOffsetX: Integer;
      AReverseX: Boolean);

    function GetAcquisitionLineRate: Double; virtual; abstract;
    function GetExposureTime: Double; virtual; abstract;

    procedure StartGrabbing; virtual;
    procedure StopGrab;

    procedure SetDisplayWindow(AHandle: HWND);

    // LineScan
    function GetOneFrame(AType: MV_SAVE_IAMGE_TYPE; AGraphic: TGraphicClass; ASpeed: Double;
      out ATime: TDateTime): TGraphic; overload;

    // AreaCAM
    function GetOneFrame(AType: MV_SAVE_IAMGE_TYPE; AGraphic: TGraphicClass; out ATime: TDateTime)
      : TGraphic; overload;

    property SensorSize: Double read FSensorSize write FSensorSize;
    property FocalLength: Integer read FFocalLength write FFocalLength;
    property WorkingDist: Integer read FWorkingDist write FWorkingDist;
    property ImageWidth: Integer read FImageWidth write FImageWidth;
    property ImageHeight: Integer read FImageHeight write FImageHeight;
    property LineRateFactor: Double read FLineRateFactor write SetLineRateFactor;
    property CameraType: TCameraType read FCameraType write FCameraType;
    property ImageCount: UInt32 read FImageCount;
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

procedure TJdcMVSAbstract.CheckSpeedChanged(ASpeed: Double);
var
  LineRate, ExpTime: Integer;
  rlt: Integer;
const
  EXPTIME_OFFSET = 7;
begin
  if FOldSpeed = ASpeed then
    Exit;

  if FResolution = 0 then
    Exit;

  FOldSpeed := ASpeed;
  LineRate := Trunc(FOldSpeed * Power(10, 9) / 3600 / FResolution);
  LineRate := Min(LineRate, 20000);
  LineRate := Max(LineRate, 1000);
  LineRate := Trunc(LineRate * FLineRateFactor);
  rlt := SetAcquisitionLineRate(LineRate);
  if rlt <> MV_OK then
  begin
    TView.Obj.sp_AsyncMessage('RestartRearCam');
    Exit;
  end;

  ExpTime := Trunc(1 / LineRate * Power(10, 6) - EXPTIME_OFFSET);
  SetAutoExposureTimeUpperLimit(ExpTime);
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

constructor TJdcMVSAbstract.Create(AModel: string);
begin
  FCameraModel := AModel;
  FImageWidth := 2048;
  FImageHeight := 1024;

  FLineRateFactor := 1;
  FOldSpeed := 0;

  m_hDevHandle := nil;
  m_nBufSizeForDriver := 0;
  m_nBufSizeForSaveImage := 0;
end;

class function TJdcMVSAbstract.CreateCamera(AVendor, AModel: string; AType: TCameraType)
  : TJdcMVSAbstract;
begin
  if AVendor = TJdcMVSHik.VENDOR_NAME then
    result := TJdcMVSHik.Create(AModel)
  else if AVendor = TJdcMVSBasler.VENDOR_NAME then
    result := TJdcMVSBasler.Create(AModel)
  else
    raise Exception.Create('Unknown Camera Vendor [' + AVendor + ']');

  result.CameraType := AType;
end;

destructor TJdcMVSAbstract.Destroy;
begin
  StopGrab;

  if Assigned(m_pBufForFlip) then
    System.AnsiStrings.StrDispose(m_pBufForFlip);

  if Assigned(m_pBufForDriver) then
    System.AnsiStrings.StrDispose(m_pBufForDriver);
end;

function TJdcMVSAbstract.FlipImage(stOutFrame: MV_FRAME_OUT_INFO_EX; FlipType: MV_IMG_FLIP_TYPE)
  : MV_CC_FLIP_IMAGE_PARAM;
begin
  if nil = m_pBufForFlip then
  begin
    m_nBufSizeForFlip := stOutFrame.nFrameLen;
    m_pBufForFlip := AnsiStrAlloc(m_nBufSizeForFlip);
    if (Nil = m_pBufForFlip) or (not(System.AnsiStrings.StrBufSize(m_pBufForFlip) > 0)) then
    begin
      TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
        'malloc m_pBufForFilp failed, run out of memory.' + IntToStr(m_nBufSizeForFlip) + ', ' +
        GetCameraName);
      Exit
    end;
  end;

  ZeroMemory(@result, sizeof(MV_CC_FLIP_IMAGE_PARAM));
  result.enPixelType := stOutFrame.enPixelType;
  result.nWidth := stOutFrame.nWidth; // en:Width
  result.nHeight := stOutFrame.nHeight; // en:Height
  result.pSrcData := m_pBufForDriver;
  result.nSrcDataLen := stOutFrame.nFrameLen;

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

function TJdcMVSAbstract.GetOneFrame(AType: MV_SAVE_IAMGE_TYPE; AGraphic: TGraphicClass;
  out ATime: TDateTime): TGraphic;
Var
  stOutFrame: MV_FRAME_OUT_INFO_EX;
  stSaveParam: MV_SAVE_IMAGE_PARAM_EX;
  Stream: TStream;
begin
  result := nil;
  FImageCount := FImageCount + 1;

  ZeroMemory(@stOutFrame, sizeof(MV_FRAME_OUT_INFO_EX));
  m_nRet := MV_CC_GetOneFrameTimeout(m_hDevHandle^, m_pBufForDriver, m_nBufSizeForDriver,
    @stOutFrame, 1000);
  if m_nRet <> MV_OK then
  begin
    // raise Exception.Create(Format('GetOneFrame,Code=0x%x', [m_nRet]));
    ATime := 0;
    Exit;
  end
  else
    ATime := Now;

  // Save Image
  stSaveParam := SaveImageEx2(stOutFrame, AType);
  try
    Stream := TMemoryStream.Create;
    try
      Stream.Write(stSaveParam.pImageBuffer^, stSaveParam.nImageLen);
      Stream.Position := 0;
      result := AGraphic.Create;
      result.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  finally
    System.AnsiStrings.StrDispose(stSaveParam.pImageBuffer);
  end;
end;

function TJdcMVSAbstract.InitBuffer: Integer;
var
  nRecvBufSize: Cardinal;
begin
  nRecvBufSize := 0;
  result := GetIntValue(m_hDevHandle, 'PayloadSize', @nRecvBufSize);
  if result <> MV_OK then
  begin
    TLogging.Obj.ApplicationMessage(msWarning, 'GetPayloadSizeFail', '[%s] 0x%x',
      [GetCameraName, result]);
    Exit
  end;
  m_nBufSizeForDriver := nRecvBufSize + 2048;
  m_pBufForDriver := AnsiStrAlloc(m_nBufSizeForDriver);

  if (Nil = m_pBufForDriver) or (not(System.AnsiStrings.StrBufSize(m_pBufForDriver) > 0)) then
  begin
    TLogging.Obj.ApplicationMessage(msWarning, 'MallocDriverFail',
      'malloc m_pBufForDriver failed, run out of memory.' + IntToStr(m_nBufSizeForDriver) + ', ' +
      GetCameraName);
    result := MV_E_UNKNOW;
    Exit;
  end;

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

function TJdcMVSAbstract.GetOneFrame(AType: MV_SAVE_IAMGE_TYPE; AGraphic: TGraphicClass;
  ASpeed: Double; out ATime: TDateTime): TGraphic;
Var
  stOutFrame: MV_FRAME_OUT_INFO_EX;
  stSaveParam: MV_SAVE_IMAGE_PARAM_EX;
  stFlipParam: MV_CC_FLIP_IMAGE_PARAM;
  Stream: TStream;
begin
  result := nil;
  FImageCount := FImageCount + 1;
  CheckSpeedChanged(ASpeed); // 속도변화 확인

  ZeroMemory(@stOutFrame, sizeof(MV_FRAME_OUT_INFO_EX));
  m_nRet := MV_CC_GetOneFrameTimeout(m_hDevHandle^, m_pBufForDriver, m_nBufSizeForDriver,
    @stOutFrame, 1000);
  if m_nRet <> MV_OK then
  begin
    // raise Exception.Create(Format('GetOneFrame,[%s] Code=0x%x', [GetCameraName, m_nRet]));
    ATime := 0;
    Exit;
  end
  else
    ATime := Now;

  // Flip Vertical
  stFlipParam := FlipImage(stOutFrame, MV_IMG_FLIP_TYPE.MV_FLIP_VERTICAL);

  // Save Image
  stSaveParam := SaveImageEx2(stFlipParam, AType);
  try
    Stream := TMemoryStream.Create;
    try
      Stream.Write(stSaveParam.pImageBuffer^, stSaveParam.nImageLen);
      Stream.Position := 0;
      result := AGraphic.Create;
      result.LoadFromStream(Stream);
    finally
      Stream.Free;
    end;

  finally
    System.AnsiStrings.StrDispose(stSaveParam.pImageBuffer);
  end;
end;

function TJdcMVSAbstract.SaveImageEx2(stFlipParam: MV_CC_FLIP_IMAGE_PARAM;
  ImageType: MV_SAVE_IAMGE_TYPE): MV_SAVE_IMAGE_PARAM_EX;
var
  pBufForSaveImage: PAnsiChar;
begin
  if m_nBufSizeForSaveImage = 0 then
  begin
    // en:BMP image size: width * height * 1 + 2048 (Reserved BMP header size)
    m_nBufSizeForSaveImage := stFlipParam.nWidth * stFlipParam.nHeight * 1 + 2048;
    TLogging.Obj.ApplicationMessage(msDebug, 'SaveImageSize', '[%s] %d',
      [GetCameraName, m_nBufSizeForSaveImage]);
  end;

  pBufForSaveImage := AnsiStrAlloc(m_nBufSizeForSaveImage);
  // en:Set camera parameter
  ZeroMemory(@result, sizeof(MV_SAVE_IMAGE_PARAM_EX));
  result.enImageType := ImageType;
  // en:Image format to save
  result.enPixelType := stFlipParam.enPixelType; // en:Camera pixel type
  result.nWidth := stFlipParam.nWidth; // en:Width
  result.nHeight := stFlipParam.nHeight; // en:Height
  result.pData := stFlipParam.pDstBuf;
  result.nDataLen := stFlipParam.nDstBufLen;

  result.pImageBuffer := pBufForSaveImage;
  result.nBufferSize := m_nBufSizeForSaveImage;
  // en:Buffer node size
  result.nJpgQuality := 80;
  // en:jpg encoding, only valid when saving as Jpg. SDK ignore this parameter when saving as BMP
  m_nRet := MV_CC_SaveImageEx2(m_hDevHandle^, @result);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_SaveImageEx2 - 0x%x', [m_nRet]));
end;

function TJdcMVSAbstract.SaveImageEx2(stOutFrame: MV_FRAME_OUT_INFO_EX;
  ImageType: MV_SAVE_IAMGE_TYPE): MV_SAVE_IMAGE_PARAM_EX;
var
  pBufForSaveImage: PAnsiChar;
begin
  if m_nBufSizeForSaveImage = 0 then
  begin
    // en:BMP image size: width * height * 1 + 2048 (Reserved BMP header size)
    m_nBufSizeForSaveImage := stOutFrame.nWidth * stOutFrame.nHeight * 1 + 2048;
    TLogging.Obj.ApplicationMessage(msDebug, 'SaveImageSize', '[%s] %d',
      [GetCameraName, m_nBufSizeForSaveImage]);
  end;

  pBufForSaveImage := AnsiStrAlloc(m_nBufSizeForSaveImage);
  // en:Set camera parameter
  ZeroMemory(@result, sizeof(MV_SAVE_IMAGE_PARAM_EX));
  result.enImageType := ImageType;
  // en:Image format to save
  result.enPixelType := stOutFrame.enPixelType; // en:Camera pixel type
  result.nWidth := stOutFrame.nWidth; // en:Width
  result.nHeight := stOutFrame.nHeight; // en:Height
  result.pData := m_pBufForDriver;
  result.nDataLen := stOutFrame.nFrameLen;

  result.pImageBuffer := pBufForSaveImage;
  result.nBufferSize := m_nBufSizeForSaveImage;
  // en:Buffer node size
  result.nJpgQuality := 80;
  // en:jpg encoding, only valid when saving as Jpg. SDK ignore this parameter when saving as BMP
  m_nRet := MV_CC_SaveImageEx2(m_hDevHandle^, @result);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_SaveImageEx2 - 0x%x', [m_nRet]));
end;

procedure TJdcMVSAbstract.SetAutoConfig(GainAuto: MV_CAM_GAIN_MODE;
  ExposureAuto: MV_CAM_EXPOSURE_AUTO_MODE);
begin
  _SetEnumValue(GAIN_AUTO, Integer(MV_GAIN_MODE_OFF), TypeInfo(MV_CAM_GAIN_MODE), msInfo);
  _SetEnumValue(EXPOSURE_AUTO, Integer(MV_EXPOSURE_AUTO_MODE_CONTINUOUS),
    TypeInfo(MV_CAM_EXPOSURE_AUTO_MODE), msInfo);
end;

procedure TJdcMVSAbstract.SetDisplayWindow(AHandle: HWND);
begin
  m_nRet := MV_CC_Display(m_hDevHandle^, AHandle);
  if m_nRet <> MV_OK then
    raise Exception.Create(Format('MV_CC_Display - 0x%x', [m_nRet]));
end;

procedure TJdcMVSAbstract.SetImageFormat(AHeight, AWidth, AOffsetX: Integer; AReverseX: Boolean);
begin
  _SetBoolValue(REVERSE_X, AReverseX, msInfo);
  _SetIntValue(IMAGE_HEIGHT, AHeight);
  _SetIntValue(OFFSET_X, 0); // Offset 초기화
  _SetIntValue(IMAGE_WIDTH, AWidth);
  _SetIntValue(OFFSET_X, AOffsetX);
end;

procedure TJdcMVSAbstract.SetLineRateFactor(const Value: Double);
begin
  FLineRateFactor := Max(Value, 0.1);
  CheckSpeedChanged(FOldSpeed + 0.1);
end;

procedure TJdcMVSAbstract.SetPacketSize(InterPacketDelay: Integer);
var
  nPacketSize: Integer;
begin
  nPacketSize := MV_CC_GetOptimalPacketSize(m_hDevHandle^);
  if nPacketSize > 0 then
    _SetIntValue(GEV_SCPS_PACKET_SIZE, nPacketSize, msInfo);
  _SetIntValue(GEV_SCPD, InterPacketDelay, msInfo);
end;

procedure TJdcMVSAbstract.StartGrabbing;
begin
  InitBuffer;
  FImageCount := 0;
  m_nRet := MV_CC_StartGrabbing(m_hDevHandle^);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, 'StartGrbbing', '[%s] PayloadSize=%d',
      [GetCameraName, m_nBufSizeForDriver])
  else
    raise Exception.Create(Format('StartGrabbingFail,[%s] Code=0x%x', [GetCameraName, m_nRet]));
end;

procedure TJdcMVSAbstract.StopGrab;
begin
  if not Assigned(m_hDevHandle) then
    Exit;

  m_nRet := MV_CC_StopGrabbing(m_hDevHandle^);
  if m_nRet = MV_OK then
    TLogging.Obj.ApplicationMessage(msInfo, 'StopGrbbing', '[%s] ImageCount=%d',
      [GetCameraName, FImageCount])
  else
    TLogging.Obj.ApplicationMessage(msError, 'StopGrabbingFail', '[%s] Code=0x%x',
      [GetCameraName, m_nRet]);
end;

function TJdcMVSAbstract._SetBoolValue(Name: string; Value: Bool; MsgType: TMessageType): Integer;
begin
  result := SetBoolValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%s',
      [GetCameraName, result, BoolToStr(Value)])
  else
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %s', [GetCameraName, BoolToStr(Value)])
end;

function TJdcMVSAbstract._SetEnumValue(Name: string; Value: Cardinal; TypeInfo: PTypeInfo;
  MsgType: TMessageType): Integer;
begin
  result := SetEnumValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%d',
      [GetCameraName, result, Value])
  else
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %s',
      [GetCameraName, GetEnumName(TypeInfo, Value)])
end;

function TJdcMVSAbstract._SetFloatValue(Name: string; Value: Single; MsgType: TMessageType)
  : Integer;
begin
  result := SetFloatValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%0.2f',
      [GetCameraName, result, Value])
  else
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %0.2f', [GetCameraName, Value])
end;

function TJdcMVSAbstract._SetIntValue(Name: string; Value: Cardinal; MsgType: TMessageType)
  : Integer;
begin
  result := SetIntValue(m_hDevHandle, Name, Value);
  if result <> MV_OK then
    TLogging.Obj.ApplicationMessage(msWarning, 'Set_' + Name, '[%s] ErrorCode=0x%x,Value=%d',
      [GetCameraName, result, Value])
  else
    TLogging.Obj.ApplicationMessage(MsgType, Name, '[%s] %d', [GetCameraName, Value])
end;

end.
