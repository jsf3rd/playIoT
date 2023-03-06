unit CameraParamsUnit;

interface

uses
  Windows, SysUtils, Variants, Classes, PixelTypeUnit;

const
  // Definition of correct code
  MV_OK: Integer = $00000000;

  // ch:通用错误码定义:范围0x80000000-0x800000FF | en: Definition of common error code : 0x80000000-0x800000FF
  MV_E_HANDLE: Integer = $80000000;  ///< 错误或无效的句柄 | en:Error or invalid handle
  MV_E_SUPPORT: Integer = $80000001;  ///< 不支持的功能 | en:Not supported function
  MV_E_BUFOVER: Integer = $80000002;  ///< 缓存已满 | en:Cache is full
  MV_E_CALLORDER: Integer = $80000003;  ///< 函数调用顺序错误 | en:Function calling order error
  MV_E_PARAMETER: Integer = $80000004;  ///< 错误的参数 | en:Incorrect parameter
  MV_E_RESOURCE: Integer = $80000006;  ///< 资源申请失败 | en:Applying resource failed
  MV_E_NODATA: Integer = $80000007;  ///< 无数据 | en:No data
  MV_E_PRECONDITION: Integer = $80000008;  ///< 前置条件有误，或运行环境已发生变化 | en:Precondition error, or running environment changed
  MV_E_VERSION: Integer = $80000009;  ///< 版本不匹配 | en:Version mismatches
  MV_E_NOENOUGH_BUF: Integer = $8000000A;  ///< 传入的内存空间不足 | en:Insufficient memory
  MV_E_ABNORMAL_IMAGE: Integer = $8000000B;  ///< 异常图像，可能是丢包导致图像不完整 | en:Abnormal image, maybe incomplete image because of lost packet
  MV_E_LOAD_LIBRARY: Integer = $8000000C;  ///< 动态导入DLL失败 | en:Load library failed
  MV_E_UNKNOW: Integer = $800000FF;  ///< 未知的错误 | en:Unknown error

  // ch:GenICam系列错误:范围0x80000100-0x800001FF | en:GenICam Series Error Codes: Range from 0x80000100 to 0x800001FF
  MV_E_GC_GENERIC: Integer = $80000100;  ///< 通用错误 | en:General error
  MV_E_GC_ARGUMENT: Integer = $80000101;  ///< 参数非法 | en:Illegal parameters
  MV_E_GC_RANGE: Integer   = $80000102;  ///< 值超出范围 | en:The value is out of range
  MV_E_GC_PROPERTY: Integer = $80000103;  ///< 属性 | en:Property
  MV_E_GC_RUNTIME: Integer = $80000104;  ///< 运行环境有问题 | en:Running environment error
  MV_E_GC_LOGICAL: Integer = $80000105;  ///< 逻辑错误 | en:Logical error
  MV_E_GC_ACCESS: Integer  = $80000106;  ///< 节点访问条件有误 | en:Node accessing condition error
  MV_E_GC_TIMEOUT: Integer = $80000107;  ///< 超时 | en:Timeout
  MV_E_GC_DYNAMICCAST: Integer = $80000108;  ///< 转换异常 | en:Transformation exception
  MV_E_GC_UNKNOW: Integer = $800001FF;  ///< GenICam未知错误 | en:GenICam unknown error

  //ch:GigE_STATUS对应的错误码:范围0x80000200-0x800002FF | en:GigE_STATUS Error Codes: Range from 0x80000200 to 0x800002FF
  MV_E_NOT_IMPLEMENTED: Integer = $80000200;  ///< 命令不被设备支持 | en:The command is not supported by device
  MV_E_INVALID_ADDRESS: Integer = $80000201;  ///< 访问的目标地址不存在 | en:The target address being accessed does not exist
  MV_E_WRITE_PROTECT: Integer = $80000202;  ///< 目标地址不可写 | en:The target address is not writable
  MV_E_ACCESS_DENIED: Integer = $80000203;  ///< 设备无访问权限 | en:No permission
  MV_E_BUSY: Integer = $80000204;  ///< 设备忙，或网络断开 | en:Device is busy, or network disconnected
  MV_E_PACKET: Integer = $80000205;  ///< 网络包数据错误 | en:Network data packet error
  MV_E_NETER: Integer = $80000206;  ///< 网络相关错误 | en:Network error
  // GigE相机特有的错误码
  MV_E_IP_CONFLICT = $80000221;  ///< 设备IP冲突 | en:Device IP conflict

  //ch:USB_STATUS对应的错误码:范围0x80000300-0x800003FF | en:USB_STATUS Error Codes: Range from 0x80000300 to 0x800003FF
  MV_E_USB_READ: Integer = $80000300;      ///< 读usb出错 | en:Reading USB error
  MV_E_USB_WRITE: Integer = $80000301;      ///< 写usb出错 | en:Writing USB error
  MV_E_USB_DEVICE: Integer = $80000302;      ///< 设备异常 | en:Device exception
  MV_E_USB_GENICAM: Integer = $80000303;      ///< GenICam相关错误 | en:GenICam error
  MV_E_USB_BANDWIDTH: Integer = $80000304;      ///< 带宽不足  该错误码新增 | en:Insufficient bandwidth, this error code is newly added
  MV_E_USB_DRIVER: Integer = $80000305;      ///< 驱动不匹配或者未装驱动 | en:Driver mismatch or unmounted drive
  MV_E_USB_UNKNOW: Integer = $800003FF;      ///< USB未知的错误 | en:USB unknown error

  //ch:升级时对应的错误码:范围0x80000400-0x800004FF | en:Upgrade Error Codes: Range from 0x80000400 to 0x800004FF
  MV_E_UPG_FILE_MISMATCH: Integer = $80000400; ///< 升级固件不匹配 | en:Firmware mismatches
  MV_E_UPG_LANGUSGE_MISMATCH: Integer = $80000401; ///< 升级固件语言不匹配 | en:Firmware language mismatches
  MV_E_UPG_CONFLICT: Integer = $80000402; ///< 升级冲突（设备已经在升级了再次请求升级即返回此错误） | en:Upgrading conflicted (repeated upgrading requests during device upgrade)
  MV_E_UPG_INNER_ERR: Integer = $80000403; ///< 升级时相机内部出现错误 | en:Camera internal error during upgrade
  MV_E_UPG_UNKNOW: Integer = $800004FF; ///< 升级时未知错误 | en:Unknown error during upgrade


  // ch: 设备类型 | en: device type
  MV_UNKNOW_DEVICE: Integer = $00000000;       // ch:未知设备类型，保留意义 | en:Unknown Device Type, Reserved
  MV_GIGE_DEVICE: Integer = $00000001;         // ch:GigE设备 | en:GigE Device
  MV_1394_DEVICE: Integer = $00000002;         // ch:1394-a/b 设备 | en:1394-a/b Device
  MV_USB_DEVICE: Integer = $00000004;          // ch:USB3.0 设备 | en:USB3.0 Device
  MV_CAMERALINK_DEVICE: Integer = $00000008;   // ch:CameraLink设备 | en:CameraLink Device


// device information
type
  MV_GIGE_DEVICE_INFO = record
    nIpCfgOption: Cardinal;
    nIpCfgCurrent: Cardinal;
    nCurrentIp: Cardinal;
    nCurrentSubNetMask: Cardinal;
    nDefultGateWay: Cardinal;
    chManufacturerName: array[0..31] of Byte;
    chModelName: array[0..31] of Byte;
    chDeviceVersion: array[0..31] of Byte;
    chManufacturerSpecificInfo: array[0..47] of Byte;
    chSerialNumber: array[0..15] of Byte;
    chUserDefinedName: array[0..15] of Byte;
    nNetExport: Cardinal;
    nReserved: array[0..3] of Cardinal;
end;

type
  MV_USB3_DEVICE_INFO = record
    CrtlInEndPoint: Byte;
    CrtlOutEndPoint: Byte;
    StreamEndPoint: Byte;
    EventEndPoint: Byte;
    idVendor: Byte;
    idProduct: Byte;
    nDeviceNumber: Cardinal;
    chDeviceGUID: array[0..63] of Byte;
    chVendorName: array[0..63] of Byte;
    chModelName: array[0..63] of Byte;
    chFamilyName: array[0..63] of Byte;
    chDeviceVersion: array[0..63] of Byte;
    chManufacturerName: array[0..63] of Byte;
    chSerialNumber: array[0..63] of Byte;
    chUserDefinedName: array[0..63] of Byte;
    nbcdUSB: Cardinal;
    nReserved: array[0..2] of Cardinal;
end;

type
  MV_CC_SpecialInfo = record
    case integer of
    0: (stGigEInfo: MV_GIGE_DEVICE_INFO);
    1: (stUsb3VInfo: MV_USB3_DEVICE_INFO);
 end;

 type
  MV_CC_DEVICE_INFO = record
     nMajorVer: Word;
     nMinorVer: Word;
     nMacAddrHigh: Cardinal;
     nMacAddrLow: Cardinal;
     nTLayerType: Cardinal;
     nReserved: array[0..3] of Cardinal;
     SpecialInfo: MV_CC_SpecialInfo;
end;

type
  PMV_CC_DEVICE_INFO_LIST = ^MV_CC_DEVICE_INFO_LIST;
  MV_CC_DEVICE_INFO_LIST = record
    nDeviceNum: Cardinal;
    pDeviceInfo: array[0..255] of ^MV_CC_DEVICE_INFO;
end;


// data type of feature node
type
  PMVCC_INTVALUE = ^MVCC_INTVALUE;
  MVCC_INTVALUE = record
     nCurValue: Cardinal;
     nMax: Cardinal;
     nMin: Cardinal;
     nInc: Cardinal;
     nReserved: array[0..3] of Cardinal;
end;

type
  PMVCC_FLOATVALUE = ^MVCC_FLOATVALUE;
  MVCC_FLOATVALUE = record
     fCurValue: Single;
     fMax: Single;
     fMin: Single;
     nReserved: array[0..3] of Cardinal;
end;


type
  PMVCC_ENUMVALUE = ^MVCC_ENUMVALUE;
  MVCC_ENUMVALUE = record
     nCurValue: Cardinal;
     nSupportedNum: Cardinal;
     nSupportValue: array[0..63] of Cardinal;
     nReserved: array[0..3] of Cardinal;
end;

type
  PMVCC_STRINGVALUE = ^MVCC_STRINGVALUE;
  MVCC_STRINGVALUE = record
     chCurValue: array[0..255] of AnsiChar;
     nReserved: array[0..3] of Cardinal;
end;

// ch:输出帧的信息 | en:Output Frame Information
type
  PMV_FRAME_OUT_INFO_EX = ^MV_FRAME_OUT_INFO_EX;
  MV_FRAME_OUT_INFO_EX = record
     nWidth: Word;  // ch:图像宽 | en:Image Width
     nHeight: Word; // ch:图像高 | en:Image Height
     enPixelType: MvGvspPixelType;
     nFrameNum: Cardinal;  // ch:帧号 | en:Frame Number
     nDevTimeStampHigh: Cardinal;   // ch:时间戳高32位 | en:Timestamp high 32 bits
     nDevTimeStampLow: Cardinal;    // ch:时间戳低32位 | en:Timestamp low 32 bits
     nReserved0: Cardinal;     // ch:保留，8字节对齐 | en:Reserved, 8-byte aligned
     nHostTimeStamp: Int64;    // ch:主机生成的时间戳 | en:Host-generated timestamp

     nFrameLen: Cardinal;

    // ch:以下为chunk新增水印信息 | en:The followings are chunk add frame-specific information
    // ch:设备水印时标 | en:Device frame-specific time scale
     nSecondCount: Cardinal;
     nCycleCount: Cardinal;
     nCycleOffset: Cardinal;

     fGain: Single;
     fExposureTime: Single;
     nAverageBrightness: Cardinal;   // ch:平均亮度 | en:Average brightness

    // ch:白平衡相关 | en:White balance
     nRed: Cardinal;
     nGreen: Cardinal;
     nBlue: Cardinal;

     nFrameCounter: Cardinal;
     nTriggerIndex: Cardinal;   // ch:触发计数 | en:Trigger Counting

     // ch:Line 输入/输出 | en:Line Input/Output
     nInput: Cardinal;
     nOutput: Cardinal;

     // ch:ROI区域 | en:ROI Region
     nOffsetX: Word;
     nOffsetY: Word;
     nChunkWidth: Word;
     nChunkHeight: Word;

     nLostPacket: Cardinal;  // ch:本帧丢包数 | en:Lost Pacekt Number In This Frame

     nReserved: array[0..38] of Cardinal;
end;

// en:Save image type
type  MV_SAVE_IAMGE_TYPE = (
       MV_Image_Undefined = 0,
       MV_Image_Bmp = 1,
       MV_Image_Jpeg = 2,
       MV_Image_Png = 3,            // ch:不支持 | en:Not support
       MV_Image_Tif = 4);            // ch:不支持 | en:Not support

// en:Save Image Parameters
type
  PMV_SAVE_IMAGE_PARAM_EX = ^MV_SAVE_IMAGE_PARAM_EX;
  MV_SAVE_IMAGE_PARAM_EX = record
     pData: PAnsiChar;  // [IN] ch:输入数据缓存 | en:Input Data Buffer
     nDataLen: Cardinal; // [IN] ch:输入数据大小 | en:Input Data Size
     enPixelType: MvGvspPixelType; // [IN] ch:输入数据的像素格式 | en:Input Data Pixel Format
     nWidth: Word;  // [IN] ch:图像宽 | en:Image Width
     nHeight: Word; // [IN] ch:图像高 | en:Image Height

     pImageBuffer: PAnsiChar;  // [OUT] ch:输出图片缓存 | en:Output Image Buffer
     nImageLen: Cardinal; // [OUT] ch:输出图片大小 | en:Output Image Size
     nBufferSize: Cardinal; // [IN] ch:提供的输出缓冲区大小 | en:Output buffer size provided
     enImageType: MV_SAVE_IAMGE_TYPE;  // [IN] ch:输出图片格式 | en:Output Image Format
     nJpgQuality: Cardinal; // [IN] ch:编码质量, (50-99] | en:Encoding quality, (50-99]

     // [IN]ch:Bayer格式转为RGB24的插值方法  0-最近邻 1-双线性 2-Hamilton （如果传入其它值则默认为最近邻）
	   // [IN]en:Interpolation method of convert Bayer to RGB24  0-nearest neighbour 1-bilinearity 2-Hamilton
     nMethodValue: Cardinal;

     nReserved: array[0..2] of Cardinal;
end;

// en:device state

// en:Acquisition mode
type  MV_CAM_ACQUISITION_MODE = (
       MV_ACQ_MODE_SINGLE = 0,            // ch:单帧模式 | en:Single Mode
       MV_ACQ_MODE_MUTLI = 1,            // ch:多帧模式 | en:Multi Mode
       MV_ACQ_MODE_CONTINUOUS = 1);            // ch:持续采集模式 | en:Continuous Mode

// ch:增益模式 | en:Gain Mode
type  MV_CAM_GAIN_MODE = (
       MV_GAIN_MODE_OFF = 0,             // ch:关闭 | en:Single Mode
       MV_GAIN_MODE_ONCE = 1,            // ch:一次 | en:Multi Mode
       MV_GAIN_MODE_CONTINUOUS = 2);     // ch:连续 | en:Continuous Mode

// ch:曝光模式 | en:Exposure Mode
type  MV_CAM_EXPOSURE_MODE = (
       MV_EXPOSURE_MODE_TIMED = 0,
       MV_EXPOSURE_MODE_TRIGGER_WIDTH = 1);

// ch:自动曝光模式 | en:Auto Exposure Mode
type  MV_CAM_EXPOSURE_AUTO_MODE = (
       MV_EXPOSURE_AUTO_MODE_OFF = 0,            // ch:关闭 | en:Off
       MV_EXPOSURE_AUTO_MODE_ONCE = 1,            // ch:一次 | en:Once
       MV_EXPOSURE_AUTO_MODE_CONTINUOUS = 2);            // ch:连续 | en:Continuous

type  MV_CAM_TRIGGER_MODE = (
       MV_TRIGGER_MODE_OFF = 0,            // ch:关闭 | en:Off
       MV_TRIGGER_MODE_ON = 1);            // ch:打开 | en:ON

type  MV_CAM_GAMMA_SELECTOR = (
       MV_GAMMA_SELECTOR_USER = 0,
       MV_GAMMA_SELECTOR_SRGB = 1);

type  MV_CAM_BALANCEWHITE_AUTO = (
       MV_BALANCEWHITE_AUTO_OFF = 0,
       MV_BALANCEWHITE_AUTO_CONTINUOUS = 1,
       MV_BALANCEWHITE_AUTO_ONCE = 2);

type  MV_CAM_TRIGGER_SOURCE = (
       MV_TRIGGER_SOURCE_LINE0 = 0,
       MV_TRIGGER_SOURCE_LINE1 = 1,
       MV_TRIGGER_SOURCE_LINE2 = 2,
       MV_TRIGGER_SOURCE_LINE3 = 3,
       MV_TRIGGER_SOURCE_COUNTER0 = 4,
       MV_TRIGGER_SOURCE_RESERVED = 5,
       MV_TRIGGER_SOURCE_RESERVED2 = 6,
       MV_TRIGGER_SOURCE_SOFTWARE = 7,
       MV_TRIGGER_SOURCE_FrequencyConverter = 8);

type  MV_GIGE_TRANSMISSION_TYPE = (
       MV_GIGE_TRANSTYPE_UNICAST = $0,      // ch:表示单播(默认) | en:Unicast mode
       MV_GIGE_TRANSTYPE_MULTICAST = $1,    // ch:表示组播 | en:Multicast mode
       MV_GIGE_TRANSTYPE_LIMITEDBROADCAST = $2,  // ch:表示局域网内广播，暂不支持 | en:Limited broadcast mode,not support
       MV_GIGE_TRANSTYPE_SUBNETBROADCAST = $3,   // ch:表示子网内广播，暂不支持 | en:Subnet broadcast mode,not support
       MV_GIGE_TRANSTYPE_CAMERADEFINED = $4,     // ch:表示从相机获取，暂不支持 | en:Transtype from camera,not support
       MV_GIGE_TRANSTYPE_UNICAST_DEFINED_PORT = $5,  // ch:表示用户自定义应用端接收图像数据Port号 | en:User Defined Receive Data Port
       MV_GIGE_TRANSTYPE_UNICAST_WITHOUT_RECV = $00010000,  // ch:表示设置了单播，但本实例不接收图像数据 | en:Unicast without receive data
       MV_GIGE_TRANSTYPE_MULTICAST_WITHOUT_RECV = $00010001);  // ch:表示组播模式，但本实例不接收图像数据 | en:Multicast without receive data


type MV_IMG_ROTATION_ANGLE = (
       MV_IMAGE_ROTATE_90 = 1,
       MV_IMAGE_ROTATE_180 = 2,
       MV_IMAGE_ROTATE_270 = 3
);

type
  PMV_CC_ROTATE_IMAGE_PARAM = ^MV_CC_ROTATE_IMAGE_PARAM;
  MV_CC_ROTATE_IMAGE_PARAM = record
     enPixelType: MvGvspPixelType; // [IN] ch:输入数据的像素格式 | en:Input Data Pixel Format
     nWidth: Word;  // [IN] ch:图像宽 | en:Image Width
     nHeight: Word; // [IN] ch:图像高 | en:Image Height

     pSrcData: PAnsiChar;  // [IN] en:Input data buffer
     nSrcDataLen: Cardinal; // [IN] en:Input data length

     pDstBuf: PAnsiChar; // [OUT] en:Output data buffer
     nDstBufLen: Cardinal; // [OUT] en:Output data length
     nDstBufSize: Cardinal; // [OUT] en:Provided output data size

     enRotationAngle: MV_IMG_ROTATION_ANGLE; // [IN] en: Flip type
     nReserved: array[0..7] of Cardinal;
end;

type MV_IMG_FLIP_TYPE = (
       MV_FLIP_VERTICAL = 1,
       MV_FLIP_HORIZONTAL = 2
);

type
  PMV_CC_FLIP_IMAGE_PARAM = ^MV_CC_FLIP_IMAGE_PARAM;
  MV_CC_FLIP_IMAGE_PARAM = record
     enPixelType: MvGvspPixelType; // [IN] ch:输入数据的像素格式 | en:Input Data Pixel Format
     nWidth: Word;  // [IN] ch:图像宽 | en:Image Width
     nHeight: Word; // [IN] ch:图像高 | en:Image Height

     pSrcData: PAnsiChar;  // [IN] en:Input data buffer
     nSrcDataLen: Cardinal; // [IN] en:Input data length

     pDstBuf: PAnsiChar; // [OUT] en:Output data buffer
     nDstBufLen: Cardinal; // [OUT] en:Output data length
     nDstBufSize: Cardinal; // [OUT] en:Provided output data size

     enFlipType: MV_IMG_FLIP_TYPE; // [IN] en: Flip type
     nReserved: array[0..7] of Cardinal;
end;


implementation

end.
