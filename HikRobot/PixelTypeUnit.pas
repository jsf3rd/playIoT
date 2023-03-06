unit PixelTypeUnit;

interface

const
  MV_GVSP_PIX_MONO: Integer = $01000000;
  MV_GVSP_PIX_COLOR: Integer = $02000000;
  MV_GVSP_PIX_CUSTOM: Integer = $80000000;

//function MV_PIXEL_BIT_COUNT( n: Integer ): Integer;

type  MvGvspPixelType =
(
  // Undefined pixel type
  PixelType_Gvsp_Undefined = $FFFFFFFF,

  // Mono buffer format defines
  PixelType_Gvsp_Mono1p = ($01000000 or (1 shl 16) or $0037),
  PixelType_Gvsp_Mono2p = ($01000000 or (2 shl 16) or $0038),
  PixelType_Gvsp_Mono4p = ($01000000 or (4 shl 16) or $0039),
  PixelType_Gvsp_Mono8 = ($01000000 or (8 shl 16) or $0001),
  PixelType_Gvsp_Mono8_Signed = ($01000000 or (8 shl 16) or $0002),
  PixelType_Gvsp_Mono10 = ($01000000 or (16 shl 16) or $0003),
  PixelType_Gvsp_Mono10_Packed = ($01000000 or (12 shl 16) or $0004),
  PixelType_Gvsp_Mono12 = ($01000000 or (16 shl 16) or $0005),
  PixelType_Gvsp_Mono12_Packed = ($01000000 or (12 shl 16) or $0006),
  PixelType_Gvsp_Mono14 = ($01000000 or (16 shl 16) or $0025),
  PixelType_Gvsp_Mono16 = ($01000000 or (16 shl 16) or $0007),

  // Bayer buffer format defines
  PixelType_Gvsp_BayerGR8 = ($01000000 or (8 shl 16) or $0008),
  PixelType_Gvsp_BayerRG8 = ($01000000 or (8 shl 16) or $0009),
  PixelType_Gvsp_BayerGB8 = ($01000000 or (8 shl 16) or $000A),
  PixelType_Gvsp_BayerBG8 = ($01000000 or (8 shl 16) or $000B),
  PixelType_Gvsp_BayerGR10 = ($01000000 or (16 shl 16) or $000C),
  PixelType_Gvsp_BayerRG10 = ($01000000 or (16 shl 16) or $000D),
  PixelType_Gvsp_BayerGB10 = ($01000000 or (16 shl 16) or $000E),
  PixelType_Gvsp_BayerBG10 = ($01000000 or (16 shl 16) or $000F),
  PixelType_Gvsp_BayerGR12 = ($01000000 or (16 shl 16) or $0010),
  PixelType_Gvsp_BayerRG12 = ($01000000 or (16 shl 16) or $0011),
  PixelType_Gvsp_BayerGB12 = ($01000000 or (16 shl 16) or $0012),
  PixelType_Gvsp_BayerBG12 = ($01000000 or (16 shl 16) or $0013),
  PixelType_Gvsp_BayerGR10_Packed = ($01000000 or (12 shl 16) or $0026),
  PixelType_Gvsp_BayerRG10_Packed = ($01000000 or (12 shl 16) or $0027),
  PixelType_Gvsp_BayerGB10_Packed = ($01000000 or (12 shl 16) or $0028),
  PixelType_Gvsp_BayerBG10_Packed = ($01000000 or (12 shl 16) or $0029),
  PixelType_Gvsp_BayerGR12_Packed = ($01000000 or (12 shl 16) or $002A),
  PixelType_Gvsp_BayerRG12_Packed = ($01000000 or (12 shl 16) or $002B),
  PixelType_Gvsp_BayerGB12_Packed = ($01000000 or (12 shl 16) or $002C),
  PixelType_Gvsp_BayerBG12_Packed = ($01000000 or (12 shl 16) or $002D),
  PixelType_Gvsp_BayerGR16 = ($01000000 or (16 shl 16) or $002E),
  PixelType_Gvsp_BayerRG16 = ($01000000 or (16 shl 16) or $002F),
  PixelType_Gvsp_BayerGB16 = ($01000000 or (16 shl 16) or $0030),
  PixelType_Gvsp_BayerBG16 = ($01000000 or (16 shl 16) or $0031),

  // RGB Packed buffer format defines
  PixelType_Gvsp_RGB8_Packed = ($02000000 or (24 shl 16) or $0014),
  PixelType_Gvsp_BGR8_Packed = ($02000000 or (24 shl 16) or $0015),
  PixelType_Gvsp_RGBA8_Packed = ($02000000 or (32 shl 16) or $0016),
  PixelType_Gvsp_BGRA8_Packed = ($02000000 or (32 shl 16) or $0017),
  PixelType_Gvsp_RGB10_Packed = ($02000000 or (48 shl 16) or $0018),
  PixelType_Gvsp_BGR10_Packed = ($02000000 or (48 shl 16) or $0019),
  PixelType_Gvsp_RGB12_Packed = ($02000000 or (48 shl 16) or $001A),
  PixelType_Gvsp_BGR12_Packed = ($02000000 or (48 shl 16) or $001B),
  PixelType_Gvsp_RGB16_Packed = ($02000000 or (48 shl 16) or $0033),
  PixelType_Gvsp_RGB10V1_Packed = ($02000000 or (32 shl 16) or $001C),
  PixelType_Gvsp_RGB10V2_Packed = ($02000000 or (32 shl 16) or $001D),
  PixelType_Gvsp_RGB12V1_Packed = ($02000000 or (36 shl 16) or $0034),
  PixelType_Gvsp_RGB565_Packed = ($02000000 or (16 shl 16) or $0035),
  PixelType_Gvsp_BGR565_Packed = ($02000000 or (16 shl 16) or $0036),

  // YUV Packed buffer format defines
  PixelType_Gvsp_YUV411_Packed = ($02000000 or (12 shl 16) or $001E),
  PixelType_Gvsp_YUV422_Packed = ($02000000 or (16 shl 16) or $001F),
  PixelType_Gvsp_YUV422_YUYV_Packed = ($02000000 or (16 shl 16) or $0032),
  PixelType_Gvsp_YUV444_Packed = ($02000000 or (24 shl 16) or $0020),
  PixelType_Gvsp_YCBCR8_CBYCR = ($02000000 or (24 shl 16) or $003A),
  PixelType_Gvsp_YCBCR422_8 = ($02000000 or (16 shl 16) or $003B),
  PixelType_Gvsp_YCBCR422_8_CBYCRY = ($02000000 or (16 shl 16) or $0043),
  PixelType_Gvsp_YCBCR411_8_CBYYCRYY = ($02000000 or (12 shl 16) or $003C),
  PixelType_Gvsp_YCBCR601_8_CBYCR = ($02000000 or (24 shl 16) or $003D),
  PixelType_Gvsp_YCBCR601_422_8 = ($02000000 or (16 shl 16) or $003E),
  PixelType_Gvsp_YCBCR601_422_8_CBYCRY = ($02000000 or (16 shl 16) or $0044),
  PixelType_Gvsp_YCBCR601_411_8_CBYYCRYY = ($02000000 or (12 shl 16) or $003F),
  PixelType_Gvsp_YCBCR709_8_CBYCR = ($02000000 or (24 shl 16) or $0040),
  PixelType_Gvsp_YCBCR709_422_8 = ($02000000 or (16 shl 16) or $0041),
  PixelType_Gvsp_YCBCR709_422_8_CBYCRY = ($02000000 or (16 shl 16) or $0045),
  PixelType_Gvsp_YCBCR709_411_8_CBYYCRYY = ($02000000 or (12 shl 16) or $0042),

  // RGB Planar buffer format defines
  PixelType_Gvsp_RGB8_Planar = ($02000000 or (24 shl 16) or $0021),
  PixelType_Gvsp_RGB10_Planar = ($02000000 or (48 shl 16) or $0022),
  PixelType_Gvsp_RGB12_Planar = ($02000000 or (48 shl 16) or $0023),
  PixelType_Gvsp_RGB16_Planar = ($02000000 or (48 shl 16) or $0024),

  // Custom image format
  PixelType_Gvsp_Jpeg = ($80000000 or (24 shl 16) or $0001),

  PixelType_Gvsp_Coord3D_ABC32f  = ($02000000 or (96 shl 16) or $00C0),
  PixelType_Gvsp_Coord3D_ABC32f_Planar = ($02000000 or (96 shl 16) or $00C1),

  PixelType_Gvsp_Coord3D_AC32f = ($02000000 or (40 shl 16) or $00C2),//0x024000C2, /* 3D coordinate A-C 32-bit floating point */
  PixelType_Gvsp_COORD3D_DEPTH_PLUS_MASK = ($82000000 or (28 shl 16) or $0001)//0x82280001
);

implementation


end.
