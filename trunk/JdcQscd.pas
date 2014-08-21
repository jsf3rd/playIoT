unit JdcQscd;

// QSCD Data Format
// Byte Order : Big-endian

interface

uses SysUtils, Classes, System.Types, JdcGlobal, Winapi.Windows;

Type
  TQscdHeader = Packed record
    CRC: array [0 .. 3] of Byte;
    Quality: Byte;
    DataType: Byte;
    Reserved: Byte;
    StationCode: array [0 .. 4] of Byte;
    DateTime: DWORD;
  end;

  TMMA = Packed record
    Minimum: DWORD;
    Maximum: DWORD;
    Average: DWORD;
  end;

  TMM = Packed record
    Minnumun: DWORD;
    Maximum: DWORD;
  end;

  TWMMA = Packed record
    Z: TMMA;
    N: TMMA;
    E: TMMA;
  end;

  TTMM = Packed record
    Z: TMM;
    N: TMM;
    E: TMM;
  end;

  TData = Packed record
    Z: DWORD;
    N: DWORD;
    E: DWORD;
  end;

  TQscdBody = Packed record
    WMMA: TWMMA;
    TMM: TTMM;
    MEC: TData;
    HPGA: DWORD;
    TPGA: DWORD;
    SI: TData;
    SI_HPGA: DWORD;
    Correlation: DWORD;
    ChannelNumber: WORD;
    Reserved: WORD;
  end;

  TQscdPacket = Packed record
    QscdHeader: TQscdHeader;
    QscdBody: TQscdBody;
  end;

implementation

end.
