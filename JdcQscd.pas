unit JdcQscd;

// QSCD Data Format
// Byte Order : Big-endian

interface

uses SysUtils, Classes, System.Types, JdcGlobal, Winapi.Windows,
  System.DateUtils;

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

  // Protocol.

  TQscdPacket = Packed record
    QscdHeader: TQscdHeader;
    QscdBody: TQscdBody;
  end;


  // Application.

  TQscdData = record
    KeyCode: string;
    EventDate: Double;
    Body: TQscdBody;
  end;

  TPGAHeader = record
    Station: string;
    EventDate: Double;
  end;

  TJdcQscd = class
  public
    class function GetHeaderInfo(AHeader: TQscdHeader): TPGAHeader;
  end;

implementation

uses IdGlobal;

class function TJdcQscd.GetHeaderInfo(AHeader: TQscdHeader): TPGAHeader;
var
  buffer: TIdBytes;
  DateTime: TDateTime;
  Seconds: Integer;
begin
  SetLength(buffer, Sizeof(AHeader.StationCode));
  CopyMemory(buffer, @AHeader.StationCode, Sizeof(AHeader.StationCode));
  CopyMemory(@Seconds, @AHeader.DateTime, Sizeof(AHeader.DateTime));

  DateTime := StrToDateTime('1970-01-01 09:00:00');
  DateTime := IncSecond(DateTime, Rev4Bytes(Seconds));

  Result.Station := BytesToString(buffer);
  Result.EventDate := DateTime;
end;

end.
