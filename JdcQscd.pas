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
    function GetStateCode: string;
    function GetEventDateTime: TDateTime;
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
    function GetTPGAValue: Double;
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

implementation

uses IdGlobal;

{ TQscdBody }

function TQscdBody.GetTPGAValue: Double;
var
  tmp: Integer;
begin
  CopyMemory(@tmp, @Self.TPGA, 4);
  Result := Rev4BytesF(tmp);
end;

{ TQscdHeader }

function TQscdHeader.GetEventDateTime: TDateTime;
var
  Seconds: Integer;
begin
  CopyMemory(@Seconds, @Self.DateTime, Sizeof(Self.DateTime));
  Result := StrToDateTime('1970-01-01 09:00:00');
  Result := IncSecond(Result, Rev4Bytes(Seconds));
end;

function TQscdHeader.GetStateCode: string;
var
  buffer: TIdBytes;
begin
  SetLength(buffer, Sizeof(Self.StationCode));
  CopyMemory(buffer, @Self.StationCode, Sizeof(Self.StationCode));
  Result := BytesToString(buffer);
end;

end.
