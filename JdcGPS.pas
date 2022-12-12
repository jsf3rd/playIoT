unit JdcGPS;

interface

uses System.SysUtils, System.Classes, JdcGlobal, Data.DB, System.JSON, REST.JSON,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, JdcGlobal.DSCommon, JdcGlobal.ClassHelper,
  System.Types, FireDAC.Stan.Param;

type
  TGPS = record
    Header: string;
    DateTime: TDateTime;
    latitude: double;
    longitude: double;
    Speed: double;
    quality: integer;
    constructor Create(AValue: string; ATime: TDateTime);
  end;

const
  GPS_GGA = 'GGA';
  GPS_RMC = 'RMC';

implementation

{ TGPS }

constructor TGPS.Create(AValue: string; ATime: TDateTime);
var
  Msg: TStringList;
  Index: integer;
begin
  Msg := TStringList.Create;
  Msg.CommaText := AValue;

  Index := 0;
  while Index < Msg.Count do
  begin
    if Pos(GPS_RMC, Msg.Strings[Index]) > 0 then
    begin
      Inc(Index);
      Self.Header := GPS_RMC;

      // $GPRMC,hhmmss,status,latitude,N,longitude,E,spd,cog,ddmmyy,mv,mvE,mode*cs<CR><LF>
      // $GPRMC,083559.00,A,4717.11437,N,00833.91522,E,0.004,77.52,091202,,,A*57

      if Msg.Strings[Index + 1] <> 'A' then
        Self.quality := 0
      else
        Self.quality := 1;

      if ATime = 0 then
        // 날짜 변경시 오류 발생
        Self.DateTime := StrToDateTime(FormatDateTime('YYYY-MM-DD ', Now) + Format('%s:%s:%s%s',
          [copy(Msg.Strings[Index], 1, 2), copy(Msg.Strings[Index], 3, 2), copy(Msg.Strings[Index],
          5, 2), copy(Msg.Strings[Index], 7, 3)]), DefaultFormatSettings)
      else
        Self.DateTime := Now;

      Self.latitude := ConvertDegree(Msg.Strings[Index + 2]);
      Self.longitude := ConvertDegree(Msg.Strings[Index + 4]);
      Self.Speed := StrToFloatDef(Msg.Strings[Index + 6], 0) * 1.852;
      Index := Index + 11;
    end;

    if Pos(GPS_GGA, Msg.Strings[Index]) > 0 then
    begin
      Inc(Index);
      Self.Header := GPS_GGA;

      // $GPGGA,hhmmss.ss,Latitude,N,Longitude,E,FS,NoSV,HDOP,msl,m,Altref,m,DiffAge,DiffStation*cs<CR><LF>
      // $GPGGA,092725.00,4717.11399,N,00833.91590,E,1,8,1.01,499.6,M,48.0,M,,0*5B

      if ATime = 0 then
        // 날짜 변경시 오류 발생
        Self.DateTime := StrToDateTime(FormatDateTime('YYYY-MM-DD ', Now) + Format('%s:%s:%s%s',
          [copy(Msg.Strings[Index], 1, 2), copy(Msg.Strings[Index], 3, 2), copy(Msg.Strings[Index],
          5, 2), copy(Msg.Strings[Index], 7, 3)]), DefaultFormatSettings)
      else
        Self.DateTime := Now;

      Self.latitude := ConvertDegree(Msg.Strings[Index + 1]);
      Self.longitude := ConvertDegree(Msg.Strings[Index + 3]);
      Self.Speed := 0;

      // 0: Fix not valid
      // 1: GPS fix
      // 2: Differential GPS fix (DGNSS), SBAS, OmniSTAR VBS, Beacon, RTX in GVBS mode
      // 3: Not applicable
      // 4: RTK Fixed, xFill
      // 5: RTK Float, OmniSTAR XP/HP, Location RTK, RTX
      // 6: INS Dead reckoning
      quality := StrToIntDef(Msg.Strings[Index + 5], 0);
      Index := Index + 15;
    end;
    Inc(Index);
  end;
end;

end.
