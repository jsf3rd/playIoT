unit JdcGPS;

interface

uses System.SysUtils, System.Classes, JdcGlobal, Data.DB, System.JSON, REST.JSON,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, JdcGlobal.DSCommon, JdcGlobal.ClassHelper,
  System.Types, FireDAC.Stan.Param;

type
  TGPS = record
    Header: string;
    DateTime: TDateTime; // GPS Time
    latitude: double;
    longitude: double;
    Speed: double;
    quality: integer;
    PCTime: TDateTime;
    constructor Create(AValue: string; _PCTime: TDateTime);
    function ToString(): string;
  end;

const
  GPS_GGA = 'GGA';
  GPS_RMC = 'RMC';

implementation

{ TGPS }

constructor TGPS.Create(AValue: string; _PCTime: TDateTime);
var
  Msg: TStringList;
  Index: integer;
begin
  Self.PCTime := _PCTime;

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

      Self.DateTime := StrToDateTimeDef(FormatDateTime('YYYY-MM-DD ', Now) + Format('%s:%s:%s%s',
        [copy(Msg.Strings[Index], 1, 2), copy(Msg.Strings[Index], 3, 2), copy(Msg.Strings[Index], 5,
        2), copy(Msg.Strings[Index], 7, 3)]), _PCTime, DefaultFormatSettings);
      Self.latitude := ConvertDegree(Msg.Strings[Index + 2]);
      Self.longitude := ConvertDegree(Msg.Strings[Index + 4]);
      Self.Speed := StrToFloatDef(Msg.Strings[Index + 6], 0) * 1.852; // knots to km/h
      Index := Index + 11;
    end;

    if Pos(GPS_GGA, Msg.Strings[Index]) > 0 then
    begin
      Inc(Index);
      Self.Header := GPS_GGA;

      // $GPGGA,hhmmss.ss,Latitude,N,Longitude,E,FS,NoSV,HDOP,msl,m,Altref,m,DiffAge,DiffStation*cs<CR><LF>
      // $GPGGA,092725.00,4717.11399,N,00833.91590,E,1,8,1.01,499.6,M,48.0,M,,0*5B

      // $GNGGA,090331.12,3651.27214,N,12637.43041,E,4,12,0.57,62.5,M,18.1,M,,4095*7C

      Self.DateTime := StrToDateTimeDef(FormatDateTime('YYYY-MM-DD ', Now) + Format('%s:%s:%s%s',
        [copy(Msg.Strings[Index], 1, 2), copy(Msg.Strings[Index], 3, 2), copy(Msg.Strings[Index], 5,
        2), copy(Msg.Strings[Index], 7, 3)]), _PCTime, DefaultFormatSettings);
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

function TGPS.ToString: string;
begin
  result := Format('%s,%s,%0.6f, %0.6f,%d', [Self.Header, Self.DateTime.ToString, Self.latitude,
    Self.longitude, Self.quality]);
end;

end.
