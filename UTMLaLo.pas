{
ZWECK       : Konvertierung geografischer Koordinaten :
              Längengrad / Breitengrad nach UTM und umgekehrt

DATEIEN     : UTMLaLo.pas
              TestUTMLaLo.pas
              TestUTMLaLo_MainForm.pas
              TestUTMLaLo_MainForm.dfm

ÄNDERUNGEN  : V1.0 Nov.98

AUTOR       : Kurt Spitzley, email Kurt.Spitzley@rz-online.de oder
                                   101.10490@germanynet.de

COPYRIGHT   : (c) 1998 bei Kurt Spitzley
              Diese Software darf zu nichtkommerziellen Zwecken frei genutzt
              und weitergegeben werden.
GEWÄHR-
LEISTUNG    : Sie akzeptieren diese Software wie sie ist, ohne eine Garantie
              jeglicher Art, einschließlich aber nicht ausschließlich der
              Eignung für eine beliebige Anwendung.

BESCHREIBUNG: Eine Sammlung von Prozeduren, Funktionen und eine Klasse
              zur Konvertierung von geografischen Positionen.

================================================================================

SCOPE       : Conversion of geographical positions:
              Longitude / Latitude to Universal Transverse Mercator (UTM)
              and vice versa

FILES       : UTMLaLo.pas
              TestUTMLaLo.pas
              TestUTMLaLo_MainForm.pas
              TestUTMLaLo_MainForm.dfm

LAST CHANGES: V1.0 Nov.98

AUTHOR      : Kurt Spitzley, email Kurt.Spitzley@rz-online.de or
                                   101.10490@germanynet.de

COPYRIGHT   : (c) 1998 by Kurt Spitzley, All rights reserved.
              This software should not be SOLD by anyone. It is distributed as
              freeware and therefore may be used free of charge.

DISCLAIMER  : You accept this software AS IS without any representation or
              warranty of any kind, including but not limited to the warranty of
              merchantability or fitness for a particular purpose.

DESCRIPTION:  A collection of procedures, functions and a class for converting
              geographical positions.
              Original C++ code by Chuck Gantz- chuck.gantz@globalstar.com
              Converted to Object Pascal and extended by Kurt Spitzley

              Reference ellipsoids derived from Peter H. Dana's website-
              http://www.utexas.edu/depts/grg/gcraft/notes/datum/elist.html
              Department of Geography, University of Texas at Austin
              Internet: pdana@mail.utexas.edu
              Source
              Defense Mapping Agency. 1987b. DMA Technical Report:
              Supplement to Department of Defense World Geodetic System
              1984 Technical Report. Part I and II. Washington, DC:
              Defense Mapping Agency
}

unit UTMLaLo;

interface

const
  PI = 3.14159265;
  FOURTHPI = PI / 4;
  deg2rad = PI / 180;
  rad2deg = 180.0 / PI;
  k0 = 0.9996;

type
  TEllipsoid = record
    Name: string;
    a,b: Extended;
  end;

  TDegMinSec = record
    Degrees: integer;
    Minutes: byte;
    Seconds: Extended;
  end;

  TLaLoPoint= record
    Latitude,
    Longitude: TDegMinSec;
  end;
  PLaLoPoint = ^TLaLoPoint;

  TRealLaLoPoint= record
    Latitude,
    Longitude: extended;
  end;
  PRealLaLoPoint = ^TRealLaLoPoint;

  TUTMPoint= record
    Zone: Byte;
    ZoneLetter: char;
    Easting,
    Northing:extended;
  end;
  PUTMPoint = ^TUTMPoint;

type
  TMapDatum = (
    Airy,
    AustralianNational,
    Bessel1841,
    Bessel1841Nambia,
    Clarke1866,
    Clarke1880,
    Everest,
    Fischer1960Mercury,
    Fischer1968,
    GRS1967,
    GRS1980,
    Helmert1906,
    Hough,
    International,
    Krassovsky,
    ModifiedAiry,
    ModifiedEverest,
    ModifiedFischer1960,
    SouthAmerican1969,
    WGS60,
    WGS66,
    WGS72,
    WGS84);

const
  // some hints to ellipsoid definitions:
  // a : große Halbachse, semi-major-axis
  // b: kleine Halbachse, semi-minor-axis
  // e^2: numerische Exzentrizität = First Eccentricity Squared
  // e^2 = (a^2-b^2)/a^2
  // also often used for ellipsoid description:
  // Flattening f = (a-b)/a

  Ellipsoid: array[TMapDatum] of TEllipsoid  = (
  (Name:'Airy';                  a:6377563; b:6356257),
  (Name:'Australian National';   a:6378160; b:6356775),
  (Name:'Bessel 1841';           a:6377397; b:6356079),
  (Name:'Bessel 1841 (Nambia) '; a:6377484; b:6356166),
  (Name:'Clarke 1866';           a:6378206; b:6356583),
  (Name:'Clarke 1880';           a:6378249; b:6356515),
  (Name:'Everest';               a:6377276; b:6356075),
  (Name:'Fischer 1960(Mercury)'; a:6378166; b:6356784),
  (Name:'Fischer 1968';          a:6378150; b:6356768),
  (Name:'GRS 1967';              a:6378160; b:6356775),
  (Name:'GRS 1980';              a:6378137; b:6356752),
  (Name:'Helmert 1906';          a:6378200; b:6356818),
  (Name:'Hough';                 a:6378270; b:6356794),
  (Name:'International';         a:6378388; b:6356912),
  (Name:'Krassovsky';            a:6378245; b:6356863),
  (Name:'Modified Airy';         a:6377340; b:6356034),
  (Name:'Modified Everest';      a:6377304; b:6356103),
  (Name:'Modified Fischer 1960'; a:6378155; b:6356773),
  (Name:'South American 1969';   a:6378160; b:6356775),
  (Name:'WGS 60';                a:6378165; b:6356783),
  (Name:'WGS 66';                a:6378145; b:6356760),
  (Name:'WGS 72';                a:6378135; b:6356751),
  (Name:'WGS 84';                a:6378137; b:6356752));

// procedures and functions for standalone-use

procedure RealLaLoToUTM(RefEllipsoid: TMapDatum;
            Lat, Long: Extended;
            var UTMNorthing, UTMEasting: Extended;
            var UTMZone: byte;
            var UTMZoneLetter: char);

procedure LaLoToUTM(RefEllipsoid: TMapDatum;
            Lat, Long: TDegMinSec;
	    var UTMNorthing, UTMEasting: Extended;
            var UTMZone: byte;
            var UTMZoneLetter: char);

procedure UTMtoRealLaLo(RefEllipsoid: TMapDatum;
            UTMNorthing, UTMEasting: Extended;
            UTMZone: byte;
            UTMZoneLetter: char;
            var Lat, Long: Extended );

procedure UTMtoLaLo(RefEllipsoid: TMapDatum;
            UTMNorthing, UTMEasting: Extended;
            UTMZone: byte;
            UTMZoneLetter: char;
	    var Lat, Long: TDegMinSec);

function RealLaLo2UTM(RefEllipsoid: TMapDatum; LaLo: TRealLaLoPoint): TUTMPoint;
function LaLo2UTM(RefEllipsoid: TMapDatum; LaLo: TLaLoPoint): TUTMPoint;

function UTM2RealLaLo(RefEllipsoid: TMapDatum; UTMPoint: TUTMPoint): TRealLaLoPoint;
function UTM2LaLo(RefEllipsoid: TMapDatum; UTMPoint: TUTMPoint): TLaLoPoint;
function UTMLetterDesignator(Lat: Extended):char;

function DegMinSec2Extended(DegMinSec: TDegMinSec):Extended;
function Extended2DegMinSec(RealDeg: Extended):TDegMinSec;

// geo-class with speed-optimized calculations (doesn't need above functions)

type
  TUTMLaLo = class
  private
    FRefEllipsoid: TMapDatum;
    FSemiMajorAxis,
    FeccSquared,
    FeccPrimeSquared,
    Fa,
    Fe1,
    M1,M2,M3,M4,
    mu1,mu2,mu3,mu4: extended;
    procedure SetRefEllipsoid(MapDatum: TMapDatum);
  public
    procedure RealLaLoToUTM(Lat, Long: Extended;
                var UTMNorthing, UTMEasting: Extended;
                var UTMZone: byte;
                var UTMZoneLetter: char);
    procedure LaLoToUTM(Lat, Long: TDegMinSec;
                var UTMNorthing, UTMEasting: Extended;
                var UTMZone: byte;
                var UTMZoneLetter: char);
    procedure UTMtoRealLaLo(UTMNorthing, UTMEasting: Extended; UTMZone: byte;
                UTMZoneLetter: char;
                var Lat, Long: Extended );
    procedure UTMtoLaLo(UTMNorthing, UTMEasting: Extended; UTMZone: byte;
                UTMZoneLetter: char;
                var Lat, Long: TDegMinSec);
    function RealLaLo2UTM(LaLo: TRealLaLoPoint): TUTMPoint;
    function LaLo2UTM(LaLo: TLaLoPoint): TUTMPoint;
    function UTM2RealLaLo(UTMPoint: TUTMPoint): TRealLaLoPoint;
    function UTM2LaLo(UTMPoint: TUTMPoint): TLaLoPoint;
    property RefEllipsoid: TMapDatum read FRefEllipsoid write SetRefEllipsoid;
  end;

implementation

uses SysUtils,Math;

procedure RealLaLoToUTM(RefEllipsoid: TMapDatum; Lat, Long: Extended;
            var UTMNorthing, UTMEasting: Extended;
            var UTMZone: byte;
            var UTMZoneLetter: char);
var
 SemiMajorAxis,
 eccSquared,
 eccPrimeSquared,
 LongOrigin,
 N, T, C, A, M,
 LongRad,
 LongOriginRad,
 LatRad: Extended;
begin
//converts lat/long to UTM coords.  Equations from USGS Bulletin 1532
//East Longitudes are positive, West longitudes are negative.
//North latitudes are positive, South latitudes are negative
//Lat and Long are in decimal degrees
//Does not take into account thespecial UTM zones between 0 degrees and
//36 degrees longitude above 72 degrees latitude and a special zone 32
//between 56 degrees and 64 degrees north latitude

  SemiMajorAxis := Ellipsoid[RefEllipsoid].a;
  eccSquared := (sqr(Ellipsoid[RefEllipsoid].a)-sqr(Ellipsoid[RefEllipsoid].b))
                 /sqr(Ellipsoid[RefEllipsoid].a);
  eccPrimeSquared := (eccSquared)/(1-eccSquared);

  LatRad := Lat*deg2rad;
  LongRad := Long*deg2rad;

  if(Long > -6) and (Long <= 0) then
    LongOrigin := -3 //arbitrarily set origin at 0 longitude to 3W
  else
    if(Long < 6) and (Long > 0) then
      LongOrigin := 3
    else
      LongOrigin := round(Long/6)*6+3*round(Long/6)/abs(round(Long/6));
  LongOriginRad := LongOrigin * deg2rad;

  //compute the UTM Zone from the latitude and longitude

  UTMZone:=round((Long + 180)/6) + 1;
  UTMZoneLetter:=UTMLetterDesignator(Lat);

  N := SemiMajorAxis/sqrt(1-eccSquared*sin(LatRad)*sin(LatRad));
  T := tan(LatRad)*tan(LatRad);
  C := eccPrimeSquared*cos(LatRad)*cos(LatRad);
  A := cos(LatRad)*(LongRad-LongOriginRad);

  M := SemiMajorAxis*((1 - eccSquared/4
         - 3*eccSquared*eccSquared/64
         - 5*eccSquared*eccSquared*eccSquared/256)*LatRad
         - (3*eccSquared/8
         + 3*eccSquared*eccSquared/32
         + 45*eccSquared*eccSquared*eccSquared/1024)*sin(2*LatRad)
         + (15*eccSquared*eccSquared/256
         + 45*eccSquared*eccSquared*eccSquared/1024)*sin(4*LatRad)
         - (35*eccSquared*eccSquared*eccSquared/3072)*sin(6*LatRad));

  UTMEasting := (k0*N*(A+(1-T+C)*A*A*A/6
		+ (5-18*T+T*T+72*C-58*eccPrimeSquared)*A*A*A*A*A/120)
		+ 500000.0);

  UTMNorthing :=(k0*(M+N*tan(LatRad)*(A*A/2+(5-T+9*C+4*C*C)*A*A*A*A/24
		+ (61-58*T+T*T+600*C-330*eccPrimeSquared)*A*A*A*A*A*A/720)));
  if(Lat < 0) then
    UTMNorthing :=UTMNorthing + 10000000.0; //10000000 meter offset for southern hemisphere
end;

procedure LaLoToUTM(RefEllipsoid: TMapDatum; Lat, Long: TDegMinSec;
	var UTMNorthing, UTMEasting: Extended;
        var UTMZone: byte;
        var UTMZoneLetter: char);
begin
  RealLaLoToUTM(RefEllipsoid, DegMinSec2Extended(Lat), DegMinSec2Extended(Long),
    UTMNorthing, UTMEasting, UTMZone, UTMZoneLetter);
end;

function RealLaLo2UTM(RefEllipsoid: TMapDatum; LaLo: TRealLaLoPoint): TUTMPoint;
begin
  with Result do
    RealLaLoToUTM(RefEllipsoid, LaLo.Latitude, LaLo.Longitude,
      Northing, Easting, Zone, ZoneLetter);
end;

function LaLo2UTM(RefEllipsoid: TMapDatum; LaLo: TLaLoPoint): TUTMPoint;
begin
  with Result do
    LaLoToUTM(RefEllipsoid, LaLo.Latitude, LaLo.Longitude,
      Northing, Easting, Zone, ZoneLetter);
end;

//------------------------------------------------------------------------------

procedure UTMtoRealLaLo(RefEllipsoid: TMapDatum;
            UTMNorthing, UTMEasting: Extended;
            UTMZone: byte;
            UTMZoneLetter: char;
            var Lat, Long: Extended );
var
  a,
  eccSquared,
  eccPrimeSquared,
  e1,
  N1, T1, C1, R1, D, M,
  LongOrigin,
  mu,
  phi1Rad,
  x, y: Extended;
  ZoneNumber: integer;
  ZoneLetter: char;
begin
//converts UTM coords to lat/long.  Equations from USGS Bulletin 1532
//East Longitudes are positive, West longitudes are negative.
//North latitudes are positive, South latitudes are negative
//Lat and Long are in decimal degrees.
//Does not take into account the special UTM zones between 0 degrees
//and 36 degrees longitude above 72 degrees latitude and a special
//zone 32 between 56 degrees and 64 degrees north latitude

  a := Ellipsoid[RefEllipsoid].a;
  eccSquared := (sqr(Ellipsoid[RefEllipsoid].a)-sqr(Ellipsoid[RefEllipsoid].b))
                 /sqr(Ellipsoid[RefEllipsoid].a);
  eccPrimeSquared := eccSquared/(1-eccSquared);
  e1 := (1-sqrt(1-eccSquared))/(1+sqrt(1-eccSquared));

  x := UTMEasting - 500000.0; //remove 500,000 meter offset for longitude
  y := UTMNorthing;

  ZoneNumber := UTMZone;
  ZoneLetter := UTMZoneLetter;

  if (ord(ZoneLetter) < ord('N'))and (y<>0) then
    // point is in southern hemisphere
    y :=y - 10000000.0;//remove 10,000,000 meter offset used for southern hemisphere

  LongOrigin := (ZoneNumber - 1)*6 - 180 + 3;  //+3 puts origin in middle of zone

  M := y / k0;
  mu := M/(a*(1-eccSquared/4-3*eccSquared*eccSquared/64-5*eccSquared*eccSquared*eccSquared/256));

  phi1Rad := mu	+ (3*e1/2-27*e1*e1*e1/32)*sin(2*mu)
		+ (21*e1*e1/16-55*e1*e1*e1*e1/32)*sin(4*mu)
		+ (151*e1*e1*e1/96)*sin(6*mu);

  N1 := a/sqrt(1-eccSquared*sin(phi1Rad)*sin(phi1Rad));
  T1 := tan(phi1Rad)*tan(phi1Rad);
  C1 := eccPrimeSquared*cos(phi1Rad)*cos(phi1Rad);
  R1 := a*(1-eccSquared)/Power(1-eccSquared*sin(phi1Rad)*sin(phi1Rad), 1.5);
  D := x/(N1*k0);

  Lat := phi1Rad
         - (N1*tan(phi1Rad)/R1)*(D*D/2
         - (5+3*T1+10*C1-4*C1*C1-9*eccPrimeSquared)*D*D*D*D/24
	 + (61+90*T1+298*C1+45*T1*T1-252*eccPrimeSquared-3*C1*C1)*D*D*D*D*D*D/720);
  Lat := Lat * rad2deg;

  Long := (D-(1+2*T1+C1)*D*D*D/6+(5-2*C1+28*T1-3*C1*C1+8*eccPrimeSquared+24*T1*T1)
	*D*D*D*D*D/120)/cos(phi1Rad);
  Long := LongOrigin + Long * rad2deg;

end;

procedure UTMtoLaLo(RefEllipsoid: TMapDatum;
            UTMNorthing, UTMEasting: Extended;
            UTMZone: byte;
            UTMZoneLetter: char;
	    var Lat, Long: TDegMinSec);
var
  RealLat,
  RealLong: Extended;
begin
  UTMtoRealLaLo(RefEllipsoid, UTMNorthing, UTMEasting, UTMZone, UTMZoneLetter,
    RealLat, RealLong);
  Lat:=Extended2DegMinSec(RealLat);
  Long:=Extended2DegMinSec(RealLong);
end;

function UTM2RealLaLo(RefEllipsoid: TMapDatum; UTMPoint: TUTMPoint): TRealLaLoPoint;
begin
  with Result do
    UTMtoRealLaLo(RefEllipsoid, UTMPoint.Northing, UTMPoint.Easting,
      UTMPoint.Zone, UTMPoint.ZoneLetter, Latitude, Longitude);
end;

function UTM2LaLo(RefEllipsoid: TMapDatum; UTMPoint: TUTMPoint): TLaLoPoint;
begin
  with Result do
    UTMtoLaLo(RefEllipsoid, UTMPoint.Northing, UTMPoint.Easting,
      UTMPoint.Zone, UTMPoint.ZoneLetter, Latitude, Longitude);
end;

//------------------------------------------------------------------------------

function DegMinSec2Extended(DegMinSec: TDegMinSec):Extended;
begin
  with DegMinSec do
    Result:=(Minutes * 60 + Seconds)/3600.0 + Degrees;
end;

function Extended2DegMinSec(RealDeg: Extended):TDegMinSec;
var
  Seconds: Extended;
begin
  Result.Degrees:= Trunc(RealDeg);
  Seconds:=Frac(RealDeg)*3600;
  Result.Minutes := Trunc(Seconds / 60);
  Result.Seconds := Seconds - Result.Minutes*60;
end;

function UTMLetterDesignator(Lat: Extended):char;
var
 LetterDesignator: char;
begin
//This routine determines the correct UTM letter designator for the given latitude
//returns 'Z' if latitude is outside the UTM limits of 80N to 80S

  if     ((80  >= Lat) and (Lat >  72)) then LetterDesignator := 'X'
  else if((72  >= Lat) and (Lat >  64)) then LetterDesignator := 'W'
  else if((64  >= Lat) and (Lat >  56)) then LetterDesignator := 'V'
  else if((56  >= Lat) and (Lat >  48)) then LetterDesignator := 'U'
  else if((48  >= Lat) and (Lat >  40)) then LetterDesignator := 'T'
  else if((40  >= Lat) and (Lat >  32)) then LetterDesignator := 'S'
  else if((32  >= Lat) and (Lat >  24)) then LetterDesignator := 'R'
  else if((24  >= Lat) and (Lat >  16)) then LetterDesignator := 'Q'
  else if((16  >= Lat) and (Lat >   8)) then LetterDesignator := 'P'
  else if(( 8  >= Lat) and (Lat >   0)) then LetterDesignator := 'N'
  else if(( 0  >= Lat) and (Lat >  -8)) then LetterDesignator := 'M'
  else if((-8  >= Lat) and (Lat > -16)) then LetterDesignator := 'L'
  else if((-16 >= Lat) and (Lat > -24)) then LetterDesignator := 'K'
  else if((-24 >= Lat) and (Lat > -32)) then LetterDesignator := 'J'
  else if((-32 >= Lat) and (Lat > -40)) then LetterDesignator := 'H'
  else if((-40 >= Lat) and (Lat > -48)) then LetterDesignator := 'G'
  else if((-48 >= Lat) and (Lat > -56)) then LetterDesignator := 'F'
  else if((-56 >= Lat) and (Lat > -64)) then LetterDesignator := 'E'
  else if((-64 >= Lat) and (Lat > -72)) then LetterDesignator := 'D'
  else if((-72 >= Lat) and (Lat > -80)) then LetterDesignator := 'C'
  else LetterDesignator := 'Z'; //This is here as an error flag to show that the Latitude is outside the UTM limits

  result:= LetterDesignator;
end;

//------------------------------------------------------------------------------

procedure TUTMLaLo.SetRefEllipsoid(MapDatum: TMapDatum);
begin
  FRefEllipsoid:=MapDatum;
  FSemiMajorAxis := Ellipsoid[FRefEllipsoid].a;
  FeccSquared := (sqr(Ellipsoid[RefEllipsoid].a)-sqr(Ellipsoid[RefEllipsoid].b))
                 /sqr(Ellipsoid[RefEllipsoid].a);
  FeccPrimeSquared := (FeccSquared)/(1-FeccSquared);
  Fa := Ellipsoid[RefEllipsoid].a;
  Fe1 := (1-sqrt(1-FeccSquared))/(1+sqrt(1-FeccSquared));
  M1 := 1 - FeccSquared / 4
      - 3 * FeccSquared * FeccSquared / 64
      - 5 * FeccSquared * FeccSquared * FeccSquared / 256;

  M2 := 3 * FeccSquared / 8
      + 3 * FeccSquared * FeccSquared / 32
      + 45*FeccSquared * FeccSquared * FeccSquared / 1024;

  M3 := 15 * FeccSquared * FeccSquared / 256
      + 45 * FeccSquared * FeccSquared * FeccSquared / 1024;

  M4 := 35 * FeccSquared * FeccSquared * FeccSquared / 3072;
  mu1 := (Fa*(1-
          FeccSquared/4 -
          3*FeccSquared * FeccSquared / 64-
          5*FeccSquared * FeccSquared * FeccSquared/256));
  mu2 := (3*Fe1/2-27*Fe1*Fe1*Fe1/32);
  mu3 := (21*Fe1*Fe1/16-55*Fe1*Fe1*Fe1*Fe1/32);
  mu4 := (151*Fe1*Fe1*Fe1/96);
end;

//Main procedure Latitude/Longitude to UTM

procedure TUTMLaLo.RealLaLoToUTM(Lat, Long: Extended;
                     var UTMNorthing, UTMEasting: Extended;
                     var UTMZone: byte;
                     var UTMZoneLetter: char);
var
 LongOrigin,
 N, T, C, A, M,
 LongRad,
 LongOriginRad,
 LatRad: Extended;
begin
  LatRad := Lat*deg2rad;
  LongRad := Long*deg2rad;

  if(Long > -6) and (Long <= 0) then
    LongOrigin := -3 //arbitrarily set origin at 0 longitude to 3W
  else
    if(Long < 6) and (Long > 0) then
      LongOrigin := 3
    else
      LongOrigin := round(Long/6)*6+3*round(Long/6)/abs(round(Long/6));
  LongOriginRad := LongOrigin * deg2rad;

  UTMZone:=round((Long + 180)/6) + 1;
  UTMZoneLetter:=UTMLetterDesignator(Lat);

  N := FSemiMajorAxis/sqrt(1-FeccSquared*sin(LatRad)*sin(LatRad));
  T := tan(LatRad)*tan(LatRad);
  C := FeccPrimeSquared*cos(LatRad)*cos(LatRad);
  A := cos(LatRad)*(LongRad-LongOriginRad);

  M := FSemiMajorAxis *
      (M1 * LatRad -
       M2 * sin(2*LatRad) +
       M3 * sin(4*LatRad) -
       M4 * sin(6*LatRad));

  UTMEasting := (k0*N*(A+(1-T+C)*A*A*A/6
		+ (5-18*T+T*T+72*C-58*FeccPrimeSquared)*A*A*A*A*A/120)
		+ 500000.0);

  UTMNorthing :=(k0*(M+N*tan(LatRad)*(A*A/2+(5-T+9*C+4*C*C)*A*A*A*A/24
		+ (61-58*T+T*T+600*C-330*FeccPrimeSquared)*A*A*A*A*A*A/720)));
  if(Lat < 0) then
    UTMNorthing :=UTMNorthing + 10000000.0; //10000000 meter offset for southern hemisphere
end;

//Main procedure UTM toLatitude/Longitude

procedure TUTMLaLo.UTMtoRealLaLo(UTMNorthing, UTMEasting: Extended;
                     UTMZone: byte;
                     UTMZoneLetter: char;
                     var Lat, Long: Extended );
var
  N1, T1, C1, R1, D, M,
  LongOrigin,
  mu,
  phi1Rad,
  x, y: Extended;
  ZoneNumber: integer;
  ZoneLetter: char;
begin
  x := UTMEasting - 500000.0; //remove 500,000 meter offset for longitude
  y := UTMNorthing;

  ZoneNumber := UTMZone;
  ZoneLetter := UTMZoneLetter;

  if (ord(ZoneLetter) < ord('N'))and (y<>0) then
    // point is in southern hemisphere
    y :=y - 10000000.0;//remove 10,000,000 meter offset used for southern hemisphere

  LongOrigin := (ZoneNumber - 1)*6 - 180 + 3;  //+3 puts origin in middle of zone

  M := y / k0;
  mu := M / mu1;

  phi1Rad := mu + mu2 * sin(2*mu) + mu3 * sin(4*mu) + mu4 * sin(6*mu);

  N1 := Fa/sqrt(1-FeccSquared*sin(phi1Rad)*sin(phi1Rad));
  T1 := tan(phi1Rad)*tan(phi1Rad);
  C1 := FeccPrimeSquared*cos(phi1Rad)*cos(phi1Rad);
  R1 := Fa*(1-FeccSquared)/Power(1-FeccSquared*sin(phi1Rad)*sin(phi1Rad), 1.5);
  D := x/(N1*k0);

  Lat := phi1Rad
         - (N1*tan(phi1Rad)/R1)*(D*D/2
         - (5+3*T1+10*C1-4*C1*C1-9*FeccPrimeSquared)*D*D*D*D/24
	 + (61+90*T1+298*C1+45*T1*T1-252*FeccPrimeSquared-3*C1*C1)*D*D*D*D*D*D/720);
  Lat := Lat * rad2deg;

  Long := (D-(1+2*T1+C1)*D*D*D/6+(5-2*C1+28*T1-3*C1*C1+8*FeccPrimeSquared+24*T1*T1)
	*D*D*D*D*D/120)/cos(phi1Rad);
  Long := LongOrigin + Long * rad2deg;
end;

//Derived procedures Latitude/Longitude to UTM

procedure TUTMLaLo.LaLoToUTM(Lat, Long: TDegMinSec;
                     var UTMNorthing, UTMEasting: Extended;
                     var UTMZone: byte;
                     var UTMZoneLetter: char);
begin
  RealLaLoToUTM(DegMinSec2Extended(Lat), DegMinSec2Extended(Long),
                UTMNorthing, UTMEasting, UTMZone, UTMZoneLetter);
end;

function TUTMLaLo.RealLaLo2UTM(LaLo: TRealLaLoPoint): TUTMPoint;
begin
  with Result do
    RealLaLoToUTM(LaLo.Latitude, LaLo.Longitude,
      Northing, Easting, Zone, ZoneLetter);
end;

function TUTMLaLo.LaLo2UTM(LaLo: TLaLoPoint): TUTMPoint;
begin
  with Result do
    LaLoToUTM(LaLo.Latitude, LaLo.Longitude,
      Northing, Easting, Zone, ZoneLetter);
end;

//Derived procedures UTM to Latitude/Longitude to UTM

procedure TUTMLaLo.UTMtoLaLo(UTMNorthing, UTMEasting: Extended;
                     UTMZone: byte;UTMZoneLetter: char;
                     var Lat, Long: TDegMinSec);
var
  RealLat,
  RealLong: Extended;
begin
  UTMtoRealLaLo(UTMNorthing, UTMEasting, UTMZone, UTMZoneLetter,
    RealLat, RealLong);
  Lat:=Extended2DegMinSec(RealLat);
  Long:=Extended2DegMinSec(RealLong);
end;

function TUTMLaLo.UTM2RealLaLo(UTMPoint: TUTMPoint): TRealLaLoPoint;
begin
  with Result do
    UTMtoRealLaLo(UTMPoint.Northing, UTMPoint.Easting,
      UTMPoint.Zone, UTMPoint.ZoneLetter, Latitude, Longitude);
end;

function TUTMLaLo.UTM2LaLo(UTMPoint: TUTMPoint): TLaLoPoint;
begin
  with Result do
    UTMtoLaLo(UTMPoint.Northing, UTMPoint.Easting,
      UTMPoint.Zone, UTMPoint.ZoneLetter, Latitude, Longitude);
end;

end.

