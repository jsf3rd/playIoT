unit MeasureDataFormat;

interface

uses System.Classes, System.SysUtils;

type
  TCodeName = array [0 .. 9] of char;

  TSensorInfo = record
    SensorID: Integer;
    SensorCode: String;
    ElementCout: Integer;
    SampleCount: Integer;
  end;

  TElementData = record
    ElementID: Integer;
    ElementCode: TCodeName;
    Values: TArray<Double>;
  end;

  TElement = TArray<TElementData>;

  TMeasureData = record
    SensorID: Integer;
    SensorCode: TCodeName;
    MeasureDate: TDateTime;
    Elements: TArray<TElement>;
  public
    constructor Create(ASensorInfo: TSensorInfo; ADateTime: TDateTime);
  end;

  TMeasureDataEncoder = class
  private
  public
  end;

implementation

{ TMeasureData }

constructor TMeasureData.Create(ASensorInfo: TSensorInfo; ADateTime: TDateTime);
var
  I: Integer;
begin
  Self.SensorID := ASensorInfo.SensorID;
  for I := 0 to Length(ASensorInfo.SensorCode) do
    Self.SensorCode[I] := ASensorInfo.SensorCode[I + 1];
  Self.MeasureDate := ADateTime;
end;

end.
