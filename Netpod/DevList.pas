unit DevList;

interface

uses
  SysUtils, Classes;

type
  TChannelCollectionItem = class(TCollectionItem)
    private
      FNo              : Integer;
      FName            : string;
      FUnits           : string;
      FResolutiuon     : Integer;
      FPartNumber      : string;
      FSerialNumber    : string;
      FManufactureDate : string;
      FData            : Double;
      FGain            : Double;
      FIsInstalled     : Boolean;
      FSWVersion       : Integer;

      procedure SetNo(const Value: Integer);
      procedure SetName(const Value: string);
      procedure SetUnits(const Value: string);
      procedure SetResolutiuon(const Value: Integer);
      procedure SetPartNumber(const Value: string);
      procedure SetSerialNumber(const Value: string);
      procedure SetManufactureDate(const Value: string);
      procedure SetData(const Value: Double);
      procedure SetGain(const Value: Double);
      procedure SetIsInstalled(const Value: Boolean);
      procedure SetSWVersion(const Value: Integer);
    public
      procedure AssignParameter(const ANo: Integer; AName, AUnits: string; AResolutiuon: Integer; APartNumber, ASerialNumber, AManufactureDate: string; AData, AGain: double; AIsInstalled: Boolean; ASWVersion: Integer); virtual;
    published
      property No: Integer read FNo write SetNo;
      property Name: string read FName write SetName;
      property Units: string read FUnits write SetUnits;
      property Resolutiuon: Integer read FResolutiuon write SetResolutiuon;
      property PartNumber: string read FPartNumber write SetPartNumber;
      property SerialNumber: string read FSerialNumber write SetSerialNumber;
      property ManufactureDate: string read FManufactureDate write SetManufactureDate;
      property Data: Double read FData write SetData;
      property Gain: Double read FGain write SetGain;
      property IsInstalled: Boolean read FIsInstalled write SetIsInstalled;
      property SWVersion: Integer read FSWVersion write SetSWVersion;
  end;

  TChannelCollection = class(TCollection)
  private
    protected
      function GetItem(Index: Integer): TChannelCollectionItem; virtual;
      procedure SetItem(Index: Integer; Value: TChannelCollectionItem); virtual;
    public
      constructor Create;
//      function IndexOf(const ACaption: String): Integer; virtual;
      function IndexOf(const ANo: Integer; const AName: string = ''): Integer; virtual;
      function Add: TChannelCollectionItem;
//      procedure AddParameter(const AName, AValue: String; const ATag: String = '');
      procedure AddItem(const ANo: Integer; AName, AUnits: string; AResolutiuon: Integer; APartNumber, ASerialNumber, AManufactureDate: string; AData, AGain: double; AIsInstalled: Boolean; ASWVersion: Integer);
      //procedure DeleteParameter(const No: Integer); overload;
      procedure DeleteParameter(const idx: Integer); overload;

      property Items[ Index: Integer ] : TChannelCollectionItem read GetItem write SetItem;
  end;

  


  TDevCollectionItem = class(TCollectionItem)
    private
      FID              : Integer;
      FName            : string;
      FSampleRate      : Double;
      FSWVersion       : Integer;
      FIP              : string;
      FEthernetAddress : string;
      FPartNumber      : string;
      FSerialNumber    : string;
      FManufactureDate : string;
      FDigitalDirection: Integer;
      FDigitalDefault  : Integer;
      FChannel         : TChannelCollection;

      procedure SetID(const Value: Integer);
      procedure SetName(const Value: string);
      procedure SetSampleRate(const Value: Double);
      procedure SetSWVersion(const Value: Integer);
      procedure SetIP(const Value: string);
      procedure SetEthernetAddress(const Value: string);
      procedure SetPartNumber(const Value: string);
      procedure SetSerialNumber(const Value: string);
      procedure SetManufactureDate(const Value: string);
      procedure SetDigitalDirection(const Value: Integer);
      procedure SetDigitalDefault(const Value: Integer);
      procedure SetChannel(const Value: TChannelCollection);
    public
      procedure AssignParameter(const AID: Integer; AName: string; ASampleRate: Double; ASWVersion: Integer; AIP, AEthernetAddress, APartNumber, ASerialNumber, AManufactureDate: string; ADigitalDirection, ADigitalDefault: Integer; AChannel: TChannelCollection); virtual;
    published
      property ID: Integer read FID write SetID;
      property Name: string read FName write SetName;
      property SampleRate: Double read FSampleRate write SetSampleRate;
      property SWVersion: Integer read FSWVersion write SetSWVersion;
      property IP: string read FIP write SetIP;
      property EthernetAddress: string read FEthernetAddress write SetEthernetAddress;
      property PartNumber: string read FPartNumber write SetPartNumber;
      property SerialNumber: string read FSerialNumber write SetSerialNumber;
      property ManufactureDate: string read FManufactureDate write SetManufactureDate;
      property DigitalDirection: Integer read FDigitalDirection write SetDigitalDirection;
      property DigitalDefault: Integer read FDigitalDefault write SetDigitalDefault;
      property Channel: TChannelCollection read FChannel write SetChannel;
  end;
    
  TDevCollection = class(TCollection)
  private
    protected
      function GetItem(Index: Integer): TDevCollectionItem; virtual;
      procedure SetItem(Index: Integer; Value: TDevCollectionItem); virtual;
    public
      constructor Create;
//      function IndexOf(const ACaption: String): Integer; virtual;
      function IndexOf(const AID: Integer; const AName: string = ''): Integer; virtual;
      function Add: TDevCollectionItem;
//      procedure AddParameter(const AName, AValue: String; const ATag: String = '');
      procedure AddItem(const AID: Integer; AName: string; ASampleRate: Double; ASWVersion: Integer; AIP, AEthernetAddress, APartNumber, ASerialNumber, AManufactureDate: string; ADigitalDirection, ADigitalDefault: Integer; AChannel: TChannelCollection);
      //procedure DeleteParameter(const No: Integer); overload;
      procedure DeleteParameter(const idx: Integer); overload;

      property Items[ Index: Integer ] : TDevCollectionItem read GetItem write SetItem;
  end;


implementation

{ TChannelCollectionItem }
{
procedure TMyCollectionItem.AssignParameter(const AName, AValue: String; const ATag: String = '');
begin
  FName  := AName;
  FValue  := AValue;
  FTag := ATag;
end;

procedure TMyCollectionItem.SetName(const Value: String);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TMyCollectionItem.SetValue(const Value: String);
begin
  if FValue <> Value then
    FValue := Value;
end;

procedure TMyCollectionItem.SetTag(const Value: String);
begin
  if FTag <> Value then
    FTag := Value;
end;
}

procedure TChannelCollectionItem.SetData(const Value: Double);
begin
  if FData <> Value then
    FData := Value;
end;

procedure TChannelCollectionItem.SetGain(const Value: Double);
begin
  if FGain <> Value then
    FGain := Value;
end;

procedure TChannelCollectionItem.SetIsInstalled(const Value: Boolean);
begin
  if FIsInstalled <> Value then
    FIsInstalled := Value;
end;

procedure TChannelCollectionItem.SetManufactureDate(const Value: string);
begin
  if FManufactureDate <> Value then
    FManufactureDate := Value;
end;

procedure TChannelCollectionItem.SetName(const Value: string);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TChannelCollectionItem.SetNo(const Value: Integer);
begin
  if FNo <> Value then
    FNo := Value;
end;

procedure TChannelCollectionItem.SetPartNumber(const Value: string);
begin
  if FPartNumber <> Value then
    FPartNumber := Value;
end;

procedure TChannelCollectionItem.SetResolutiuon(const Value: Integer);
begin
  if FResolutiuon <> Value then
    FResolutiuon := Value;
end;

procedure TChannelCollectionItem.SetSerialNumber(const Value: string);
begin
  if FSerialNumber <> Value then
    FSerialNumber := Value;
end;

procedure TChannelCollectionItem.SetUnits(const Value: string);
begin
  if FUnits <> Value then
    FUnits := Value;
end;

procedure TChannelCollectionItem.SetSWVersion(const Value: Integer);
begin
  if FSWVersion <> Value then
    FSWVersion := Value;
end;

procedure TChannelCollectionItem.AssignParameter(const ANo: Integer; AName, AUnits: string; AResolutiuon: Integer; APartNumber, ASerialNumber, AManufactureDate: string; AData, AGain: double; AIsInstalled: Boolean; ASWVersion: Integer);
begin
  No              := ANo;
  Name            := AName;
  Units           := AUnits;
  Resolutiuon     := AResolutiuon;
  PartNumber      := APartNumber;
  SerialNumber    := ASerialNumber;
  ManufactureDate := AManufactureDate;
  Data            := AData;
  Gain            := AGain;
  IsInstalled     := AIsInstalled;
end;

{ TDevCollection }

function TChannelCollection.Add: TChannelCollectionItem;
begin
  Result := TChannelCollectionItem(inherited Add);
end;

procedure TChannelCollection.AddItem(const ANo: Integer; AName, AUnits: string; AResolutiuon: Integer; APartNumber, ASerialNumber, AManufactureDate: string; AData, AGain: double; AIsInstalled: Boolean; ASWVersion: Integer);
begin
  Add.AssignParameter(ANo, AName, AUnits, AResolutiuon, APartNumber, ASerialNumber, AManufactureDate, AData, AGain, AIsInstalled, ASWVersion);
end;
{
procedure TMyCollection.AddParameter(const AName, AValue: String; const ATag: String = '');
begin
  Add.AssignParameter(AName, AValue, ATag);
end;
}
constructor TChannelCollection.Create;
begin
  inherited Create(TChannelCollectionItem);
end;
{
procedure TMyCollection.DeleteParameter(const Name: String);
begin
  Items[ IndexOf(Name) ].Free;
end;
}
procedure TChannelCollection.DeleteParameter(const idx: Integer);
begin
  Items[idx].Free;
end;

function TChannelCollection.GetItem(Index: Integer): TChannelCollectionItem;
begin
  Result := TChannelCollectionItem(inherited GetItem(Index));
end;
{
function TMyCollection.IndexOf(const ACaption: String): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result].Caption = ACaption then
      exit;
  result := -1;
//  raise Exception.CreateFmt('Error: Parameter "%s" does not exist', [AName]);
end;
}
function TChannelCollection.IndexOf(const ANo: Integer; const AName: string = ''): Integer;
begin
  for Result := 0 to Count - 1 do
    if (Items[Result].No = ANo) and (((AName <> '') and (Pos(AName, Items[Result].Name) > 0)) or (AName = '')) then
      exit;
  result := -1;
//  raise Exception.CreateFmt('Error: Parameter "%s" does not exist', [AName]);
end;

procedure TChannelCollection.SetItem(Index: Integer; Value: TChannelCollectionItem);
begin
  inherited SetItem(Index, Value);
end;







{ TDevCollectionItem }

procedure TDevCollectionItem.AssignParameter(const AID: Integer;
  AName: string; ASampleRate: Double; ASWVersion: Integer; AIP,
  AEthernetAddress, APartNumber, ASerialNumber, AManufactureDate: string;
  ADigitalDirection, ADigitalDefault: Integer; AChannel: TChannelCollection);
begin
  ID              := AID;
  Name            := AName;
  SampleRate      := ASampleRate;
  SWVersion       := ASWVersion;
  IP              := AIP;
  EthernetAddress := AEthernetAddress;
  PartNumber      := APartNumber;
  SerialNumber    := ASerialNumber;
  ManufactureDate := AManufactureDate;
  DigitalDirection:= ADigitalDirection;
  DigitalDefault  := ADigitalDefault;
  Channel         := AChannel;
end;

procedure TDevCollectionItem.SetChannel(const Value: TChannelCollection);
begin
  if FChannel <> Value then
    FChannel := Value;
end;

procedure TDevCollectionItem.SetDigitalDefault(const Value: Integer);
begin
  if FDigitalDefault <> Value then
    FDigitalDefault := Value;
end;

procedure TDevCollectionItem.SetDigitalDirection(const Value: Integer);
begin
  if FDigitalDirection <> Value then
    FDigitalDirection := Value;
end;

procedure TDevCollectionItem.SetEthernetAddress(const Value: string);
begin
  if FEthernetAddress <> Value then
    FEthernetAddress := Value;
end;

procedure TDevCollectionItem.SetID(const Value: Integer);
begin
  if FID <> Value then
    FID := Value;
end;

procedure TDevCollectionItem.SetIP(const Value: string);
begin
  if FIP <> Value then
    FIP := Value;
end;

procedure TDevCollectionItem.SetManufactureDate(const Value: string);
begin
  if FManufactureDate <> Value then
    FManufactureDate := Value;
end;

procedure TDevCollectionItem.SetName(const Value: string);
begin
  if FName <> Value then
    FName := Value;
end;

procedure TDevCollectionItem.SetPartNumber(const Value: string);
begin
  if FPartNumber <> Value then
    FPartNumber := Value;
end;

procedure TDevCollectionItem.SetSampleRate(const Value: Double);
begin
  if FSampleRate <> Value then
    FSampleRate:= Value;
end;

procedure TDevCollectionItem.SetSerialNumber(const Value: string);
begin
  if FSerialNumber <> Value then
    FSerialNumber := Value;
end;

procedure TDevCollectionItem.SetSWVersion(const Value: Integer);
begin
  if FSWVersion <> Value then
    FSWVersion := Value;
end;


{ TDevCollection }

function TDevCollection.Add: TDevCollectionItem;
begin
  Result := TDevCollectionItem(inherited Add);
end;

procedure TDevCollection.AddItem(const AID: Integer; AName: string;
  ASampleRate: Double; ASWVersion: Integer; AIP, AEthernetAddress,
  APartNumber, ASerialNumber, AManufactureDate: string; ADigitalDirection,
  ADigitalDefault: Integer; AChannel: TChannelCollection);
begin
  Add.AssignParameter(AID, AName, ASampleRate, ASWVersion, AIP, AEthernetAddress, APartNumber, ASerialNumber, AManufactureDate, ADigitalDirection, ADigitalDefault, AChannel);
end;

constructor TDevCollection.Create;
begin
  inherited Create(TDevCollectionItem);
end;

procedure TDevCollection.DeleteParameter(const idx: Integer);
begin
  Items[idx].Free;
end;

function TDevCollection.GetItem(Index: Integer): TDevCollectionItem;
begin
  Result := TDevCollectionItem(inherited GetItem(Index));
end;

function TDevCollection.IndexOf(const AID: Integer;
  const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if (Items[Result].ID = AID) and (((AName <> '') and (Pos(AName, Items[Result].Name) > 0)) or (AName = '')) then
      exit;
  result := -1;
end;

procedure TDevCollection.SetItem(Index: Integer;
  Value: TDevCollectionItem);
begin
  inherited SetItem(Index, Value);
end;


end.
