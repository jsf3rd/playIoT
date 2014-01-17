unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, DBXJSON;

type
  TDSCommon = class
  public
    // Clear JSONObject Members
    class procedure ClearJSONObject(AObject: TJSONObject);

    // DataSnap으로 전달된 TStream데이터를 TBytesStream으로 변환
    class function DSStreamToBytesStream(AValue: TStream): TBytesStream;

    // DataSnap으로 전달된 FDQuery의 TStream데이터를 FDMemTable로 변환
    class procedure StreamToMemTable(AStream: TStream; ATable: TFDMemTable);
  end;

implementation

{ TDSCommon }

class procedure TDSCommon.ClearJSONObject(AObject: TJSONObject);
var
  Pair: TJSONPair;
  I: integer;
begin
  for I := AObject.Size - 1 downto 0 do
  begin
    Pair := AObject.Get(I);
    AObject.RemovePair(Pair.JsonString.Value).Free;
  end;

end;

class function TDSCommon.DSStreamToBytesStream(AValue: TStream): TBytesStream;
const
  BufferSize = 1024 * 10;
var
  Buffer: TBytes;
  BytesReadCount: integer;
begin

  result := TBytesStream.Create;

  if AValue.Size < 0 then
  begin
    SetLength(Buffer, BufferSize);

    repeat
      BytesReadCount := AValue.Read(Buffer[0], BufferSize);
      if BytesReadCount > 0 then
        result.Write(Buffer[0], BytesReadCount);
    until BytesReadCount < BufferSize;
  end
  else
  begin
    result := TBytesStream.Create;
    result.LoadFromStream(AValue);
  end;

  result.Position := 0;
end;

class procedure TDSCommon.StreamToMemTable(AStream: TStream;
  ATable: TFDMemTable);
var
  BytesStream: TBytesStream;
begin

  BytesStream := DSStreamToBytesStream(AStream);

  ATable.Close;
  ATable.DisableControls;
  ATable.LoadFromStream(BytesStream, sfBinary);
  ATable.EnableControls;

  BytesStream.Free;
end;

end.
