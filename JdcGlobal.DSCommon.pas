unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DBXPlatform
{$IF CompilerVersion  > 26}
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

type
  TDSCommon = class
  public
    // Clear JSONObject Members
    class procedure ClearJSONObject(AObject: TJSONObject); overload;
    class procedure ClearJSONObject(AObject: TJSONArray); overload;

    // FDQuery To TSream
    class function DataSetToStream(AQuery: TFDQuery): TStream;

    // TStream to TBytesStream
    class function DSStreamToBytesStream(AValue: TStream): TBytesStream;

    // FDQuery's TStream to FDMemTable
    class procedure StreamToMemTable(AStream: TStream; ATable: TFDMemTable);
  end;

implementation

{ TDSCommon }

class procedure TDSCommon.ClearJSONObject(AObject: TJSONArray);
begin
  while AObject.Size > 0 do
  begin
    AObject.Remove(0).Free;
  end;
end;

class function TDSCommon.DataSetToStream(AQuery: TFDQuery): TStream;
begin
  result := TBytesStream.Create;
  AQuery.Open;
  AQuery.FetchAll;
  AQuery.SaveToStream(result, sfBinary);
  result.Position := 0;
  AQuery.Close;
end;

class procedure TDSCommon.ClearJSONObject(AObject: TJSONObject);
var
  Name: String;
begin
  while AObject.Size > 0 do
  begin
    Name := AObject.Get(0).JsonString.Value;
    AObject.RemovePair(Name).Free;
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
  try
    if BytesStream.Size = 0 then
      raise Exception.Create('Reveiced Null Stream, ' + ATable.Name);

    ATable.Close;
    ATable.DisableControls;
    ATable.LoadFromStream(BytesStream, sfBinary);
    ATable.EnableControls;
  finally
    FreeAndNil(BytesStream);
  end;

end;

end.
