unit JdcGlobal.DSCommon;

interface

uses
  Classes, SysUtils, FireDAC.Comp.Client, FireDAC.Stan.Intf, Data.DBXPlatform,
  FireDAC.Comp.DataSet
{$IF CompilerVersion  > 26} // upper XE5
    , System.JSON
{$ELSE}
    , Data.DBXJSON
{$ENDIF}
    ;

type
  TDSCommon = class
  private

  public
    // Clear JSONObject Members
    class procedure ClearJSONObject(AObject: TJSONObject); overload;
    class procedure ClearJSONObject(AObject: TJSONArray); overload;

    // FDQuery to TSream
    class function DataSetToStream(ADataSet: TFDQuery): TStream; overload;

    // FDMemTable to TStream
    class function DataSetToStream(ADataSet: TFDMemTable): TStream; overload;

    // TStream to TBytesStream
    class function DSStreamToBytesStream(AValue: TStream): TBytesStream;

    // FDQuery's TStream to FDMemTable
    class procedure StreamToMemTable(AStream: TStream; ATable: TFDMemTable);
      deprecated 'Use StreamToFDDataSet';

    // FDQuery's TStream to TFDDataSet
    class procedure StreamToFDDataSet(AStream: TStream;
      ADataSet: TFDDataSet); static;
  end;

  TFDQueryHelper = class helper for TFDQuery
  public
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);
  end;

  TFDMemTableHelper = class helper for TFDMemTable
  public
    function ToStream: TStream;
    procedure LoadFromDSStream(AStream: TStream);
  end;

implementation

{ TDSCommon }

class procedure TDSCommon.ClearJSONObject(AObject: TJSONArray);
begin
{$IF CompilerVersion  > 26} // upper XE5
  while AObject.Count > 0 do
{$ELSE}
  while AObject.Size > 0 do
{$ENDIF}
  begin
    AObject.Remove(0).Free;
  end;
end;

class function TDSCommon.DataSetToStream(ADataSet: TFDQuery): TStream;
begin
  result := TBytesStream.Create;
  ADataSet.Open;
  ADataSet.FetchAll;
  ADataSet.SaveToStream(result, sfBinary);
  result.Position := 0;
  ADataSet.Close;
end;

class function TDSCommon.DataSetToStream(ADataSet: TFDMemTable): TStream;
begin
  result := TBytesStream.Create;
  ADataSet.SaveToStream(result, sfBinary);
  result.Position := 0;
end;

class procedure TDSCommon.ClearJSONObject(AObject: TJSONObject);
var
  Name: String;
begin
  if not Assigned(AObject) then
    Exit;

{$IF CompilerVersion  > 26} // upper XE5
  while AObject.Count > 0 do
  begin
    Name := AObject.Pairs[0].JsonString.Value;
{$ELSE}
  while AObject.Size > 0 do
  begin
    Name := AObject.Get(0).JsonString.Value;
{$ENDIF}
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

    ATable.LoadFromStream(BytesStream, sfBinary);
  finally
    FreeAndNil(BytesStream);
  end;

end;

class procedure TDSCommon.StreamToFDDataSet(AStream: TStream;
  ADataSet: TFDDataSet);
var
  BytesStream: TBytesStream;
begin
  BytesStream := DSStreamToBytesStream(AStream);
  try
    if BytesStream.Size = 0 then
      raise Exception.Create('Reveiced Null Stream, ' + ADataSet.Name);

    ADataSet.LoadFromStream(BytesStream, sfBinary);
  finally
    FreeAndNil(BytesStream);
  end;

end;

{ TFDDataSetHelper }

{ TFDQueryHelper }
procedure TFDQueryHelper.LoadFromDSStream(AStream: TStream);
begin
  TDSCommon.StreamToFDDataSet(AStream, Self);
end;

function TFDQueryHelper.ToStream: TStream;
begin
  result := TDSCommon.DataSetToStream(Self);
end;

{ TFDMemTableHelper }

procedure TFDMemTableHelper.LoadFromDSStream(AStream: TStream);
begin
  TDSCommon.StreamToFDDataSet(AStream, Self);
end;

function TFDMemTableHelper.ToStream: TStream;
begin
  result := TDSCommon.DataSetToStream(Self);
end;

end.
