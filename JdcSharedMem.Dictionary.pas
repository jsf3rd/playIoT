unit JdcSharedMem.Dictionary;

interface

uses System.Classes, System.SysUtils, Winapi.Windows, JdcSharedMem.Common, JdcLogging,
  System.Generics.Collections, SharedMMFMem;

type
  TSharedMemDic = class
  private
    FReader: TDictionary<String, Pointer>;
    FWriter: TDictionary<String, Pointer>;
    function GetItems(Key: string): Double;
    procedure SetItems(Key: string; const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    property Items[Key: string]: Double read GetItems write SetItems;
  end;

implementation

{ TSharedMemDic }

const
  MEM_DIC = '_MEM_DIC';

constructor TSharedMemDic.Create;
begin
  FReader := TDictionary<String, Pointer>.Create;
  FWriter := TDictionary<String, Pointer>.Create;
end;

destructor TSharedMemDic.Destroy;
var
  MyElem: TPair<String, Pointer>;
  MyPointer: Pointer;
begin
  for MyElem in FReader do
  begin
    MyPointer := MyElem.Value;
    SharedFreeMem(MyPointer);
  end;
  FReader.Free;

  for MyElem in FWriter do
  begin
    MyPointer := MyElem.Value;
    SharedFreeMem(MyPointer);
  end;
  FWriter.Free;

  inherited;
end;

function TSharedMemDic.GetItems(Key: string): Double;
var
  p: Pointer;
  MyKey: string;
begin
  MyKey := Key + MEM_DIC;
  if FReader.ContainsKey(MyKey) then
    CopyMemory(@result, FReader.Items[MyKey], SizeOf(Double))
  else
  begin
    if SharedOpenMem(p, MyKey, FILE_MAP_READ) then
    begin
      FReader.Add(MyKey, p);
      CopyMemory(@result, FReader.Items[MyKey], SizeOf(Double));
    end
    else
      result := -9999;
  end;
end;

procedure TSharedMemDic.SetItems(Key: string; const Value: Double);
var
  p: Pointer;
  MyKey: string;
begin
  MyKey := Key + MEM_DIC;
  if not FReader.ContainsKey(MyKey) then
  begin
    p := SharedAllocMem(MyKey, SizeOf(Double));
    FReader.Add(MyKey, p);
  end;
  p := FReader.Items[MyKey];
  CopyMemory(p, @Value, SizeOf(Double))
end;

end.
