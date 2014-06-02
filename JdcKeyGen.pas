unit JdcKeyGen;

interface

uses
  System.Classes, System.SysUtils, IdGlobal, JclSysInfo, JdcGlobal, Q330.CRC32,
  IdHashMessageDigest, System.Types;

type
  TKeyPair = record
    PCCode: string;
    EnbKey: String;

    Constructor Create(APCCode, AEnbKey: String);
    function Open(AProjectIndex: integer): boolean;
  end;

  TJdcKeyGen = class
  const
    PROJECT_PSMR = 0;
    ProjectCode: array [0 .. 0] of integer = (317781909 // PSMR
      );

  public
    class function GetPCCode(ADrive: String): String;
    class function CalcMD5(AValue: string): TIdBytes;
    class function GenerateEnbKey(AValue: string;
      AProjectIndex: integer): string;
  end;

implementation

Constructor TKeyPair.Create(APCCode, AEnbKey: String);
begin
  Self.PCCode := APCCode;
  Self.EnbKey := AEnbKey;
end;

function TKeyPair.Open(AProjectIndex: integer): boolean;
begin
  result := Self.EnbKey = TJdcKeyGen.GenerateEnbKey(Self.PCCode, AProjectIndex);
end;

class function TJdcKeyGen.GenerateEnbKey(AValue: string;
  AProjectIndex: integer): string;
var
  Buffer: TIdBytes;
begin
  AppendBytes(Buffer, ToBytes(AValue));
  AppendBytes(Buffer, ToBytes(ProjectCode[AProjectIndex]));
  AppendBytes(Buffer, ToBytes(CalcCRC32(Buffer)));
  Buffer := CalcMD5(IdBytesToHex(Buffer, ''));
  RemoveBytes(Buffer, 1, 15);
  RemoveBytes(Buffer, 1, 13);
  RemoveBytes(Buffer, 1, 11);
  RemoveBytes(Buffer, 1, 9);
  RemoveBytes(Buffer, 1, 7);
  RemoveBytes(Buffer, 1, 5);
  RemoveBytes(Buffer, 1, 3);
  RemoveBytes(Buffer, 1, 1);
  result := IdBytesToHex(Buffer, '');
end;

class function TJdcKeyGen.CalcMD5(AValue: string): TIdBytes;
var
  IdMD5: TIdHashMessageDigest5;
  str: string;
begin
  Setlength(result, 16);
  IdMD5 := TIdHashMessageDigest5.Create;
  try
    str := LowerCase(AValue);
    result := IdMD5.HashString(str);
  finally
    IdMD5.Free;
  end;
end;

class function TJdcKeyGen.GetPCCode(ADrive: String): String;
var
  tmp: string;
  Buffer: TIdBytes;
begin
  tmp := GetVolumeSerialNumber(ADrive.Substring(0, 1));
  Buffer := ToBytes(tmp);
  result := IntToStr(Rev2Bytes(CalcCRC32(Buffer)));
end;

end.
