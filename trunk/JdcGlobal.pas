unit JdcGlobal;

interface

uses
  ValueList,
  Classes, SysUtils, Windows, ZLib, IdGlobal, IOUtils;

// �α� ���..
procedure PrintLog(AFile, AMessage: String);

// ������ ����..
function CompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;

// ������ ���� ����..
function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;

// ���� �˻�..
function Contains(Contents: string; const str: array of const): boolean;
function IsGoodResponse(Text, Command: string;
  Response: array of const): boolean;

// Reverse 4Btye..
function Rev4Bytes(Value: LongInt): LongInt;

function HexStrToByte(const ASource: String; const AIndex: integer = 1): Byte;
function HexStrToWord(const ASource: string; const AIndex: integer = 1): Word;
function HexStrToBytes(const ASource: string;
  const AIndex: integer = 1): TBytes;

implementation

procedure PrintLog(AFile, AMessage: String);
var
  FileHandle: integer;
  S: AnsiString;

  SL: TStringList;
begin

  if FileExists(AFile) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(AFile);

      if SL.Count > 100000 then
      begin
        SL.Clear;
        SL.SaveToFile(AFile);
      end;
    finally
      SL.Free;
    end;

    FileHandle := FileOpen(AFile, fmOpenWrite);
  end
  else
  begin
    FileHandle := FileCreate(AFile);
  end;

  try
    FileSeek(FileHandle, 0, 2);
    S := AnsiString(AMessage) + #13#10;
    FileWrite(FileHandle, S[1], Length(S));
  finally
    FileClose(FileHandle);
  end;

end;

function CompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;
var
  CS: TZCompressionStream;
begin
  CS := TZCompressionStream.Create(OutStream); // ��Ʈ�� ����
  try
    if Assigned(OnProgress) then
      CS.OnProgress := OnProgress;
    CS.CopyFrom(Stream, Stream.Size); // ���⼭ ������ �����
    // �׽�Ʈ ��� ����Ϸ�� �̺�Ʈ�� �߻����� �ʱ� ������
    // �Ϸ�� �ѹ� �� �̺�Ʈ�� �ҷ��ش�.
    if Assigned(OnProgress) then
      OnProgress(CS);
    Result := True;
  finally
    CS.Free;
  end;
end;

function Contains(Contents: string; const str: array of const): boolean;
var
  i: integer;
begin
  Result := False;

  for i := 0 to High(str) do
  begin
    if Pos(str[i].VPWideChar, Contents) = 0 then
      Exit;
  end;

  Result := True;
end;

function IsGoodResponse(Text, Command: string;
  Response: array of const): boolean;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := Text;

    Result := (sl.Strings[0] = Command) and (Contains(Text, Response));
  finally
    sl.Free;
  end;
end;

function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;
const
  BuffSize = 65535; // ���� ������
var
  DS: TZDeCompressionStream;
  Buff: PChar; // �ӽ� ����
  ReadSize: integer; // ���� ũ��
begin
  if Stream = OutStream then
    // �Է� ��Ʈ���� ��½�Ʈ���� ������ ������ �߻��Ѵ�
    raise Exception.Create('�Է� ��Ʈ���� ��� ��Ʈ���� �����ϴ�');
  Stream.Position := 0; // ��Ʈ�� Ŀ�� �ʱ�ȭ
  OutStream.Position := 0;
  // ��ǲ ��Ʈ���� �ɼ����� ��ü ����.
  DS := TZDeCompressionStream.Create(Stream);
  try
    if Assigned(OnProgress) then
      DS.OnProgress := OnProgress;
    GetMem(Buff, BuffSize);
    try
      // ���� �����ŭ �о�´�. Read�Լ��� �θ��� ������ Ǯ���� �ȴ�.
      repeat
        ReadSize := DS.Read(Buff^, BuffSize);
        if ReadSize <> 0 then
          OutStream.Write(Buff^, ReadSize);
      until ReadSize < BuffSize;
      if Assigned(OnProgress) then
        OnProgress(DS); // Compress�� ��������
      Result := True;
    finally
      FreeMem(Buff)
    end;
  finally
    DS.Free;
  end;
end;

function Rev4Bytes(Value: LongInt): LongInt;
asm
  BSWAP    EAX;
end;

function HexStrToByte(const ASource: String; const AIndex: integer): Byte;
var
  str: String;
  tmp: TBytes;
begin

  str := ASource;

  if Length(str) = 1 then
    str := '0' + str;

  if Length(str) < AIndex + 1 then
  begin
    Result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 2);
  tmp := HexStrToBytes(str);
  CopyMemory(@Result, tmp, 1);
end;

function HexStrToWord(const ASource: string; const AIndex: integer): Word;
var
  str: string;
begin

  str := ASource;

  if Length(str) = 1 then
    str := '000' + str;

  if Length(str) < AIndex + 3 then
  begin
    Result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 4);
  Result := BytesToWord(HexStrToBytes(str));
end;

function HexStrToBytes(const ASource: string; const AIndex: integer): TIdBytes;
var
  i, j, n: integer;
  c: char;
  b: Byte;
begin
  SetLength(Result, 0);

  j := 0;
  b := 0;
  n := 0;

  for i := AIndex to Length(ASource) do
  begin
    c := ASource[i];
    case c of
      '0' .. '9':
        n := ord(c) - ord('0');
      'A' .. 'F':
        n := ord(c) - ord('A') + 10;
      'a' .. 'f':
        n := ord(c) - ord('a') + 10;
    else
      Continue;
    end;

    if j = 0 then
    begin
      b := n;
      j := 1;
    end
    else
    begin
      b := (b shl 4) + n;
      j := 0;

      AppendBytes(Result, ToBytes(b));
    end
  end;

  if j <> 0 then
    raise Exception.Create
      ('Input contains an odd number of hexadecimal digits.[' + ASource + '/' +
      IntToStr(AIndex) + ']');
end;

end.
