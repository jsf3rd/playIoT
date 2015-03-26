unit JdcGlobal;

interface

uses
  Classes, SysUtils, Windows, ZLib, IdGlobal, IOUtils, StdCtrls, JclFileUtils;

// 로그 찍기..
procedure PrintLog(const AFile, AMessage: String); overload;
procedure PrintLog(AMemo: TMemo; const AMsg: String); overload;

procedure PrintDebug(const Format: string; const Args: array of const);
  overload;
procedure PrintDebug(const str: string); overload;

// 데이터 압축..
function CompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;

// 데이터 압축 해제..
function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;

// 응답 검사..
function Contains(Contents: string; const str: array of const): boolean;
function IsGoodResponse(Text, Command: string;
  Response: array of const): boolean;

// Reverse 2Btyes..
function Rev2Bytes(w: WORD): WORD;

// Reverse 4Btyes..
function Rev4Bytes(Value: LongInt): LongInt;

// Reverse 4Btyes..
function Rev4BytesF(Value: LongInt): Single;

// Big endian
function WordToBytes(AValue: WORD): TIdBytes;

// Big endian
function DWordToBytes(AValue: DWORD): TIdBytes;

// little endian
function HexStrToWord(const ASource: string; const AIndex: integer = 1): WORD;

function HexStrToByte(const ASource: String; const AIndex: integer = 1): Byte;
function HexStrToBytes(const ASource: string; const AIndex: integer = 1)
  : TIdBytes;

function IdBytesToHex(const AValue: TIdBytes;
  const ASpliter: String = ' '): String;
function BytesToHex(const AValue: TBytes; const ASpliter: String = ' '): String;

function IdBytesPos(const SubIdBytes, IdBytes: TIdBytes;
  const AIndex: integer = 0): integer;

type
  IExecuteFunc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    function Execute(AValue: String): T;
  End;

  IExecuteProc<T> = Interface
    ['{48E4B912-AE21-4201-88E0-4835432FEE69}']
    procedure Execute(AValue: T);
  End;

implementation

function IdBytesPos(const SubIdBytes, IdBytes: TIdBytes;
  const AIndex: integer = 0): integer;
var
  Index: integer;
  I: integer;
begin
  Index := ByteIndex(SubIdBytes[0], IdBytes, AIndex);
  if Index = -1 then
    Exit(-1);

  for I := 0 to Length(SubIdBytes) - 1 do
  begin
    if IdBytes[Index + I] <> SubIdBytes[I] then
      Exit(IdBytesPos(SubIdBytes, IdBytes, Index + I));
  end;
  result := Index;
end;

function IdBytesToHex(const AValue: TIdBytes; const ASpliter: String): String;
var
  I: integer;
begin
  result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    result := result + ByteToHex(AValue[I]) + ASpliter;
  end;
end;

function BytesToHex(const AValue: TBytes; const ASpliter: String): String;
var
  I: integer;
begin
  result := '';
  for I := 0 to Length(AValue) - 1 do
  begin
    result := result + ByteToHex(AValue[I]) + ASpliter;
  end;
end;

procedure PrintLog(AMemo: TMemo; const AMsg: String);
begin
  if AMemo.Lines.Count > 3000 then
    AMemo.Lines.Clear;

  if AMsg = '' then
    AMemo.Lines.Add('')
  else
    AMemo.Lines.Add(FormatDateTime('YYYY-MM-DD, HH:NN:SS.zzz, ', now) + AMsg);
end;

procedure PrintLog(const AFile, AMessage: String);
var
  Stream: TStreamWriter;
  FileName: String;
begin
  FileName := AFile;

  if FileExists(FileName) then
  begin
    if JclFileUtils.FileGetSize(FileName) > 1024 * 1024 * 5 then
    begin
      try
        FileMove(AFile, ChangeFileExt(FileName,
          FormatDateTime('_YYYYMMDD_HHNNSS', now) + '.bak'), true);
      except
        on E: Exception do
          FileName := ChangeFileExt(FileName, FormatDateTime('_YYYYMMDD', now)
            + '.tmp');
      end;
    end;
  end;

  try
    Stream := TFile.AppendText(FileName);
    try
      Stream.WriteLine(FormatDateTime('YYYY-MM-DD, HH:NN:SS.zzz, ', now) +
        AMessage);
    finally
      FreeAndNil(Stream);
    end;
  except
    on E: Exception do
      //
  end;

end;

procedure PrintDebug(const Format: string; const Args: array of const);
  overload;
var
  str: string;
begin
  FmtStr(str, Format, Args);
  OutputDebugString(PChar('[JDC] ' + str));
end;

procedure PrintDebug(const str: string); overload;
begin
  PrintDebug(str, []);
end;

function CompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;
var
  CS: TZCompressionStream;
begin
  CS := TZCompressionStream.Create(OutStream); // 스트림 생성
  try
    if Assigned(OnProgress) then
      CS.OnProgress := OnProgress;
    CS.CopyFrom(Stream, Stream.Size); // 여기서 압축이 진행됨
    // 테스트 결과 압축완료시 이벤트가 발생하지 않기 때문에
    // 완료시 한번 더 이벤트를 불러준다.
    if Assigned(OnProgress) then
      OnProgress(CS);
    result := true;
  finally
    CS.Free;
  end;
end;

function Contains(Contents: string; const str: array of const): boolean;
var
  I: integer;
begin
  result := False;

  for I := 0 to High(str) do
  begin
    if Pos(str[I].VPWideChar, Contents) = 0 then
      Exit;
  end;

  result := true;
end;

function IsGoodResponse(Text, Command: string;
  Response: array of const): boolean;
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := Text;

    result := (SL.Strings[0] = Command) and (Contains(Text, Response));
  finally
    SL.Free;
  end;
end;

function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;
const
  BuffSize = 65535; // 버퍼 사이즈
var
  DS: TZDeCompressionStream;
  Buff: PChar; // 임시 버퍼
  ReadSize: integer; // 읽은 크기
begin
  if Stream = OutStream then
    // 입력 스트림과 출력스트림이 같으면 문제가 발생한다
    raise Exception.Create('입력 스트림과 출력 스트림이 같습니다');
  Stream.Position := 0;
  // 스트림 커서 초기화
  OutStream.Position := 0;
  // 인풋 스트림을 옵션으로 객체 생성.
  DS := TZDeCompressionStream.Create(Stream);
  try
    if Assigned(OnProgress) then
      DS.OnProgress := OnProgress;
    GetMem(Buff, BuffSize);
    try
      // 버퍼 사이즈만큼 읽어온다. Read함수를 부르면 압축이 풀리게 된다.
      repeat
        ReadSize := DS.Read(Buff^, BuffSize);
        if ReadSize <> 0 then
          OutStream.Write(Buff^, ReadSize);
      until ReadSize < BuffSize;
      if Assigned(OnProgress) then
        OnProgress(DS);
      // Compress와 같은이유
      result := true;
    finally
      FreeMem(Buff)
    end;
  finally
    DS.Free;
  end;
end;

function Rev2Bytes(w: WORD): WORD;
asm
  xchg   al, ah
end;

function Rev4Bytes(Value: LongInt): LongInt;
asm
  BSWAP    EAX;
end;

function Rev4BytesF(Value: LongInt): Single;
var
  tmp: LongInt;
begin
  tmp := Rev4Bytes(Value);
  CopyMemory(@result, @tmp, SizeOf(tmp));
end;

function CheckHexStr(ASource: String): String;
begin
  if (Length(ASource) mod 2) = 0 then
    result := ASource
  else
    result := '0' + ASource;
end;

function HexStrToByte(const ASource: String; const AIndex: integer): Byte;
var
  str: String;
  tmp: TIdBytes;
begin
  str := CheckHexStr(ASource);

  if Length(str) < AIndex + 1 then
  begin
    result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 2);
  tmp := HexStrToBytes(str);
  CopyMemory(@result, tmp, 1);
end;

function WordToBytes(AValue: WORD): TIdBytes;
begin
  result := ToBytes(Rev2Bytes(AValue));
end;

function DWordToBytes(AValue: DWORD): TIdBytes;
begin
  result := ToBytes(Rev4Bytes(AValue));
end;

function HexStrToWord(const ASource: string; const AIndex: integer): WORD;
var
  str: string;
begin
  str := CheckHexStr(ASource);

  if Length(str) = 2 then
    str := '00' + str;

  if Length(str) < AIndex + 3 then
  begin
    result := $00;
    Exit;
  end;

  str := Copy(str, AIndex, 4);
  result := BytesToWord(HexStrToBytes(str));
end;

function HexStrToBytes(const ASource: string; const AIndex: integer): TIdBytes;
var
  I, j, n: integer;
  c: char;
  b: Byte;
  str: string;
begin
  str := CheckHexStr(ASource);

  SetLength(result, 0);

  j := 0;
  b := 0;
  n := 0;

  for I := AIndex to Length(str) do
  begin
    c := ASource[I];
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

      AppendBytes(result, ToBytes(b));
    end
  end;

  if j <> 0 then
    raise Exception.Create
      ('Input contains an odd number of hexadecimal digits.[' + ASource + '/' +
      IntToStr(AIndex) + ']');
end;

end.
