unit JdcGlobal;

interface

uses
  ValueList,
  Classes, SysUtils, Windows, ZLib;

// 로그 찍기..
procedure PrintLog(AFile, AMessage: String);

// 데이터 압축..
function CompressStream(Stream: TStream; OutStream: TStream;
OnProgress: TNotifyEvent): boolean;

// 데이터 압축 해제..
function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;


// 응답 검사..
function Contains(Contents: string; const str: array of const): Boolean;
function IsGoodResponse(Text, Command: string; Response: array of const): Boolean;

// Reverse 4Btye..
function Rev4Bytes(Value: LongInt): LongInt;

implementation


procedure PrintLog(AFile, AMessage: String);
var
  FileHandle: Integer;
  S: AnsiString;
begin

  if FileExists(AFile) then
  begin
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
   CS := TZCompressionStream.Create(OutStream); // 스트림 생성
try
   if Assigned(OnProgress) then CS.OnProgress := OnProgress;
      CS.CopyFrom(Stream, Stream.Size); // 여기서 압축이 진행됨
                                          // 테스트 결과 압축완료시 이벤트가 발생하지 않기 때문에
                                          // 완료시 한번 더 이벤트를 불러준다.
   if Assigned(OnProgress) then OnProgress(CS);
      Result := True;
   finally
      CS.Free;
   end;
end;


function Contains(Contents: string; const str: array of const): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to High(str) do
    begin
      if Pos(str[i].VPWideChar, Contents) = 0 then
        Exit;
    end;

  Result := True;
end;


function IsGoodResponse(Text, Command: string; Response: array of const): Boolean;
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
  BuffSize = 65535; // 버퍼 사이즈
var
  DS: TZDeCompressionStream;
  Buff: PChar; // 임시 버퍼
  ReadSize: integer; // 읽은 크기
begin
  if Stream = OutStream then
    // 입력 스트림과 출력스트림이 같으면 문제가 발생한다
    raise Exception.Create('입력 스트림과 출력 스트림이 같습니다');
  Stream.Position := 0; // 스트림 커서 초기화
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
        OnProgress(DS); // Compress와 같은이유
      Result := true;
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


end.
