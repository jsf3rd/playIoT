unit JdcGlobal;

interface

uses
  ValueList,
  Classes, SysUtils, Windows, ZLib;

// �α� ���..
procedure PrintLog(AFile, AMessage: String);

// ������ ����..
function CompressStream(Stream: TStream; OutStream: TStream;
OnProgress: TNotifyEvent): boolean;

// ������ ���� ����..
function DeCompressStream(Stream: TStream; OutStream: TStream;
  OnProgress: TNotifyEvent): boolean;


// ���� �˻�..
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
   CS := TZCompressionStream.Create(OutStream); // ��Ʈ�� ����
try
   if Assigned(OnProgress) then CS.OnProgress := OnProgress;
      CS.CopyFrom(Stream, Stream.Size); // ���⼭ ������ �����
                                          // �׽�Ʈ ��� ����Ϸ�� �̺�Ʈ�� �߻����� �ʱ� ������
                                          // �Ϸ�� �ѹ� �� �̺�Ʈ�� �ҷ��ش�.
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
