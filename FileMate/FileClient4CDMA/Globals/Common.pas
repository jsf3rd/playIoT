unit Common;

interface

uses Classes, SysUtils;

function GetSubFolder(FileName : String):String;

implementation

function GetSubFolder(FileName : String):String;
begin
  result := '';

  if Pos('calc',FileName) = 0 then exit;

  if Length(FileName) < 14 then exit;

  result := Copy(FileName,7,6) +'\'+ Copy(FileName,13,2);
end;

end.
