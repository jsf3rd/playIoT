unit Option;

interface

uses
  Classes, SysUtils, JdcOption, Global, IniFiles;

type
  TOption = class(TOptionAbstract)
  private
    function GetID(ASec: string): string;
    function GetPassword(ASec: string): string;
    procedure SetID(ASec: string; const Value: string);
    procedure SetPassword(ASec: string; const Value: string);

  public
    class function Obj: TOption;

    property ReadID[Sec: string]: string read GetID write SetID;
    property ReadPassword[Sec: string]: string read GetPassword write SetPassword;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetID(ASec: string): string;
begin
  result := FIniFile.ReadString(ASec, 'ID', '');
end;

function TOption.GetPassword(ASec: string): string;
begin
  result := FIniFile.ReadString(ASec, 'Password', '');
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create;
  end;
  result := MyObj;
end;

procedure TOption.SetID(ASec: string; const Value: string);
begin
  FIniFile.WriteString(ASec, 'ID', '');
end;

procedure TOption.SetPassword(ASec: string; const Value: string);
begin
  FIniFile.WriteString(ASec, 'Password', '');
end;

end.
