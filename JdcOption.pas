unit JdcOption;

interface

uses
  Classes, SysUtils, IniFiles;

type
  TIniProc = reference to procedure(AIni: TIniFile);

  TOptionAbstract = class(TComponent)
  protected
    FExeName: String;
    procedure IniTemplete(ACallBack: TIniProc);
    function GetStringValue(const ASec, AIdent, ADefault: String): String;
    procedure SetStringValue(const ASec, AIndent, AValue: String);

    function GetIntegerValue(const ASec, AIdent: String;
      ADefault: Integer): Integer;
    procedure SetIntegerValue(const ASec, AIndent: String; AValue: Integer);

    function GetBoolValue(const ASec, AIdent: String;
      ADefault: Boolean): Boolean;
    procedure SetBoolValue(const ASec, AIndent: String; AValue: Boolean);

    function GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime)
      : TDateTime;
    procedure SetDateTimeValue(const ASec, AIndent: String; AValue: TDateTime);

  end;

implementation

var
  MyObj: TOptionAbstract = nil;

  { TOption }

function TOptionAbstract.GetBoolValue(const ASec, AIdent: String;
  ADefault: Boolean): Boolean;
var
  Value: Boolean;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ReadBool(ASec, AIdent, ADefault);
    end);

  result := Value;

end;

function TOptionAbstract.GetDateTimeValue(const ASec, AIdent: String;
ADefault: TDateTime): TDateTime;
var
  Value: TDateTime;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ReadDateTime(ASec, AIdent, ADefault);
    end);

  result := Value;

end;

function TOptionAbstract.GetIntegerValue(const ASec, AIdent: String;
ADefault: Integer): Integer;
var
  Value: Integer;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ReadInteger(ASec, AIdent, ADefault);
    end);

  result := Value;

end;

function TOptionAbstract.GetStringValue(const ASec, AIdent,
  ADefault: String): String;
var
  Value: String;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ReadString(ASec, AIdent, ADefault);
    end);

  result := Value;
end;

procedure TOptionAbstract.IniTemplete(ACallBack: TIniProc);
var
  FIni: TIniFile;
begin

  FIni := TIniFile.Create(ChangeFileExt(FExeName, '.ini'));
  try

    with FIni do
    begin

      ACallBack(FIni);

    end;

  finally
    FIni.Free;
  end;
end;

procedure TOptionAbstract.SetBoolValue(const ASec, AIndent: String;
AValue: Boolean);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteBool(ASec, AIndent, AValue);

    end);

end;

procedure TOptionAbstract.SetDateTimeValue(const ASec, AIndent: String;
AValue: TDateTime);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteDateTime(ASec, AIndent, AValue);

    end);

end;

procedure TOptionAbstract.SetIntegerValue(const ASec, AIndent: String;
AValue: Integer);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteInteger(ASec, AIndent, AValue);

    end);

end;

procedure TOptionAbstract.SetStringValue(const ASec, AIndent, AValue: String);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteString(ASec, AIndent, AValue);

    end);
end;

end.
