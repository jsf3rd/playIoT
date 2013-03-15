unit JdcOption;

interface

uses
  Classes, SysUtils, IniFiles;

type
  TIniProc = reference to procedure(AIni: TIniFile);

  TOptionAbstract = class(TComponent)
  private

  protected
    FExeName: String;
    procedure IniTemplete(ACallBack: TIniProc);
    function GetStringValue(const ASec, AIdent, ADefault: String): String;
    procedure SetStringValue(const ASec, AIdent, AValue: String);

    function GetIntegerValue(const ASec, AIdent: String;
      ADefault: Integer): Integer;
    procedure SetIntegerValue(const ASec, AIdent: String; AValue: Integer);

    function GetBoolValue(const ASec, AIdent: String;
      ADefault: Boolean): Boolean;
    procedure SetBoolValue(const ASec, AIdent: String; AValue: Boolean);

    function GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime)
      : TDateTime;
    procedure SetDateTimeValue(const ASec, AIdent: String; AValue: TDateTime);
  public
    function ReadSections: TStrings;
    function ReadSection(ASection: string): TStrings;
    function ReadSectionValues(ASection: string): TStrings;
  end;

implementation

var
  MyObj: TOptionAbstract = nil;

  { TOption }
function TOptionAbstract.ReadSection(ASection: string): TStrings;
var
  Value: TStrings;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      AIni.ReadSection(ASection, Value);
    end);

  result := Value;
end;

function TOptionAbstract.ReadSections: TStrings;
var
  Value: TStrings;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      AIni.ReadSections(Value);
    end);

  result := Value;
end;

function TOptionAbstract.ReadSectionValues(ASection: string): TStrings;
var
  Value: TStrings;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      AIni.ReadSectionValues(ASection, Value);
    end);

  result := Value;
end;

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

procedure TOptionAbstract.SetBoolValue(const ASec, AIdent: String;
AValue: Boolean);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteBool(ASec, AIdent, AValue);

    end);

end;

procedure TOptionAbstract.SetDateTimeValue(const ASec, AIdent: String;
AValue: TDateTime);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteDateTime(ASec, AIdent, AValue);

    end);

end;

procedure TOptionAbstract.SetIntegerValue(const ASec, AIdent: String;
AValue: Integer);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteInteger(ASec, AIdent, AValue);

    end);

end;

procedure TOptionAbstract.SetStringValue(const ASec, AIdent, AValue: String);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteString(ASec, AIdent, AValue);

    end);
end;

end.
