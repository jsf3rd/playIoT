unit JdcOption;

interface

uses
  Classes, SysUtils, IniFiles, Win.Registry, Winapi.Windows, JdcGlobal;

type
  TIniProc = reference to procedure(AIni: TIniFile);
  TRegProc = reference to procedure(ARegistry: TRegistry);

  TOptionAbstract = class abstract
  protected
    FIniFile: TCustomIniFile;
    function GetUseCloudLog: boolean; virtual;
    procedure SetUseCloudLog(const Value: boolean); virtual;
    function GetLogServer: TConnInfo; virtual;
    procedure SetLogServer(const Value: TConnInfo); virtual;
    function GetUseDebug: boolean; virtual;
    procedure SetUseDebug(const Value: boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function FileName: string;
    property LogServer: TConnInfo read GetLogServer write SetLogServer;
    property UseCloudLog: boolean read GetUseCloudLog write SetUseCloudLog;
    property UseDebug: boolean read GetUseDebug write SetUseDebug;
  end;

  // Deprecated
  TOptionInterface = class abstract(TComponent)
  private
    FPath: String;
    procedure SetPath(const Value: String);
  protected
    function GetStringValue(const ASec, AIdent, ADefault: String): String; virtual; abstract;
    procedure SetStringValue(const ASec, AIdent, AValue: String); virtual; abstract;

    function GetIntegerValue(const ASec, AIdent: String; ADefault: Integer): Integer; virtual; abstract;
    procedure SetIntegerValue(const ASec, AIdent: String; AValue: Integer); virtual; abstract;

    function GetFloatValue(const ASec, AIdent: String; ADefault: real): real; virtual; abstract;
    procedure SetFloatValue(const ASec, AIdent: String; AValue: real); virtual; abstract;

    function GetBoolValue(const ASec, AIdent: String; ADefault: boolean): boolean; virtual; abstract;
    procedure SetBoolValue(const ASec, AIdent: String; AValue: boolean); virtual; abstract;

    function GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime): TDateTime; virtual; abstract;
    procedure SetDateTimeValue(const ASec, AIdent: String; AValue: TDateTime); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;

    function ReadSections: TStrings; virtual; abstract;
    function ReadSection(ASection: string): TStrings; virtual; abstract;
    function ReadSectionValues(ASection: string): TStrings; virtual; abstract;

    procedure EraseSection(const ASec: String); virtual; abstract;
    procedure DeleteKey(const ASec, AKey: String); virtual; abstract;

    function KeyExist(const ASec, AKey: String): boolean; virtual; abstract;

    property Path: String read FPath write SetPath;
  end;

  // Deprecated
  TOptionIniFiles = class(TOptionInterface)
  private
    procedure IniTemplete(ACallBack: TIniProc);
  protected
    function GetStringValue(const ASec, AIdent, ADefault: String): String; override;
    procedure SetStringValue(const ASec, AIdent, AValue: String); override;

    function GetIntegerValue(const ASec, AIdent: String; ADefault: Integer): Integer; override;
    procedure SetIntegerValue(const ASec, AIdent: String; AValue: Integer); override;

    function GetFloatValue(const ASec, AIdent: String; ADefault: real): real; override;
    procedure SetFloatValue(const ASec, AIdent: String; AValue: real); override;

    function GetBoolValue(const ASec, AIdent: String; ADefault: boolean): boolean; override;
    procedure SetBoolValue(const ASec, AIdent: String; AValue: boolean); override;

    function GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime): TDateTime; override;
    procedure SetDateTimeValue(const ASec, AIdent: String; AValue: TDateTime); override;
  public
    function ReadSections: TStrings; override;
    function ReadSection(ASection: string): TStrings; override;
    function ReadSectionValues(ASection: string): TStrings; override;

    procedure EraseSection(const ASec: String); override;
    procedure DeleteKey(const ASec, AKey: String); override;

    function KeyExist(const ASec, AKey: String): boolean; override;
  end;

  // Deprecated
  TOptionRegistry = class(TOptionInterface)
  private
    FRootKey: HKEY;
    procedure RegistryTemplete(ASec: String; ACallBack: TRegProc);
  protected
    function GetStringValue(const ASec, AIdent, ADefault: String): String; override;
    procedure SetStringValue(const ASec, AIdent, AValue: String); override;

    function GetIntegerValue(const ASec, AIdent: String; ADefault: Integer): Integer; override;
    procedure SetIntegerValue(const ASec, AIdent: String; AValue: Integer); override;

    function GetFloatValue(const ASec, AIdent: String; ADefault: real): real; override;
    procedure SetFloatValue(const ASec, AIdent: String; AValue: real); override;

    function GetBoolValue(const ASec, AIdent: String; ADefault: boolean): boolean; override;
    procedure SetBoolValue(const ASec, AIdent: String; AValue: boolean); override;

    function GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime): TDateTime; override;
    procedure SetDateTimeValue(const ASec, AIdent: String; AValue: TDateTime); override;
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AKey: HKEY); reintroduce; overload;

    function ReadSections: TStrings; override;
    function ReadSection(ASection: string): TStrings; override;
    function ReadSectionValues(ASection: string): TStrings; override;

    procedure EraseSection(const ASec: String); override;
    procedure DeleteKey(const ASec, AKey: String); override;

    function KeyExist(const ASec, AKey: String): boolean; override;

    property RootKey: HKEY read FRootKey;
  end;

implementation

var
  MyObj: TOptionIniFiles = nil;

  { TOption }

procedure TOptionIniFiles.DeleteKey(const ASec, AKey: String);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.DeleteKey(ASec, AKey);

    end);

end;

procedure TOptionIniFiles.EraseSection(const ASec: String);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.EraseSection(ASec);

    end);
end;

procedure TOptionIniFiles.SetFloatValue(const ASec, AIdent: String; AValue: real);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteFloat(ASec, AIdent, AValue);

    end);
end;

function TOptionIniFiles.ReadSection(ASection: string): TStrings;
var
  Value: TStrings;
begin
  Value := TStringList.Create;

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      AIni.ReadSection(ASection, Value);
    end);

  result := Value;
end;

function TOptionIniFiles.ReadSections: TStrings;
var
  Value: TStrings;
begin
  Value := TStringList.Create;

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      AIni.ReadSections(Value);
    end);

  result := Value;
end;

function TOptionIniFiles.ReadSectionValues(ASection: string): TStrings;
var
  Value: TStrings;
begin
  Value := TStringList.Create;

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      AIni.ReadSectionValues(ASection, Value);
    end);

  result := Value;
end;

function TOptionIniFiles.GetBoolValue(const ASec, AIdent: String; ADefault: boolean): boolean;
var
  Value: boolean;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ReadBool(ASec, AIdent, ADefault);
    end);

  result := Value;

end;

function TOptionIniFiles.GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime): TDateTime;
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

function TOptionIniFiles.GetFloatValue(const ASec, AIdent: String; ADefault: real): real;
var
  Value: real;
begin

  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ReadFloat(ASec, AIdent, ADefault);
    end);

  result := Value;
end;

function TOptionIniFiles.GetIntegerValue(const ASec, AIdent: String; ADefault: Integer): Integer;
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

function TOptionIniFiles.GetStringValue(const ASec, AIdent, ADefault: String): String;
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

procedure TOptionIniFiles.IniTemplete(ACallBack: TIniProc);
var
  IniFile: TIniFile;
begin
  if FPath.IsEmpty then
    raise Exception.Create('Empty Option Path.');

  IniFile := TIniFile.Create(FPath);
  try

    with IniFile do
    begin

      ACallBack(IniFile);

    end;

  finally
    IniFile.Free;
  end;
end;

function TOptionIniFiles.KeyExist(const ASec, AKey: String): boolean;
var
  Value: boolean;
begin
  IniTemplete(

    procedure(AIni: TIniFile)
    begin
      Value := AIni.ValueExists(ASec, AKey);
    end);

  result := Value;
end;

procedure TOptionIniFiles.SetBoolValue(const ASec, AIdent: String; AValue: boolean);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteBool(ASec, AIdent, AValue);

    end);

end;

procedure TOptionIniFiles.SetDateTimeValue(const ASec, AIdent: String; AValue: TDateTime);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteDateTime(ASec, AIdent, AValue);

    end);

end;

procedure TOptionIniFiles.SetIntegerValue(const ASec, AIdent: String; AValue: Integer);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteInteger(ASec, AIdent, AValue);

    end);
end;

procedure TOptionIniFiles.SetStringValue(const ASec, AIdent, AValue: String);
begin
  IniTemplete(
    procedure(AIni: TIniFile)
    begin

      AIni.WriteString(ASec, AIdent, AValue);

    end);
end;

{ TOptionAbstract }

constructor TOptionInterface.Create(AOwner: TComponent);
begin
  Inherited;

  FPath := '';
end;

procedure TOptionInterface.SetPath(const Value: String);
begin
  FPath := Value;

  if Value.IsEmpty then
    raise Exception.Create('::JdcOption:: Can not set empty string to Option Path.');
end;

{ TOptionAbstract }

constructor TOptionAbstract.Create;
var
  FileName: string;
begin
  FileName := ChangeFileExt(ParamStr(0), '.ini');
  FIniFile := TIniFile.Create(FileName);

  /// ////////////////////////////////////////////////////////////////////////////////
  // Override Create;

  // FIniFile := TMemIniFile.Create(FileName);

  // Registry...
  // FileName := 'SOFTWARE\DACO\' + PROJECT_CODE;
  // FIniFile := TRegistryIniFile.Create(FileName);
  // TRegistryIniFile(FIniFile).RegIniFile.RootKey := HKEY_CURRENT_USER;
  // TRegistryIniFile(FIniFile).RegIniFile.OpenKey(FIniFile.FileName, True);
end;

destructor TOptionAbstract.Destroy;
begin
  if Assigned(FIniFile) then
    FreeAndNil(FIniFile);
  inherited;
end;

function TOptionAbstract.FileName: string;
begin
  result := FIniFile.FileName;
end;

function TOptionAbstract.GetLogServer: TConnInfo;
begin
  result.StringValue := FIniFile.ReadString('CloudLog', 'IP', '');
  result.IntegerValue := FIniFile.ReadInteger('CloudLog', 'Port', 8094);
end;

function TOptionAbstract.GetUseCloudLog: boolean;
begin
  result := FIniFile.ReadBool('CloudLog', 'Enable', False);
end;

function TOptionAbstract.GetUseDebug: boolean;
begin
  result := FIniFile.ReadBool('Config', 'UseDebug', False);
end;

procedure TOptionAbstract.SetLogServer(const Value: TConnInfo);
begin
  FIniFile.WriteString('CloudLog', 'IP', Value.StringValue);
  FIniFile.WriteInteger('CloudLog', 'Port', Value.IntegerValue);
end;

procedure TOptionAbstract.SetUseCloudLog(const Value: boolean);
begin
  FIniFile.WriteBool('CloudLog', 'Enable', Value);
end;

procedure TOptionAbstract.SetUseDebug(const Value: boolean);
begin
  FIniFile.WriteBool('Config', 'UseDebug', Value);
end;

{ TOptionRegistry }

constructor TOptionRegistry.Create(AOwner: TComponent; AKey: HKEY);
begin
  inherited Create(AOwner);
  FRootKey := AKey;
end;

constructor TOptionRegistry.Create(AOwner: TComponent);
begin
  Create(AOwner, HKEY_CURRENT_USER);
end;

procedure TOptionRegistry.DeleteKey(const ASec, AKey: String);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.DeleteValue(AKey);
    end);
end;

procedure TOptionRegistry.EraseSection(const ASec: String);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.CloseKey;

      ARegistry.OpenKey('\SOFTWARE\' + FPath, True);
      ARegistry.DeleteKey(ASec);
    end);
end;

function TOptionRegistry.GetBoolValue(const ASec, AIdent: String; ADefault: boolean): boolean;
var
  Value: boolean;
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      if ARegistry.ValueExists(AIdent) then
        Value := ARegistry.ReadBool(AIdent)
      else
        Value := ADefault;
    end);

  result := Value;
end;

function TOptionRegistry.GetDateTimeValue(const ASec, AIdent: String; ADefault: TDateTime): TDateTime;
var
  Value: TDateTime;
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      if ARegistry.ValueExists(AIdent) then
        Value := ARegistry.ReadDateTime(AIdent)
      else
        Value := ADefault;
    end);

  result := Value;
end;

function TOptionRegistry.GetFloatValue(const ASec, AIdent: String; ADefault: real): real;
var
  Value: Double;
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      if ARegistry.ValueExists(AIdent) then
        Value := ARegistry.ReadFloat(AIdent)
      else
        Value := ADefault;
    end);

  result := Value;
end;

function TOptionRegistry.GetIntegerValue(const ASec, AIdent: String; ADefault: Integer): Integer;
var
  Value: Integer;
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      if ARegistry.ValueExists(AIdent) then
        Value := ARegistry.ReadInteger(AIdent)
      else
        Value := ADefault;
    end);

  result := Value;

end;

function TOptionRegistry.GetStringValue(const ASec, AIdent, ADefault: String): String;
var
  Value: String;
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      if ARegistry.ValueExists(AIdent) then
        Value := ARegistry.ReadString(AIdent)
      else
        Value := ADefault;
    end);

  result := Value;
end;

function TOptionRegistry.KeyExist(const ASec, AKey: String): boolean;
var
  Value: boolean;
begin

  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      Value := ARegistry.KeyExists(AKey);
    end);

  result := Value;
end;

function TOptionRegistry.ReadSection(ASection: string): TStrings;
var
  Value: TStrings;
begin
  Value := TStringList.Create;

  RegistryTemplete(ASection,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.GetKeyNames(Value);
    end);

  result := Value;
end;

function TOptionRegistry.ReadSections: TStrings;
var
  Value: TStrings;
begin
  Value := TStringList.Create;

  RegistryTemplete('',
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.GetKeyNames(Value);
    end);

  result := Value;
end;

function TOptionRegistry.ReadSectionValues(ASection: string): TStrings;
var
  Value: TStrings;
begin
  Value := TStringList.Create;

  RegistryTemplete(ASection,
    procedure(ARegistry: TRegistry)
    var
      tmp: TStrings;
      MyElem: String;
    begin
      tmp := TStringList.Create;
      try
        ARegistry.GetValueNames(tmp);

        for MyElem in tmp do
        begin
          Value.Values[MyElem] := ARegistry.ReadString(MyElem);
        end;

      finally
        tmp.Free;
      end;

    end);

  result := Value;
end;

procedure TOptionRegistry.RegistryTemplete(ASec: String; ACallBack: TRegProc);
var
  Registry: TRegistry;
begin
  if FPath.IsEmpty then
    raise Exception.Create('Empty Option Path.');

  Registry := TRegistry.Create(KEY_READ or KEY_WRITE);
  try
    Registry.RootKey := FRootKey;
    Registry.OpenKey('\SOFTWARE\' + FPath + '\' + ASec, True);

    ACallBack(Registry);

    Registry.CloseKey;
  finally
    Registry.Free;
  end;
end;

procedure TOptionRegistry.SetBoolValue(const ASec, AIdent: String; AValue: boolean);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.WriteBool(AIdent, AValue);
    end);
end;

procedure TOptionRegistry.SetDateTimeValue(const ASec, AIdent: String; AValue: TDateTime);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.WriteDateTime(AIdent, AValue);
    end);
end;

procedure TOptionRegistry.SetFloatValue(const ASec, AIdent: String; AValue: real);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.WriteFloat(AIdent, AValue);
    end);
end;

procedure TOptionRegistry.SetIntegerValue(const ASec, AIdent: String; AValue: Integer);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.WriteInteger(AIdent, AValue);
    end);
end;

procedure TOptionRegistry.SetStringValue(const ASec, AIdent, AValue: String);
begin
  RegistryTemplete(ASec,
    procedure(ARegistry: TRegistry)
    begin
      ARegistry.WriteString(AIdent, AValue);
    end);
end;

end.
