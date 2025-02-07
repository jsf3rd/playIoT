unit JdcOption;

interface

uses
  Classes, SysUtils, IniFiles, JdcGlobal;

type
  TOptionAbstract = class abstract
  protected
    FIniFile: TCustomIniFile;
    function GetUseCloudLog: boolean; virtual;
    procedure SetUseCloudLog(const Value: boolean); virtual;
    function GetLogServer: TConnInfo; virtual;
    procedure SetLogServer(const Value: TConnInfo); virtual;
    function GetUseDebug: boolean; virtual;
    procedure SetUseDebug(const Value: boolean); virtual;

    function GetAppCode: string; virtual;
    function GetProjectCode: string; virtual;
    procedure SetAppCode(const Value: string); virtual;
    procedure SetProjectCode(const Value: string); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function FileName: string;
    property LogServer: TConnInfo read GetLogServer write SetLogServer;
    property UseCloudLog: boolean read GetUseCloudLog write SetUseCloudLog;
    property UseDebug: boolean read GetUseDebug write SetUseDebug;
    property IniFile: TCustomIniFile read FIniFile write FIniFile;
    property AppCode: string read GetAppCode write SetAppCode;
    property ProjectCode: string read GetProjectCode write SetProjectCode;
  end;

implementation

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
  // FileName := 'SOFTWARE\UDNS\' + PROJECT_CODE;
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

function TOptionAbstract.GetAppCode: string;
begin
  result := FIniFile.ReadString('Option', 'AppCode', 'UdnsApplication');
end;

function TOptionAbstract.GetLogServer: TConnInfo;
begin
  result.StringValue := FIniFile.ReadString('CloudLog', 'IP', '');
  result.IntegerValue := FIniFile.ReadInteger('CloudLog', 'Port', 8094);
end;

function TOptionAbstract.GetProjectCode: string;
begin
  result := FIniFile.ReadString('Option', 'ProjectCode', 'UdnsProject');
end;

function TOptionAbstract.GetUseCloudLog: boolean;
begin
  result := FIniFile.ReadBool('CloudLog', 'Enable', False);
end;

function TOptionAbstract.GetUseDebug: boolean;
begin
  result := FIniFile.ReadBool('Config', 'UseDebug', False);
end;

procedure TOptionAbstract.SetAppCode(const Value: string);
begin
  FIniFile.WriteString('Option', 'AppCode', Value);
end;

procedure TOptionAbstract.SetLogServer(const Value: TConnInfo);
begin
  FIniFile.WriteString('CloudLog', 'IP', Value.StringValue);
  FIniFile.WriteInteger('CloudLog', 'Port', Value.IntegerValue);
end;

procedure TOptionAbstract.SetProjectCode(const Value: string);
begin
  FIniFile.WriteString('Option', 'ProjectCode', Value);
end;

procedure TOptionAbstract.SetUseCloudLog(const Value: boolean);
begin
  FIniFile.WriteBool('CloudLog', 'Enable', Value);
end;

procedure TOptionAbstract.SetUseDebug(const Value: boolean);
begin
  FIniFile.WriteBool('Config', 'UseDebug', Value);
end;

end.
