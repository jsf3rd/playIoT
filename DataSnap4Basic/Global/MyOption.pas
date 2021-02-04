unit MyOption;

interface

uses
  Classes, SysUtils, MyGlobal, System.IniFiles, Registry, Winapi.Windows, JdcGlobal, JdcOption;

type
  TOption = class(TOptionAbstract)
  protected
    function GetDBInfo: String;
    procedure SetDBInfo(ADBInfo: String);
    function GetTcpPort: Integer;
    procedure SetTcpPort(const Value: Integer);
    procedure SetHttpPort(const Value: Integer);
    function GetHttpPort: Integer;

    function GetAppCode: string;
  public
    class function Obj: TOption;
    property DBInfo: String read GetDBInfo write SetDBInfo;
    property TcpPort: Integer read GetTcpPort write SetTcpPort;
    property HttpPort: Integer read GetHttpPort write SetHttpPort;

    property AppCode: String read GetAppCode;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }
function TOption.GetAppCode: string;
begin
  result := FIniFile.ReadString('Config', 'AppCode', APPLICATION_CODE);
end;

function TOption.GetDBInfo: String;
begin
  result := FIniFile.ReadString('DB', 'Params', '');
end;

function TOption.GetTcpPort: Integer;
begin
  result := FIniFile.ReadInteger('DSServer', 'TCPPort', 211);
end;

function TOption.GetHttpPort: Integer;
begin
  result := FIniFile.ReadInteger('DSServer', 'HTTPPort', 80);
end;

procedure TOption.SetDBInfo(ADBInfo: String);
begin
  FIniFile.WriteString('DB', 'Params', ADBInfo);
end;

procedure TOption.SetTcpPort(const Value: Integer);
begin
  FIniFile.WriteInteger('DSServer', 'TCPPort', Value);
end;

procedure TOption.SetHttpPort(const Value: Integer);
begin
  FIniFile.WriteInteger('DSServer', 'HTTPPort', Value);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
    MyObj := TOption.Create;
  result := MyObj;
end;

initialization

TOption.Obj;

finalization

FreeAndNilEx(MyObj);

end.
