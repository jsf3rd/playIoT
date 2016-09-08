unit MyOption;

interface

uses
  Classes, SysUtils, JdcOption, MyGlobal, Winapi.Windows;

type
  TOption = class(TOptionIniFiles)
  private
    function GetDBInfo: String;
    procedure SetDBInfo(ADBInfo: String);
    function GetTcpPort: Integer;
    procedure SetTcpPort(const Value: Integer);
    procedure SetHttpPort(const Value: Integer);
    function GetHttpPort: Integer;

  public
    class function Obj: TOption;

    property TcpPort: Integer read GetTcpPort write SetTcpPort;
    property HttpPort: Integer read GetHttpPort write SetHttpPort;
    property DBInfo: String read GetDBInfo write SetDBInfo;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetDBInfo: String;
begin
  // Server=db.playiot.biz,DataBase=mydb,User_Name=playiot,Password=playiot,Port=5432
  result := GetStringValue('DB', 'Params', 'Edit DB connect params...');
end;

function TOption.GetTcpPort: Integer;
begin
  result := GetIntegerValue('DSServer', 'TCPPort', 211);
end;

function TOption.GetHttpPort: Integer;
begin
  result := GetIntegerValue('DSServer', 'HTTPPort', 80);
end;

procedure TOption.SetDBInfo(ADBInfo: String);
begin
  SetStringValue('DB', 'Params', ADBInfo);
end;

procedure TOption.SetTcpPort(const Value: Integer);
begin
  SetIntegerValue('DSServer', 'TCPPort', Value);
end;

procedure TOption.SetHttpPort(const Value: Integer);
begin
  SetIntegerValue('DSServer', 'HTTPPort', Value);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create(nil);
    MyObj.Path := ChangeFileExt(TGlobal.Obj.ExeName, '.ini');
  end;
  result := MyObj;
end;

end.
