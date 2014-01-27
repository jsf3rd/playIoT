unit Option;

interface

uses
  Classes, SysUtils, IniFiles, Global, JdcOption;

type
  TIniProc = reference to procedure(AIni: TIniFile);

  TOption = class(TOptionAbstract)
  private
    function GetFileServer: TConnInfo;
    procedure SetFileServer(const Value: TConnInfo);
    function GetCDMAInfo: TConnInfo;
    procedure SetCDMAInfo(const Value: TConnInfo);

    function GetBufferSize: Integer;
    function GetShowCDMALog: boolean;
    procedure SetShowCDMALog(const Value: boolean);
    function GetDataFolder: String;
    procedure SetDataFolder(const Value: String);
    function GetCheckInterval: Integer;
    procedure SetCheckInterval(const Value: Integer);
  public
    class function Obj: TOption;

    property BufferSize: Integer read GetBufferSize;
    property FileServer: TConnInfo read GetFileServer write SetFileServer;
    property CDMAInfo: TConnInfo read GetCDMAInfo write SetCDMAInfo;
    property ShowCDMALog: boolean read GetShowCDMALog write SetShowCDMALog;
    property DataFolder: String read GetDataFolder write SetDataFolder;
    property CheckInterval: Integer read GetCheckInterval write SetCheckInterval;
  end;

implementation

var
  MyObj: TOption = nil;

  { TOption }

function TOption.GetBufferSize: Integer;
begin

  result := GetIntegerValue('CDMA', 'BufferSize', DEFAULT_BUFFER_SIZE);

  if result = DEFAULT_BUFFER_SIZE then
    SetIntegerValue('CDMA', 'BufferSize', DEFAULT_BUFFER_SIZE);

end;

function TOption.GetCDMAInfo: TConnInfo;
begin

  result.StringValue := GetStringValue('CDMA', 'CommPort',
    CDMA_DEFAULT_COMM_PORT);
  result.IntegerValue := GetIntegerValue('CDMA', 'BaudRate',
    CDMA_DEFAULT_BAUD_RATE);
end;

function TOption.GetCheckInterval: Integer;
begin
  result := GetIntegerValue('Option', 'CheckInterval', 60000);
end;

function TOption.GetDataFolder: String;
begin
  result := GetStringValue('Option', 'DataFolder', '');
end;

function TOption.GetFileServer: TConnInfo;
begin

  result.StringValue := GetStringValue('FileServer', 'IP', LOCAL_HOST);
  result.IntegerValue := GetIntegerValue('FileServer', 'Port',
    FILE_DEFAULT_PORT);

end;

function TOption.GetShowCDMALog: boolean;
begin
  result := GetBoolValue('CDMA', 'Log', false);
end;

class function TOption.Obj: TOption;
begin
  if MyObj = nil then
  begin
    MyObj := TOption.Create(nil);
    MyObj.FExeName := TGlobal.Obj.ExeName;
  end;
  result := MyObj;
end;

procedure TOption.SetCDMAInfo(const Value: TConnInfo);
begin
  SetStringValue('CDMA', 'CommPort', Value.StringValue);
  SetIntegerValue('CDMA', 'BaudRate', Value.IntegerValue);
end;

procedure TOption.SetCheckInterval(const Value: Integer);
begin
  SetIntegerValue('Option', 'CheckInterval', Value);
end;

procedure TOption.SetDataFolder(const Value: String);
begin
  SetStringValue('Option', 'DataFolder', Value);
end;

procedure TOption.SetFileServer(const Value: TConnInfo);
begin
  SetStringValue('FileServer', 'IP', Value.StringValue);
  SetIntegerValue('FileServer', 'Port', Value.IntegerValue);
end;

procedure TOption.SetShowCDMALog(const Value: boolean);
begin
  SetBoolValue('CDMA', 'Log', Value);
end;

end.
