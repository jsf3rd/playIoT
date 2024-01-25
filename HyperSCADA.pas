unit HyperSCADA;

interface

uses
  System.SysUtils, System.Classes, Windows, System.IOUtils;

const

  MAX_VALUE_BUF = 100;

{$IFDEF WIN32}
  HYPER_SCADA_DLL = 'HyperSDKAPI.dll';
{$ENDIF}
{$IFDEF WIN64}
  HYPER_SCADA_DLL = 'HyperSDKAPI_X64.dll';
{$ENDIF}

type
  TCOV = class
  const
    STATUS_NORMAL = 0;
    STATUS_FAILED = 1;
    STATUS_OUT_OF_SERVICE = 2;
    STATUS_UNLOAD = 128;
  end;

  CString = string[100];

  TOnCallbackEvent = procedure(const Index: Integer; const TagName: string; const Value: String;
    const Alarm: Integer; const Status: Integer) of object;

  /// <summary>
  /// - string szTagName: ���� ����� �±� �̸�
  /// - string szTagValue: ����� �±� ��(���ڿ�)
  /// - int iAlarmStatus: �溸 ���� ǥ�� ��(����: 255 , ����: 0, �溸: 1 �̻��� ��)
  /// - int iStatus :  �±� ����ǥ�ð�(Unload (iStatus & 128=128), Failed(iStatus & 1=1),  OutOfService(iStatus & 2=2), System Alarm(iStatus & 4= 4), Normal(iStatus==0))
  /// </summary>
  TOnFireCOVEvent = procedure(const iIndex: Int32; const szTagName: PAnsiChar;
    const szValue: PAnsiChar; const iAlarm: Int32; const iStatus: Int32)stdcall;

  /// <summary>
  /// ������ �±��� �̸��� �Է��Ͽ� �±׵��� ����մϴ�.
  /// - �Է� ���� : ���� ������ �±� �̸��� �Է��մϴ�.������ "���ڿ�"
  /// - ��� ����:  ���� �±� �����Ҷ� ����  �ε����� ��ȯ�մϴ�. (����)  �ε��� ���� �����Ͽ� GetTagValue�� �Է����ڷ� ����� �� �ֽ��ϴ�.
  /// int iMonitorIndex = SetMonitoringTag(char * sTagName);
  /// </summary>
  TSetMonitoringTag = function(const szTagName: PAnsiChar): Int32 stdcall;

  /// <summary>
  /// ������ �±� ����� �Ϸ�� ��� ���� ���� �����մϴ�. (��, ���α׷� �������� �ݵ�� ���� ���� ���� ������ �� �� �����ؾ� �մϴ�.)
  /// - �Է� ���� : 0 �̸� ���� �����̸� , 1�̸� ���� �����̴�.
  /// - ��� ����: ������ ���� ��带 ��ȯ�մϴ�.
  /// </summary>
  TSetMonitorMode = function(const iMode: Int32): Int32 stdcall;

  /// <summary>
  /// SetMonitoringTag ���� ������ �±� ������ ��ȯ�� Index�� �̿��Ͽ� ������ �±� ���� �о� �ɴϴ�.
  /// * �±� ���� ȹ���� �̺�Ʈ�� ����Ͻñ� �ٶ��ϴ�.GetTagValue�� ����� �Ǵ� ��������� ������ּ���.
  /// iTag : SetMonitoringTag �Լ� ȣ��ÿ� ��ȯ�Ǵ� �ε���.
  /// szBuffer : ��û�� �Է������� �ε������� �ش��ϴ� �±� ��.
  /// iBufferSize : ���� �ִ� ����.
  /// - ��� ����: szBuffer ����
  /// </summary>
  TGetTagValue = function(const iTag: Int32; szBuffer: PAnsiChar; const iBuffSize: Int32)
    : Int32; stdcall;

  /// <summary>
  /// SetMonitoringTag ���� ������ �±� ������ ��ȯ�� Index�� �̿��Ͽ� ������ �±� ���� �о� �ɴϴ�.
  /// * �±� ���� ȹ���� �̺�Ʈ�� ����Ͻñ� �ٶ��ϴ�.GetTagValue2�� ����� �Ǵ� ��������� ������ּ���.
  /// - �Է� ����: SetMonitoringTag �Լ� ȣ��ÿ� ��ȯ�Ǵ� �ε����� �Է��Ͽ� �ش� �±��� ���� �о�ɴϴ�.
  /// - ��� ����: ��û�� �Է������� �ε������� �ش��ϴ� �±� ���� ��ȯ�մϴ�.
  /// </summary>
  TGetTagValueV2 = function(const iTag: Int32): Pointer; stdcall;

  /// <summary>
  /// �±׿� ���ο� ���� ���� ����� �� ����մϴ�.
  /// - �Է� ����: �±��̸��� �±װ��� ���ڿ� ������ �Է��մϴ�.
  /// </summary>
  TWriteTagValue = function(const szTagName: PAnsiChar; const szValue: PAnsiChar): IntPtr stdcall;

  /// <summary>
  /// �±� ���� ��� �����츦 ǥ���ϰų� ����ϴ�. SDK���� �������� �±��� ���� ���¸� Ȯ���ϴ� �뵵�� ����մϴ�.
  /// - �Է� ����: iShow �Է°��� 1�� �Է��ϸ� ��� �����츦 ǥ���մϴ�. 0�ϰ�� ����� ����ϴ�.
  /// </summary>
  TShowMonitorWindow = procedure(const iShow: Int32)stdcall;

  /// <summary>
  /// SetMonitoringTag �Լ��� ���� ������ �±� ���� ����� ��� ����ϴ�.
  /// </summary>
  TClearAllTags = function: IntPtr stdcall;

  /// <summary>
  /// DLL�� Loading�Ŀ� ȣ�����ش�.(�ʼ�)
  /// </summary>
  TRunningUpdate = procedure stdcall;

  /// <summary>
  /// DLL�� Unloading�ϱ����� ȣ�����ش�.(�ʼ�)
  /// </summary>
  TStopUpdate = procedure stdcall;

  /// <summary>
  /// �±� ���� ����� ��� ������Ʈ ������ CALLBACK���� ���� �� �ֽ��ϴ�.
  /// - �Է�����:  CallBack �Լ��� �����͸� �����մϴ�.
  /// </summary>
  TSetEventCallback = procedure(const Callback: TOnFireCOVEvent)stdcall;

  /// <summary>
  /// TAG ������Ʈ �ð��� �����մϴ�. 0.1~3�� ���� ������ ����մϴ�. Single������ �Է��ϸ� ���������� ms�� ��ȯ�Ͽ� ����մϴ�.
  /// - �Է����� : ���͹� ��(��)
  /// </summary>
  TSetUpdateRate = procedure(const fInterval: Single)stdcall;

  THyperSCADA = class
  private
    FDLLHandle: THandle;
    FSetMonitoringTag: TSetMonitoringTag;
    FSetMonitorMode: TSetMonitorMode;
    FGetTagValue: TGetTagValue;
    FGetTagValueV2: TGetTagValueV2;
    FWriteTagValue: TWriteTagValue;
    FShowMonitorWindow: TShowMonitorWindow;
    FClearAllTags: TClearAllTags;
    FRunningUpdate: TRunningUpdate;
    FStopUpdate: TStopUpdate;
    FSetEventCallback: TSetEventCallback;
    FSetUpdateRate: TSetUpdateRate;

    procedure FreeDLL;
    procedure LoadDLL;

    procedure RunningUpdate;
    procedure StopUpdate;
  public
    constructor Create;
    destructor Destroy; override;

    function SetMonitoringTag(TagName: String): Integer;
    function SetMonitorMode(Mode: Integer): Integer;
    function GetTagValue(TagID: Integer): String;
    procedure WriteTagValue(TagName: String; Value: String);
    procedure ShowMonitorWindow(Mode: Integer);
    procedure ClearAllTags;
    procedure SetUpdateRate(Interval: Single);
    procedure SetCallbackEvent(AProc: TOnCallbackEvent);
  end;

implementation

var
  OnCallbackEvent: TOnCallbackEvent;

  { THyperSCADA }

procedure THyperSCADA.ClearAllTags;
begin
  FClearAllTags;
end;

constructor THyperSCADA.Create;
begin
  FDLLHandle := 0;
  OnCallbackEvent := nil;
  LoadDLL;
  RunningUpdate;
end;

destructor THyperSCADA.Destroy;
begin
  StopUpdate;
  FreeDLL;
  inherited;
end;

procedure THyperSCADA.FreeDLL;
begin
  StopUpdate;
  if FDLLHandle <> 0 then
  begin
    try
      FDLLHandle := 0;
    except
      on E: Exception do
    end;
  end;
end;

function THyperSCADA.GetTagValue(TagID: Integer): String;
var
  len: Integer;
  buff: array [0 .. MAX_VALUE_BUF - 1] of AnsiChar;
begin
  if FDLLHandle = 0 then
    Exit('0');

  len := FGetTagValue(TagID, buff, MAX_VALUE_BUF);
  result := String(Copy(buff, 0, len));
end;

procedure OnFireCOVEvent(const iIndex: Int32; const szTagName: PAnsiChar; const szValue: PAnsiChar;
  const iAlarm: Int32; const iStatus: Int32)stdcall;
begin
  if Assigned(OnCallbackEvent) then
    OnCallbackEvent(0, String(szTagName), String(szValue), iAlarm, iStatus);
end;

procedure THyperSCADA.LoadDLL;
begin
  FDLLHandle := LoadLibrary(HYPER_SCADA_DLL);
  if FDLLHandle < 32 then
    raise Exception.Create('Load DLL Exception');

  @FSetMonitoringTag := GetProcAddress(FDLLHandle, 'SetMonitoringTag');
  @FSetMonitorMode := GetProcAddress(FDLLHandle, 'SetMonitorMode');
  @FGetTagValue := GetProcAddress(FDLLHandle, 'GetTagValue');
  @FGetTagValueV2 := GetProcAddress(FDLLHandle, 'GetTagValueV2');
  @FWriteTagValue := GetProcAddress(FDLLHandle, 'WriteTagValue');
  @FClearAllTags := GetProcAddress(FDLLHandle, 'ClearAllTags');
  @FShowMonitorWindow := GetProcAddress(FDLLHandle, 'ShowMonitorWindow');
  @FRunningUpdate := GetProcAddress(FDLLHandle, 'RunningUpdate');
  @FStopUpdate := GetProcAddress(FDLLHandle, 'StopUpdate');
  @FSetEventCallback := GetProcAddress(FDLLHandle, 'SetEventCallback');
  @FSetUpdateRate := GetProcAddress(FDLLHandle, 'SetUpdateRate');

  FSetEventCallback(OnFireCOVEvent);
end;

procedure THyperSCADA.RunningUpdate;
begin
  if FDLLHandle = 0 then
    Exit;
  FRunningUpdate;
end;

function THyperSCADA.SetMonitorMode(Mode: Integer): Integer;
begin
  if FDLLHandle = 0 then
    Exit(0);
  result := FSetMonitorMode(Mode);
end;

procedure THyperSCADA.SetUpdateRate(Interval: Single);
begin
  if FDLLHandle = 0 then
    Exit;
  FSetUpdateRate(Interval);
end;

procedure THyperSCADA.ShowMonitorWindow(Mode: Integer);
begin
  if FDLLHandle = 0 then
    Exit;
  FShowMonitorWindow(Mode);
end;

procedure THyperSCADA.StopUpdate;
begin
  if FDLLHandle = 0 then
    Exit;
  FStopUpdate;
end;

procedure THyperSCADA.SetCallbackEvent(AProc: TOnCallbackEvent);
begin
  OnCallbackEvent := AProc;
end;

function THyperSCADA.SetMonitoringTag(TagName: String): Integer;
var
  Name: AnsiString;
begin
  if FDLLHandle = 0 then
    Exit(-1);
  Name := AnsiString(TagName);
  result := FSetMonitoringTag(PAnsiChar(Name));
end;

procedure THyperSCADA.WriteTagValue(TagName: String; Value: String);
begin
  if FDLLHandle = 0 then
    Exit;
  FWriteTagValue(PAnsiChar(AnsiString(TagName)), PAnsiChar(AnsiString(Value)));
end;

end.
