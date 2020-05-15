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
  /// - string szTagName: 값이 변경된 태그 이름
  /// - string szTagValue: 변경된 태그 값(문자열)
  /// - int iAlarmStatus: 경보 상태 표시 값(복귀: 255 , 정상: 0, 경보: 1 이상의 값)
  /// - int iStatus :  태그 상태표시값(Unload (iStatus & 128=128), Failed(iStatus & 1=1),  OutOfService(iStatus & 2=2), System Alarm(iStatus & 4= 4), Normal(iStatus==0))
  /// </summary>
  TOnFireCOVEvent = procedure(const iIndex: Int32; const szTagName: PAnsiChar; const szValue: PAnsiChar;
    const iAlarm: Int32; const iStatus: Int32)stdcall;

  /// <summary>
  /// 감시할 태그의 이름을 입력하여 태그들을 등록합니다.
  /// - 입력 인자 : 감시 설정할 태그 이름을 입력합니다.형식은 "문자열"
  /// - 출력 인자:  감시 태그 설정할때 마다  인덱스를 반환합니다. (숫자)  인덱스 값을 보관하여 GetTagValue의 입력인자로 사용할 수 있습니다.
  /// int iMonitorIndex = SetMonitoringTag(char * sTagName);
  /// </summary>
  TSetMonitoringTag = function(const szTagName: PAnsiChar): Int32 stdcall;

  /// <summary>
  /// 감시할 태그 등록이 완료된 경우 감시 모드로 설정합니다. (단, 프로그램 종료전에 반드시 감시 정지 모드로 변경한 후 에 정지해야 합니다.)
  /// - 입력 인자 : 0 이면 감시 정지이며 , 1이면 감시 시작이다.
  /// - 출력 인자: 현재의 감시 모드를 반환합니다.
  /// </summary>
  TSetMonitorMode = function(const iMode: Int32): Int32 stdcall;

  /// <summary>
  /// SetMonitoringTag 에서 설정한 태그 감시의 반환값 Index를 이용하여 현재의 태그 값을 읽어 옵니다.
  /// * 태그 값의 획득은 이벤트를 사용하시기 바랍니다.GetTagValue는 디버깅 또는 참고용으로 사용해주세요.
  /// iTag : SetMonitoringTag 함수 호출시에 반환되는 인덱스.
  /// szBuffer : 요청한 입력인자의 인덱스값에 해당하는 태그 값.
  /// iBufferSize : 버퍼 최대 길이.
  /// - 출력 인자: szBuffer 길이
  /// </summary>
  TGetTagValue = function(const iTag: Int32; szBuffer: PAnsiChar; const iBuffSize: Int32): Int32; stdcall;

  /// <summary>
  /// SetMonitoringTag 에서 설정한 태그 감시의 반환값 Index를 이용하여 현재의 태그 값을 읽어 옵니다.
  /// * 태그 값의 획득은 이벤트를 사용하시기 바랍니다.GetTagValue2는 디버깅 또는 참고용으로 사용해주세요.
  /// - 입력 인자: SetMonitoringTag 함수 호출시에 반환되는 인덱스를 입력하여 해당 태그의 값을 읽어옵니다.
  /// - 출력 인자: 요청한 입력인자의 인덱스값에 해당하는 태그 값을 반환합니다.
  /// </summary>
  TGetTagValueV2 = function(const iTag: Int32): Pointer; stdcall;

  /// <summary>
  /// 태그에 새로운 값을 제어 명령할 때 사용합니다.
  /// - 입력 인자: 태그이름과 태그값을 문자열 값으로 입력합니다.
  /// </summary>
  TWriteTagValue = function(const szTagName: PAnsiChar; const szValue: PAnsiChar): IntPtr stdcall;

  /// <summary>
  /// 태그 감시 목록 위도우를 표시하거나 감춥니다. SDK개발 과정에서 태그의 감시 상태를 확인하는 용도로 사용합니다.
  /// - 입력 인자: iShow 입력값에 1을 입력하면 목록 위도우를 표시합니다. 0일경우 목록을 숨깁니다.
  /// </summary>
  TShowMonitorWindow = procedure(const iShow: Int32)stdcall;

  /// <summary>
  /// SetMonitoringTag 함수로 감시 설정한 태그 감시 목록을 모두 지웁니다.
  /// </summary>
  TClearAllTags = function: IntPtr stdcall;

  /// <summary>
  /// DLL을 Loading후에 호출해준다.(필수)
  /// </summary>
  TRunningUpdate = procedure stdcall;

  /// <summary>
  /// DLL을 Unloading하기전에 호출해준다.(필수)
  /// </summary>
  TStopUpdate = procedure stdcall;

  /// <summary>
  /// 태그 값이 변경될 경우 업데이트 정보를 CALLBACK으로 받을 수 있습니다.
  /// - 입력인자:  CallBack 함수의 포인터를 전달합니다.
  /// </summary>
  TSetEventCallback = procedure(const Callback: TOnFireCOVEvent)stdcall;

  /// <summary>
  /// TAG 업데이트 시간을 설정합니다. 0.1~3초 점위 내에서 사용합니다. Single형으로 입력하면 내부적으로 ms로 변환하여 사용합니다.
  /// - 입력인자 : 인터벌 값(초)
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
