// *******************************************************
//
// DACO-M 1000P TCP Common
//
// Copyright(c) 2020 DACO.
//
// jsf3rd@e-daco.net
//
// *******************************************************

unit JdcDacoM.Common;

interface

uses System.SysUtils, System.Classes, IdGlobal, Winapi.Windows;

const
  MAX_UNIT_ID = 188;
  MODULE_COUNT = 39; // 고급형 모듈 개수
  METER_COUNT = 240; // 경제형 미터 개수

type
  TID = packed record
    Hi: Byte; // 대표ID
    Low: Byte; // 설치 상 정보
  end;

  TIDArray40 = array [0 .. MODULE_COUNT] of TID;

  TProtocolType = (ptIDTable, ptPowerModule, ptIOControl, ptModulePart1, ptModulePart2, ptError, ptCheck,
    ptResponse, ptUnknown);

  TDacoM = class
  const
    SERVER_PORT = 8900;

    DEVICE_1000APS = '1000APS'; // DACO-M 1000(A, P, S)
    DEVICE_1000E = '1000E'; // DACP-M 1000E

    COMM_TCP = 'TCP';
    COMM_RS485 = 'RS485';

    READ_TIME_OUT = 3000;
  public
    class function GetModuleTitle(ADevice: string): string;
    class function GetMeaDataTitle: string;
  end;

implementation

class function TDacoM.GetMeaDataTitle: string;
var
  Header: TStrings;
begin
  Header := TStringList.Create;
  Header.Add('TimeStamp');
  Header.Add('Current(A)');
  Header.Add('Voltage(V)');
  Header.Add('Power(kW)');
  Header.Add('ActivePower(kW)');
  Header.Add('Energy(kWh)');
  Header.Add('PowerFactor');
  Header.Add('Peak(kW)');
  Header.Add('ReactivePower(Var)');
  result := Header.CommaText;
  Header.Free;
end;

function Get1000APS_Title: string;
var
  I: Integer;
  Header: TStrings;
begin
  Header := TStringList.Create;
  Header.Add('TimeStamp');
  Header.Add('Voltage_LN_A');
  Header.Add('Voltage_LN_B');
  Header.Add('Voltage_LN_C');
  Header.Add('Frequency_LN_A');
  Header.Add('Frequency_LN_B');
  Header.Add('Frequency_LN_C');
  Header.Add('Current_A');
  Header.Add('Current_B');
  Header.Add('Current_C');
  Header.Add('Current_average');
  Header.Add('Zero_sequence_component_current');
  Header.Add('Current_fundamental_A');
  Header.Add('Current_fundamental_B');
  Header.Add('Current_fundamental_C');
  Header.Add('Current_fundamental_average');
  Header.Add('Current_THD_A');
  Header.Add('Current_THD_B');
  Header.Add('Current_THD_C');
  Header.Add('Current_TDD_A');
  Header.Add('Current_TDD_B');
  Header.Add('Current_TDD_C');
  Header.Add('Current_phasor_A-X');
  Header.Add('Current_phasor_A-Y');
  Header.Add('Current_phasor_B-X');
  Header.Add('Current_phasor_B-Y');
  Header.Add('Current_phasor_C-X');
  Header.Add('Current_phasor_C-Y');
  Header.Add('Current_unbalance');
  Header.Add('Current_U0_unbalance');
  Header.Add('Current_U2_unbalance');
  Header.Add('Crest_factor_A');
  Header.Add('Crest_factor_B');
  Header.Add('Crest_factor_C');
  Header.Add('K_factor_A');
  Header.Add('K_factor_B');
  Header.Add('K_factor_C');
  Header.Add('KW_A');
  Header.Add('KW_B');
  Header.Add('KW_C');
  Header.Add('KW_total');
  Header.Add('KVAR_A');
  Header.Add('KVAR_B');
  Header.Add('KVAR_C');
  Header.Add('KVAR_total');
  Header.Add('KVA_A');
  Header.Add('KVA_B');
  Header.Add('KVA_C');
  Header.Add('KVA_total');
  Header.Add('ZCT_current');
  Header.Add('KWh_received');
  Header.Add('KWh_delivered');
  Header.Add('KWh_sum');
  Header.Add('KWh_net');
  Header.Add('KVARh_received');
  Header.Add('KVARh_delivered');
  Header.Add('KVARh_sum');
  Header.Add('KVARh_net');
  Header.Add('KVAh');
  Header.Add('Demand_KW_A');
  Header.Add('Demand_KW_B');
  Header.Add('Demand_KW_C');
  Header.Add('Demand_KW_total');
  Header.Add('Demand_KW_total_prediction');
  Header.Add('Demand_current_A');
  Header.Add('Demand_current_B');
  Header.Add('Demand_current_C');
  Header.Add('Demand_current_average');
  Header.Add('Demand_current_average_prediction');
  Header.Add('Power_factor_A');
  Header.Add('Power_factor_B');
  Header.Add('Power_factor_C');
  Header.Add('Power_factor_total');

  for I := 1 to 31 do
    Header.Add(Format('%dth_Harmonic', [I]));

  for I := 0 to 9 do
    Header.Add('Reserved');

  Header.Add('__SN');
  Header.Add('Connection_direction');
  Header.Add('Order_of_Wiring');
  Header.Add('Reserved');
  Header.Add('ProductRating');
  Header.Add('PFInfoA');
  Header.Add('PFInfoB');
  Header.Add('PFInfoC');

  // for I := 0 to 3 do
  // Header.Add('Reserved');

  result := Header.CommaText;
  Header.Free;
end;

function Get1000E_Title: string;
var
  Header: TStrings;
  I: Integer;
begin
  Header := TStringList.Create;
  Header.Add('TimeStamp');
  Header.Add('Voltage_LN_A');
  Header.Add('Voltage_LN_B');
  Header.Add('Voltage_LN_C');
  Header.Add('Frequency_LN_A');
  Header.Add('Frequency_LN_B');
  Header.Add('Frequency_LN_C');
  Header.Add('Current_A');
  Header.Add('Current_B');
  Header.Add('Current_C');
  Header.Add('Current_average');

  for I := 0 to 25 do
    Header.Add('Reserved');

  Header.Add('KW_A');
  Header.Add('KW_B');
  Header.Add('KW_C');
  Header.Add('KW_total');
  Header.Add('KVAR_A');
  Header.Add('KVAR_B');
  Header.Add('KVAR_C');
  Header.Add('KVAR_total');
  Header.Add('KVA_A');
  Header.Add('KVA_B');
  Header.Add('KVA_C');
  Header.Add('KVA_total');

  for I := 0 to 19 do
    Header.Add('Reserved');

  Header.Add('Power_factor_A');
  Header.Add('Power_factor_B');
  Header.Add('Power_factor_C');
  Header.Add('Power_factor_total');

  for I := 1 to 31 do
    Header.Add(Format('%dth_Harmonic', [I]));

  for I := 0 to 9 do
    Header.Add('Reserved');

  Header.Add('__SN');
  Header.Add('Connection_direction');
  Header.Add('Order_of_Wiring');

  for I := 0 to 4 do
    Header.Add('Reserved');

  // Header.Add('Voltage_LL_AB');
  // Header.Add('Voltage_LL_BC');
  // Header.Add('Voltage_LL_CA');
  // Header.Add('Voltage_LL_Average');
  result := Header.CommaText;
  Header.Free;
end;

class function TDacoM.GetModuleTitle(ADevice: string): string;
begin
  if ADevice = TDacoM.DEVICE_1000APS then
    result := Get1000APS_Title
  else
    result := Get1000E_Title;
end;

end.
