unit uNetPod;

interface

uses Classes, SysUtils, DevList, Windows, dllinc;
{
// 넷포드 채널 정보
type
  TChannelInfo = packed record
    No              : Integer;
    Name            : TShortString;
    Units           : TShortString;
    Resolutiuon     : Integer;
    PartNumber      : TShortString;
    SerialNumber    : TShortString;
    ManufactureDate : TShortString;
    Data            : Double;
    Gain            : Double;
    IsInstalled     : Boolean;
  end;

// 넷포드 정보
type
  TPodInfo = packed record
    ID              : Integer;
    Name            : TShortString;
    SampleRate      : Double;
    SWVersion       : Integer;
    IP              : TShortString;
    EthernetAddress : TShortString;
    PartNumber      : TShortString;
    SerialNumber    : TShortString;
    ManufactureDate : TShortString;
    DigitalDirection: Integer;
    DigitalDefault  : Integer;
    ChannelInfo     : packed array[0..15] of TChannelInfo;
  end;
}

type
  TDevChannel = record
    PodID     : Integer;
    Channel   : Integer;
  end;



procedure GetPodInfo(var PodList: TDevCollection; UseNetPod: Boolean = true);


implementation

procedure GetPodInfo(var PodList: TDevCollection; UseNetPod: Boolean = true);
var
  data : packed array[0..100] of Integer;
  i, j: integer;

  pdi: TPodInfoStruct;
  chi: TChannelInfoStruct;

  Channel: TChannelCollection;
begin
  PodList.Clear;

  if not UseNetPod then                 // 함수 발생기일 경우
    begin
      Channel := TChannelCollection.Create;
      Channel.AddItem(0, 'CH0', '-', 0, '-', '-', '-', -1, 1, true, 1);
      // 넷포드 추가
      PodList.AddItem(0, '사인파 발생기', 0, 1, '-', '-', '-', '-', '-', 0, 0, Channel);
    end
  else
    begin
      // 넷포드 리스트 구하기
      NP_GetPodList(data);

      i := 0;
      while i < High(data) do
        begin
          if data[i] = 0 then
            break;

          if NP_GetPodInfo(data[i], pdi) = 0 then
            with pdi do
              begin
                // 채널 정보
                Channel := TChannelCollection.Create;
                try
                  for j := 0 to 15 do
                    begin
                      NP_GetChannelInfo(data[i], j, chi);
                      if data[i] <> PodID then
                        break;

                      if chi.IsInstalled = 0 then
                        Channel.AddItem(chi.Channel, '',       chi.Units, -1,                '',             '',               '',             -1, -1,       not (chi.IsInstalled = 0), chi.SWVersion)
                      else
                        Channel.AddItem(chi.Channel, chi.Name, chi.Units, chi.ADCResolution, chi.PartNumber, chi.SerialNumber, chi.StrManDate, -1, chi.Gain, not (chi.IsInstalled = 0), chi.SWVersion);
                    end;

                  // 넷포드 추가
                  PodList.AddItem(PodID, Name, SampleRate, SWVersion, StrIPAddr, StrEthernet, PartNumber, SerialNumber, StrManDate, DigDDR, DigDefault, Channel);
                finally
                  //Channel.Free;
                end;
              end;
          inc(i);
        end;
    end;
end;




end.
