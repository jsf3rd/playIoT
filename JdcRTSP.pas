unit JdcRTSP;

interface

uses System.SysUtils, System.Classes, Winapi.Windows, ONVIF, JdcGlobal, JdcLogging;

type
  plibvlc_instance_t = type Pointer;
  plibvlc_media_player_t = type Pointer;
  plibvlc_media_t = type Pointer;

  TRTSP = class
  strict private
    FConnInfo: TConnInfo;
    FUser: string;
    FPassword: string;
    FOption: string;

    FURI: string;

    vlcLib: NativeInt;
    vlcInstance: plibvlc_instance_t;
    vlcMedia: plibvlc_media_t;
    vlcMediaPlayer: plibvlc_media_player_t;

    libvlc_media_new_path: function(p_instance: plibvlc_instance_t; path: PAnsiChar)
      : plibvlc_media_t; cdecl;
    libvlc_media_new_location: function(p_instance: plibvlc_instance_t; psz_mrl: PAnsiChar)
      : plibvlc_media_t; cdecl;
    libvlc_media_player_new_from_media: function(p_media: plibvlc_media_t)
      : plibvlc_media_player_t; cdecl;
    libvlc_media_player_set_hwnd: procedure(p_media_player: plibvlc_media_player_t;
      drawable: Pointer); cdecl;
    libvlc_media_player_play: procedure(p_media_player: plibvlc_media_player_t); cdecl;
    libvlc_media_player_stop: procedure(p_media_player: plibvlc_media_player_t); cdecl;
    libvlc_media_player_release: procedure(p_media_player: plibvlc_media_player_t); cdecl;
    libvlc_media_player_is_playing: function(p_media_player: plibvlc_media_player_t)
      : Integer; cdecl;
    libvlc_media_release: procedure(p_media: plibvlc_media_t); cdecl;
    libvlc_new: function(argc: Integer; argv: PAnsiChar): plibvlc_instance_t; cdecl;
    libvlc_media_add_option: procedure(p_md: plibvlc_media_t; ppsz_options: PAnsiChar); cdecl;
    libvlc_release: procedure(p_instance: plibvlc_instance_t); cdecl;
    libvlc_video_set_aspect_ratio: procedure(media_player: plibvlc_media_player_t;
      psz_aspect: PAnsiChar); cdecl;
    libvlc_video_take_snapshot: procedure(media_player: plibvlc_media_player_t; num: Integer;
      psz_filepath: PAnsiChar; i_width: Cardinal; i_height: Cardinal); cdecl;

    function LoadVLCLibrary(APath: string): THandle;
    function GetAProcAddress(Handle: THandle; var addr: Pointer; procName: PAnsiChar;
      failedList: TStringList): Integer;
    function LoadVLCFunctions(vlcHandle: THandle; failedList: TStringList): Boolean;
  private
    FRtspPort: Integer;
    function _Play(AIndex: Integer; AHandle: HWND; Caching: string = '150'): Boolean;
    procedure _Stop;
  public
    constructor Create(ConnInfo: TConnInfo; AUser, APassword, ARemark: string);
    destructor Destroy; override;

    function Play(AIndex: Integer; AHandle: HWND; Caching: string = '150'): Boolean;
    procedure SetAspectRatio(W, H: Integer);
    procedure Stop;
    function IsPlaying: Boolean;

    procedure SnapShot(AFileName: String; width: Integer; height: Integer);

    property RtspPort: Integer read FRtspPort write FRtspPort;
    property URI: string read FURI;
  end;

implementation

var
  CritSect: TRTLCriticalSection;

  { TRTSP }

function TRTSP.LoadVLCFunctions(vlcHandle: THandle; failedList: TStringList): Boolean;
begin
  GetAProcAddress(vlcHandle, @libvlc_new, 'libvlc_new', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_new_location, 'libvlc_media_new_location', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_new_from_media,
    'libvlc_media_player_new_from_media', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_release, 'libvlc_media_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_set_hwnd, 'libvlc_media_player_set_hwnd',
    failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_play, 'libvlc_media_player_play', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_stop, 'libvlc_media_player_stop', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_release, 'libvlc_media_player_release',
    failedList);
  GetAProcAddress(vlcHandle, @libvlc_release, 'libvlc_release', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_player_is_playing, 'libvlc_media_player_is_playing',
    failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_new_path, 'libvlc_media_new_path', failedList);
  GetAProcAddress(vlcHandle, @libvlc_media_add_option, 'libvlc_media_add_option', failedList);
  GetAProcAddress(vlcHandle, @libvlc_video_set_aspect_ratio, 'libvlc_video_set_aspect_ratio',
    failedList);
  GetAProcAddress(vlcHandle, @libvlc_video_take_snapshot, 'libvlc_video_take_snapshot', failedList);
  // if all functions loaded, result is an empty list, otherwise result is a list of functions failed
  Result := failedList.Count = 0;
end;

function TRTSP.LoadVLCLibrary(APath: string): THandle;
begin
  LoadLibrary(PChar(APath + 'libvlccore.dll'));
  Result := LoadLibrary(PChar(APath + 'libvlc.dll'));
end;

function TRTSP.Play(AIndex: Integer; AHandle: HWND; Caching: string): Boolean;
begin
  EnterCriticalSection(CritSect);
  try
    Result := _Play(AIndex, AHandle, Caching);
  finally
    LeaveCriticalSection(CritSect);
  end;
end;

procedure TRTSP.SetAspectRatio(W, H: Integer);
begin
  if Assigned(vlcMediaPlayer) then
    libvlc_video_set_aspect_ratio(vlcMediaPlayer, PAnsiChar(AnsiString(Format('%d:%d', [W, H]))));
end;

procedure TRTSP.SnapShot(AFileName: String; width: Integer; height: Integer);
begin
  libvlc_video_take_snapshot(vlcMediaPlayer, 0, PAnsiChar(AnsiString(AFileName)), width, height);
end;

procedure TRTSP.Stop;
begin
  EnterCriticalSection(CritSect);
  try
    _Stop;
  finally
    LeaveCriticalSection(CritSect);
  end;
end;

function TRTSP._Play(AIndex: Integer; AHandle: HWND; Caching: string): Boolean;
var
  profxml, StreamXml: string;
  prof: TProfiles;
  URL: string;
  StreamUri: TStreamUri;
const
  ERROR_MSG = 'Can''t locate the url';

  LibVLCOptions: array [0 .. 1] of PAnsiChar = //
    (PAnsiChar('--no-snapshot-preview'), //
    PAnsiChar('--no-osd') //
    );
begin
  if FConnInfo.StringValue.Contains('/') then
  begin
    // has URI
    FURI := FConnInfo.StringValue;
    URL := 'rtsp://' + FURI.Insert(Pos('/', FURI) - 1, Format(':%d', [FConnInfo.IntegerValue]));
  end
  else
  begin
    profxml := '';
    try
      profxml := Trim(ONVIFGetProfiles('http://' + FConnInfo.ToString + '/onvif/media', FUser,
        FPassword));

    except
      on E: Exception do
        TLogging.Obj.ApplicationMessage(msError, 'ONVIF', E.Message);
    end;

    if profxml = ERROR_MSG then
      profxml := Trim(ONVIFGetProfiles('http://' + FConnInfo.ToString + '/onvif/media_service',
        FUser, FPassword));

    XMLProfilesToProfiles(profxml, prof);
    if Length(prof) = 0 then
      raise Exception.Create('No channel list');

    StreamXml := Trim(ONVIFGetStreamUri('http://' + FConnInfo.ToString + '/onvif/media', FUser,
      FPassword, 'RTP-Unicast', 'RTSP', prof[AIndex].token));
    if StreamXml = ERROR_MSG then
      StreamXml := Trim(ONVIFGetStreamUri('http://' + FConnInfo.ToString + '/onvif/media_service',
        FUser, FPassword, 'RTP-Unicast', 'RTSP', prof[AIndex].token));

    XMLStreamUriToStreamUri(StreamXml, StreamUri);

    if StreamUri.URI = '' then
      raise Exception.Create('No RTSP URI');

    FURI := StreamUri.URI;

    // FURI := 'rtsp://192.168.11.100:554/profile2/media.smp';
    // FURI := 'rtsp://ys01.jrpwim.com:55415/media/video1';

    if FRtspPort = 0 then
    begin
      // 카메라에 설정된 RTSP포트 사용
      URL := FURI.Substring(Pos(':', FURI, 7));
      URL := Format('rtsp://%s:%s@%s:%s', [FUser, FPassword, FConnInfo.StringValue, URL]);
    end
    else
    begin
      // 사용자가 설정한 RTSP포트 사용
      URL := FURI.Substring(Pos('/', FURI, 7));
      URL := Format('rtsp://%s:%s@%s:%d/%s', [FUser, FPassword, FConnInfo.StringValue,
        FRtspPort, URL]);
    end;
  end;

  TLogging.Obj.ApplicationMessage(msInfo, 'libvlc_new', URL);
  vlcInstance := libvlc_new(Length(LibVLCOptions), @LibVLCOptions[0]);

  vlcMedia := libvlc_media_new_location(vlcInstance, PAnsiChar(AnsiString(URL)));
  libvlc_media_add_option(vlcMedia, PAnsiChar(AnsiString(':network-caching=' + Caching)));

  libvlc_media_add_option(vlcMedia, ':clock-jitter=0');
  libvlc_media_add_option(vlcMedia, ':clock-synchro=0');
  vlcMediaPlayer := libvlc_media_player_new_from_media(vlcMedia);
  libvlc_video_set_aspect_ratio(vlcMediaPlayer, '16:9');
  libvlc_media_player_set_hwnd(vlcMediaPlayer, Pointer(AHandle));
  libvlc_media_player_play(vlcMediaPlayer);
  Result := IsPlaying;
end;

procedure TRTSP._Stop;
var
  Count: Integer;
begin
  if Assigned(vlcMediaPlayer) then
  begin
    libvlc_media_player_stop(vlcMediaPlayer);
    Count := 0;
    while libvlc_media_player_is_playing(vlcMediaPlayer) = 1 do
    begin
      Sleep(100);
      Inc(Count);

      if Count > 10 then
        raise Exception.Create('StopTimeOut');
    end;
    libvlc_media_player_release(vlcMediaPlayer);
    vlcMediaPlayer := nil;
  end;

  if Assigned(vlcMedia) then
  begin
    libvlc_media_release(vlcMedia);
    vlcMedia := nil;
  end;

  if Assigned(vlcInstance) then
  begin
    libvlc_release(vlcInstance);
    vlcInstance := nil;
  end;
end;

constructor TRTSP.Create(ConnInfo: TConnInfo; AUser, APassword, ARemark: string);
var
  sL: TStringList;
  _path: string;
begin
  FConnInfo := ConnInfo;
  FUser := AUser;
  FPassword := APassword;
  FOption := ARemark;
  FRtspPort := 0;
  FURI := '';

  _path := ExtractFilePath(ParamStr(0));
  vlcLib := LoadVLCLibrary(_path);
  if vlcLib = 0 then
    raise Exception.Create('Load vlc library failed');

  sL := TStringList.Create;
  try
    if not LoadVLCFunctions(vlcLib, sL) then
    begin
      FreeLibrary(vlcLib);
      raise Exception.Create('failed to load : ' + #13#10 + sL.Text);
    end;
  finally
    sL.Free;
  end;
end;

destructor TRTSP.Destroy;
begin
  Self.Stop;
  FreeLibrary(vlcLib);
  inherited;
end;

function TRTSP.GetAProcAddress(Handle: THandle; var addr: Pointer; procName: PAnsiChar;
  failedList: TStringList): Integer;
begin
  addr := GetProcAddress(Handle, procName);
  if Assigned(addr) then
    Result := 0
  else
  begin
    if Assigned(failedList) then
      failedList.Add(String(procName));
    Result := -1;
  end;
end;

function TRTSP.IsPlaying: Boolean;
begin
  Result := Assigned(vlcMediaPlayer) and (libvlc_media_player_is_playing(vlcMediaPlayer) = 1);
end;

initialization

InitializeCriticalSection(CritSect);

finalization

DeleteCriticalSection(CritSect);

end.
