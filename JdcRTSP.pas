unit JdcRTSP;

interface

uses System.SysUtils, System.Classes, Winapi.Windows, ONVIF, JdcGlobal;

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

    vlcLib: Integer;
    vlcInstance: plibvlc_instance_t;
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

    function LoadVLCLibrary(APath: string): Integer;
    function GetAProcAddress(Handle: Integer; var addr: Pointer; procName: string;
      failedList: TStringList): Integer;
    function LoadVLCFunctions(vlcHandle: Integer; failedList: TStringList): Boolean;
  private
    FRtspPort: Integer;
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

{ TRTSP }

function TRTSP.LoadVLCFunctions(vlcHandle: Integer; failedList: TStringList): Boolean;
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

function TRTSP.LoadVLCLibrary(APath: string): Integer;
begin
  Result := LoadLibrary(PChar(APath + 'libvlccore.dll'));
  Result := LoadLibrary(PChar(APath + 'libvlc.dll'));
end;

function TRTSP.Play(AIndex: Integer; AHandle: HWND; Caching: string): Boolean;
var
  vlcMedia: plibvlc_media_t;

  profxml, StreamXml: string;
  prof: TProfiles;
  URL: string;
  StreamUri: TStreamUri;
begin
  profxml := Trim(ONVIFGetProfiles('http://' + FConnInfo.ToString + '/onvif/media', FUser,
    FPassword));
  XMLProfilesToProfiles(profxml, prof);
  if Length(prof) = 0 then
    raise Exception.Create('No channel list');

  StreamXml := Trim(ONVIFGetStreamUri('http://' + FConnInfo.ToString + '/onvif/media', FUser,
    FPassword, 'RTP-Unicast', 'RTSP', prof[AIndex].token));

  XMLStreamUriToStreamUri(StreamXml, StreamUri);

  if StreamUri.URI = '' then
    raise Exception.Create('No RTSP URI');

  FURI := StreamUri.URI;
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

  vlcInstance := libvlc_new(0, nil);
  vlcMedia := libvlc_media_new_location(vlcInstance, PAnsiChar(AnsiString(URL)));
  libvlc_media_add_option(vlcMedia, PAnsiChar(AnsiString(':network-caching=' + Caching)));
  libvlc_media_add_option(vlcMedia, ':clock-jitter=0');
  libvlc_media_add_option(vlcMedia, ':clock-synchro=0');
  vlcMediaPlayer := libvlc_media_player_new_from_media(vlcMedia);
  libvlc_media_release(vlcMedia);
  libvlc_video_set_aspect_ratio(vlcMediaPlayer, '16:9');
  libvlc_media_player_set_hwnd(vlcMediaPlayer, Pointer(AHandle));
  libvlc_media_player_play(vlcMediaPlayer);
  Result := IsPlaying;
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

  if Assigned(vlcInstance) then
  begin
    libvlc_release(vlcInstance);
    vlcInstance := nil;
  end;
end;

constructor TRTSP.Create(ConnInfo: TConnInfo; AUser, APassword, ARemark: string);
var
  sL: TStringList;
begin
  FConnInfo := ConnInfo;
  FUser := AUser;
  FPassword := APassword;
  FOption := ARemark;
  FRtspPort := 0;
  FURI := '';

  vlcLib := LoadVLCLibrary(ExtractFilePath(ParamStr(0)));
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
  FreeLibrary(vlcLib);
  inherited;
end;

function TRTSP.GetAProcAddress(Handle: Integer; var addr: Pointer; procName: string;
  failedList: TStringList): Integer;
begin
  addr := GetProcAddress(Handle, PWideChar(procName));
  if Assigned(addr) then
    Result := 0
  else
  begin
    if Assigned(failedList) then
      failedList.Add(procName);
    Result := -1;
  end;
end;

function TRTSP.IsPlaying: Boolean;
begin
  Result := Assigned(vlcMediaPlayer) and (libvlc_media_player_is_playing(vlcMediaPlayer) = 1);
end;

end.
