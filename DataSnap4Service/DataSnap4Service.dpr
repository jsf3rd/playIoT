program DataSnap4Service;

uses
{$IFDEF DEBUG}
  // FastMM4, FastMM4Messages,
{$ENDIF}
  Vcl.SvcMgr,
  _smDataProvider in 'DataSnap\_smDataProvider.pas' {smDataProvider: TDSServerModule},
  _ServerContainer in 'DataSnap\_ServerContainer.pas' {ServerContainer: TService},
  MyGlobal in 'Global\MyGlobal.pas',
  MyOption in 'Global\MyOption.pas';

{$R *.RES}

begin
  // Windows 2003 Server requires StartServiceCtrlDispatcher to be
  // called before CoRegisterClassObject, which can be called indirectly
  // by Application.Initialize. TServiceApplication.DelayInitialize allows
  // Application.Initialize to be called from TService.Main (after
  // StartServiceCtrlDispatcher has been called).
  //
  // Delayed initialization of the Application object may affect
  // events which then occur prior to initialization, such as
  // TService.OnCreate. It is only recommended if the ServiceApplication
  // registers a class object with OLE and is intended for use with
  // Windows 2003 Server.
  //
  // Application.DelayInitialize := True;
  //
  if not Application.DelayInitialize or Application.Installing then
    Application.Initialize;
  Application.CreateForm(TServerContainer, ServerContainer);
  Application.Run;

end.
