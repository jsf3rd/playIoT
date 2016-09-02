object ServiceMain: TServiceMain
  OldCreateOrder = False
  OnCreate = ServiceCreate
  AllowPause = False
  DisplayName = 'Service Templete'
  AfterInstall = ServiceAfterInstall
  AfterUninstall = ServiceAfterUninstall
  OnExecute = ServiceExecute
  OnShutdown = ServiceShutdown
  OnStart = ServiceStart
  OnStop = ServiceStop
  Height = 150
  Width = 215
end
