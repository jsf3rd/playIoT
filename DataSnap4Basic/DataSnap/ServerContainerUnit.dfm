object ServerContainer: TServerContainer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 234
  Width = 325
  object DSServer: TDSServer
    OnConnect = DSServerConnect
    AutoStart = False
    Left = 96
    Top = 11
  end
  object DSTCPServerTransport: TDSTCPServerTransport
    Server = DSServer
    Filters = <>
    Left = 96
    Top = 73
  end
  object dscDataLoader: TDSServerClass
    OnGetClass = dscDataLoaderGetClass
    Server = DSServer
    Left = 224
    Top = 11
  end
  object dscDataProvier: TDSServerClass
    OnGetClass = dscDataProvierGetClass
    Server = DSServer
    Left = 224
    Top = 72
  end
end
