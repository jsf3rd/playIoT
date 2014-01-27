object smDataLoader: TsmDataLoader
  OldCreateOrder = False
  OnCreate = DSServerModuleCreate
  OnDestroy = DSServerModuleDestroy
  Height = 224
  Width = 346
  object FDConnection: TFDConnection
    Left = 64
    Top = 32
  end
  object FDQuery: TFDQuery
    Connection = FDConnection
    Left = 152
    Top = 32
  end
end
