object smDataProvider: TsmDataProvider
  OldCreateOrder = False
  OnCreate = DSServerModuleCreate
  Height = 150
  Width = 215
  object qryMember: TFDQuery
    UpdateOptions.UpdateTableName = 'Member'
    Left = 88
    Top = 56
  end
end
