object daDispatch: TdaDispatch
  OldCreateOrder = False
  Left = 474
  Top = 333
  Height = 150
  Width = 215
  object coDispatch: TIBDatabase
    DatabaseName = 'agent2'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = trDispatchStat
    AllowStreamedConnected = False
    Left = 24
    Top = 8
  end
  object trDispatchStat: TIBTransaction
    DefaultDatabase = coDispatch
    Left = 24
    Top = 52
  end
  object sqAction: TIBSQL
    Database = coDispatch
    ParamCheck = False
    Transaction = trDispatchStat
    Left = 116
    Top = 8
  end
  object bsBackup: TIBBackupService
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    TraceFlags = []
    BlockingFactor = 0
    DatabaseName = 'agent2'
    Options = []
    Left = 116
    Top = 52
  end
end
