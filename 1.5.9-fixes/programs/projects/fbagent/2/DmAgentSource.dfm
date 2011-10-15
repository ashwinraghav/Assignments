object daAgentSource: TdaAgentSource
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 494
  Top = 285
  Height = 292
  Width = 290
  object coAgent: TIBDatabase
    DatabaseName = 'localhost:agent'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey'
      'lc_ctype=ISO8859_1')
    LoginPrompt = False
    DefaultTransaction = trRunStat
    AllowStreamedConnected = False
    Left = 24
    Top = 4
  end
  object trRunStat: TIBTransaction
    DefaultDatabase = coAgent
    Left = 24
    Top = 48
  end
  object doRunStatUpd: TIBStoredProc
    Database = coAgent
    Transaction = trAction
    StoredProcName = 'DO_RUN_UPD'
    Left = 116
    Top = 4
    ParamData = <
      item
        DataType = ftInteger
        Name = 'RUN_ID'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'LAST_RUN'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'NEXT_RUN'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'REPEAT'
        ParamType = ptInput
      end>
  end
  object quRunStat: TIBQuery
    Database = coAgent
    Transaction = trRunStat
    SQL.Strings = (
      'select * from qry_run')
    Left = 116
    Top = 48
  end
  object doHistoryCheck: TIBStoredProc
    Database = coAgent
    Transaction = trAction
    StoredProcName = 'DO_HISTORY_CHECK'
    Left = 116
    Top = 92
    ParamData = <
      item
        DataType = ftInteger
        Name = 'HISTORY_ID_OUT'
        ParamType = ptOutput
      end
      item
        DataType = ftInteger
        Name = 'HISTORY_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'PARENT_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'TASK_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'STEP_ID'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'START_TIME'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'END_TIME'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'STATUS'
        ParamType = ptInput
      end
      item
        DataType = ftBlob
        Name = 'TEXT_MESSAGE'
        ParamType = ptInput
      end>
  end
  object trAction: TIBTransaction
    DefaultDatabase = coAgent
    Left = 24
    Top = 96
  end
  object doHistoryDel: TIBStoredProc
    Database = coAgent
    Transaction = trAction
    StoredProcName = 'DO_HISTORY_DEL'
    Left = 116
    Top = 136
    ParamData = <
      item
        DataType = ftInteger
        Name = 'TASK_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'MAX_SIZE'
        ParamType = ptInput
      end>
  end
  object quTaskNotification: TIBQuery
    Database = coAgent
    Transaction = trRunStat
    SQL.Strings = (
      'select * from qry_task_notification(:task_id)')
    Left = 116
    Top = 180
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'task_id'
        ParamType = ptUnknown
      end>
  end
end
