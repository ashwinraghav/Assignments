object daMain: TdaMain
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 366
  Top = 238
  Height = 426
  Width = 638
  object dbAgent: TIBDatabase
    Connected = True
    DatabaseName = 'agent'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey'
      'lc_ctype=ISO8859_1')
    LoginPrompt = False
    DefaultTransaction = taMain
    AllowStreamedConnected = False
    Left = 20
    Top = 12
  end
  object doCategoryInsert: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_CATEGORY_INSERT'
    Left = 92
    Top = 12
  end
  object taMain: TIBTransaction
    Active = True
    DefaultDatabase = dbAgent
    Left = 20
    Top = 56
  end
  object doActionInsert: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_ACTION_INSERT'
    Left = 92
    Top = 56
  end
  object doTaskInsert: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_TASK_INSERT'
    Left = 92
    Top = 100
  end
  object doScheduleCheck: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_SCHEDULE_CHECK'
    Left = 272
    Top = 56
    ParamData = <
      item
        DataType = ftInteger
        Name = 'NEW_SCHEDULE_ID'
        ParamType = ptOutput
      end
      item
        DataType = ftInteger
        Name = 'SCHEDULE_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'TASK_ID'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'NAME'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'ENABLED'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'DEL_KIND'
        ParamType = ptInput
      end>
  end
  object doTaskUpdate: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_TASK_UPD'
    Left = 176
    Top = 56
    ParamData = <
      item
        DataType = ftInteger
        Name = 'TASK_ID'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'NAME'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'ENABLED'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'CATEGORY_ID'
        ParamType = ptInput
      end
      item
        DataType = ftBlob
        Name = 'DESCRIPTION'
        ParamType = ptInput
      end>
  end
  object doStepInsert: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_STEP_INS'
    Left = 380
    Top = 56
    ParamData = <
      item
        DataType = ftInteger
        Name = 'STEP_ID'
        ParamType = ptOutput
      end
      item
        DataType = ftInteger
        Name = 'TASK_ID'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'NAME'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'ENABLED'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'ITEM_ORDER'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'ON_SUCCESS'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'ON_FAILURE'
        ParamType = ptInput
      end
      item
        DataType = ftBlob
        Name = 'DATA'
        ParamType = ptInput
      end>
  end
  object doStepUpdate: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_STEP_UPD'
    Left = 380
    Top = 12
    ParamData = <
      item
        DataType = ftInteger
        Name = 'STEP_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'TASK_ID'
        ParamType = ptInput
      end
      item
        DataType = ftString
        Name = 'NAME'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'ENABLED'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'ITEM_ORDER'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'ON_SUCCESS'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'ON_FAILURE'
        ParamType = ptInput
      end
      item
        DataType = ftBlob
        Name = 'DATA'
        ParamType = ptInput
      end>
  end
  object doStepDelete: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_STEP_DEL'
    Left = 380
    Top = 100
    ParamData = <
      item
        DataType = ftInteger
        Name = 'STEP_ID'
        ParamType = ptInput
      end>
  end
  object doScheduleDelete: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_SCHEDULE_DEL'
    Left = 272
    Top = 12
    ParamData = <
      item
        DataType = ftInteger
        Name = 'SCHEDULE_ID'
        ParamType = ptInput
      end>
  end
  object doRecurrenceCheck: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_RECURRENCE_CHECK'
    Left = 484
    Top = 12
    ParamData = <
      item
        DataType = ftInteger
        Name = 'NEW_RECURRENCE_ID'
        ParamType = ptOutput
      end
      item
        DataType = ftInteger
        Name = 'RECURRENCE_ID'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'SCHEDULE_ID'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'INCREMENT'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'DAY_FILTER_KIND'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'DAY_FILTER'
        ParamType = ptInput
      end
      item
        DataType = ftSmallint
        Name = 'DOW_POS_FILTER'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'MONTH_FILTER'
        ParamType = ptInput
      end
      item
        DataType = ftTime
        Name = 'START_TIME'
        ParamType = ptInput
      end
      item
        DataType = ftDate
        Name = 'START_DATE'
        ParamType = ptInput
      end
      item
        DataType = ftTime
        Name = 'END_TIME'
        ParamType = ptInput
      end
      item
        DataType = ftDate
        Name = 'END_DATE'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'RUN_COUNT'
        ParamType = ptInput
      end>
  end
  object doTaskDelete: TIBStoredProc
    Database = dbAgent
    Transaction = taMain
    StoredProcName = 'DO_TASK_DEL'
    Left = 176
    Top = 12
  end
end
