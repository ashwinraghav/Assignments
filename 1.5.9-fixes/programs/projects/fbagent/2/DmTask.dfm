object daTask: TdaTask
  OldCreateOrder = False
  Left = 503
  Top = 644
  Height = 270
  Width = 504
  object doCategoryDel: TIBStoredProc
    Database = daAgent.dbAgent
    Transaction = taCategory
    StoredProcName = 'DO_CATEGORY_DEL'
    Left = 212
    Top = 144
    ParamData = <
      item
        DataType = ftInteger
        Name = 'CATEGORY_ID'
        ParamType = ptInput
      end>
  end
  object taCategory: TIBTransaction
    DefaultDatabase = daAgent.dbAgent
    Left = 212
    Top = 96
  end
  object quCategory: TIBQuery
    Database = daAgent.dbAgent
    Transaction = taCategory
    SQL.Strings = (
      'select * from qry_category_list')
    Left = 212
    Top = 52
  end
  object doCategoryCheck: TIBStoredProc
    Database = daAgent.dbAgent
    Transaction = taCategory
    StoredProcName = 'DO_CATEGORY_CHECK'
    Left = 212
    Top = 8
    ParamData = <
      item
        DataType = ftInteger
        Name = 'CATEGORY_ID'
        ParamType = ptOutput
      end
      item
        DataType = ftString
        Name = 'NAME'
        ParamType = ptInput
      end>
  end
  object doHistoryDel: TIBStoredProc
    Database = daAgent.dbAgent
    Transaction = taCategory
    StoredProcName = 'DO_HISTORY_DEL'
    Left = 300
    Top = 8
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
  object quHistory: TIBQuery
    Database = daAgent.dbAgent
    Transaction = taHistory
    SQL.Strings = (
      'select * from qry_history_task(:task_id, :show_step)')
    Left = 140
    Top = 52
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'task_id'
        ParamType = ptUnknown
      end
      item
        DataType = ftUnknown
        Name = 'show_step'
        ParamType = ptUnknown
      end>
    object quHistoryITEM_ORDER: TIntegerField
      DisplayLabel = 'Order'
      DisplayWidth = 5
      FieldName = 'ITEM_ORDER'
      Origin = 'QRY_HISTORY_TASK.ITEM_ORDER'
    end
    object quHistoryNAME: TIBStringField
      DisplayLabel = 'Step Name'
      FieldName = 'NAME'
      Origin = 'QRY_HISTORY_TASK.NAME'
      Size = 64
    end
    object quHistorySTART_TIME: TDateTimeField
      DisplayLabel = 'Start Time'
      FieldName = 'START_TIME'
      Origin = 'QRY_HISTORY_TASK.START_TIME'
    end
    object quHistoryEND_TIME: TDateTimeField
      DisplayLabel = 'Run Duration'
      FieldName = 'END_TIME'
      Origin = 'QRY_HISTORY_TASK.END_TIME'
      OnGetText = quHistoryEND_TIMEGetText
    end
    object quHistorySTATUS: TIBStringField
      DisplayLabel = 'Result'
      FieldName = 'STATUS'
      Origin = 'QRY_HISTORY_TASK.STATUS'
      FixedChar = True
      Size = 3
    end
    object quHistoryTEXT_MESSAGE: TIBStringField
      DisplayLabel = 'Message'
      FieldName = 'TEXT_MESSAGE'
      Origin = 'QRY_HISTORY_TASK.TEXT_MESSAGE'
      Size = 1024
    end
  end
  object taHistory: TIBTransaction
    DefaultDatabase = daAgent.dbAgent
    Left = 140
    Top = 96
  end
  object taSchedule: TIBTransaction
    DefaultDatabase = daAgent.dbAgent
    Left = 76
    Top = 96
  end
  object quSchedule: TIBQuery
    Database = daAgent.dbAgent
    Transaction = taSchedule
    SQL.Strings = (
      'select * from qry_schedules(:task_id)')
    Left = 76
    Top = 52
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'task_id'
        ParamType = ptUnknown
      end>
    object quScheduleSCHEDULE_ID: TIntegerField
      FieldName = 'SCHEDULE_ID'
      Origin = 'QRY_SCHEDULES.SCHEDULE_ID'
      Visible = False
    end
    object quScheduleNAME: TIBStringField
      DisplayLabel = 'Name'
      FieldName = 'NAME'
      Origin = 'QRY_SCHEDULES.NAME'
      Size = 64
    end
    object quScheduleENABLED: TSmallintField
      Alignment = taLeftJustify
      DisplayLabel = 'Enabled'
      DisplayWidth = 8
      FieldName = 'ENABLED'
      Origin = 'QRY_SCHEDULES.ENABLED'
      OnGetText = GetEnabledText
    end
    object quScheduleDEL_KIND: TSmallintField
      FieldName = 'DEL_KIND'
      Origin = 'QRY_SCHEDULES.DEL_KIND'
      Visible = False
    end
    object quScheduleINCREMENT: TDateTimeField
      FieldName = 'INCREMENT'
      Origin = 'QRY_SCHEDULES.INCREMENT'
      Visible = False
    end
    object quScheduleDAY_FILTER_KIND: TSmallintField
      FieldName = 'DAY_FILTER_KIND'
      Origin = 'QRY_SCHEDULES.DAY_FILTER_KIND'
      Visible = False
    end
    object quScheduleDAY_FILTER: TIntegerField
      FieldName = 'DAY_FILTER'
      Origin = 'QRY_SCHEDULES.DAY_FILTER'
      Visible = False
    end
    object quScheduleMONTH_FILTER: TIntegerField
      FieldName = 'MONTH_FILTER'
      Origin = 'QRY_SCHEDULES.MONTH_FILTER'
      Visible = False
    end
    object quScheduleDOW_POS_FILTER: TIntegerField
      FieldName = 'DOW_POS_FILTER'
      Origin = 'QRY_SCHEDULES.DOW_POS_FILTER'
      Visible = False
    end
    object quScheduleSTART_TIME: TTimeField
      FieldName = 'START_TIME'
      Origin = 'QRY_SCHEDULES.START_TIME'
      Visible = False
    end
    object quScheduleSTART_DATE: TDateField
      FieldName = 'START_DATE'
      Origin = 'QRY_SCHEDULES.START_DATE'
      Visible = False
    end
    object quScheduleEND_TIME: TTimeField
      FieldName = 'END_TIME'
      Origin = 'QRY_SCHEDULES.END_TIME'
      Visible = False
    end
    object quScheduleEND_DATE: TDateField
      FieldName = 'END_DATE'
      Origin = 'QRY_SCHEDULES.END_DATE'
      Visible = False
    end
    object quScheduleRUN_COUNT: TIntegerField
      FieldName = 'RUN_COUNT'
      Origin = 'QRY_SCHEDULES.RUN_COUNT'
      Visible = False
    end
  end
  object quStep: TIBQuery
    Database = daAgent.dbAgent
    Transaction = taStep
    SQL.Strings = (
      'select * from qry_steps(:task_id)')
    Left = 20
    Top = 52
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'task_id'
        ParamType = ptUnknown
      end>
    object quStepSTEP_ID: TIntegerField
      FieldName = 'STEP_ID'
      Origin = 'QRY_STEPS.STEP_ID'
      Visible = False
    end
    object quStepNAME: TIBStringField
      DisplayLabel = 'Name'
      FieldName = 'NAME'
      Origin = 'QRY_STEPS.NAME'
      Size = 64
    end
    object quStepENABLED: TSmallintField
      Alignment = taLeftJustify
      DisplayLabel = 'Enabled'
      DisplayWidth = 8
      FieldName = 'ENABLED'
      Origin = 'QRY_STEPS.ENABLED'
      OnGetText = GetEnabledText
    end
    object quStepITEM_ORDER: TIntegerField
      FieldName = 'ITEM_ORDER'
      Origin = 'QRY_STEPS.ITEM_ORDER'
      Visible = False
    end
    object quStepON_SUCCESS: TIntegerField
      FieldName = 'ON_SUCCESS'
      Origin = 'QRY_STEPS.ON_SUCCESS'
      Visible = False
    end
    object quStepON_FAILURE: TIntegerField
      FieldName = 'ON_FAILURE'
      Origin = 'QRY_STEPS.ON_FAILURE'
      Visible = False
    end
    object quStepDATA: TMemoField
      FieldName = 'DATA'
      Origin = 'QRY_STEPS.DATA'
      Visible = False
      BlobType = ftMemo
      Size = 8
    end
  end
  object taStep: TIBTransaction
    DefaultDatabase = daAgent.dbAgent
    Left = 20
    Top = 96
  end
end
