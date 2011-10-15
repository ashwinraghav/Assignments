object foTask: TfoTask
  Left = 313
  Top = 252
  Width = 478
  Height = 313
  ActiveControl = edTaskName
  BorderIcons = [biSystemMenu]
  Caption = 'Edici'#243'n de tarea'
  Color = clBtnFace
  Constraints.MinHeight = 258
  Constraints.MinWidth = 464
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    470
    286)
  PixelsPerInch = 96
  TextHeight = 13
  object pcTasks: TPageControl
    Left = 4
    Top = 4
    Width = 460
    Height = 245
    ActivePage = tsTask
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsTask: TTabSheet
      Caption = 'Tarea'
      object Label1: TLabel
        Left = 4
        Top = 8
        Width = 40
        Height = 13
        Caption = 'Nombre:'
      end
      object Label2: TLabel
        Left = 4
        Top = 64
        Width = 59
        Height = 13
        Caption = 'Descripci'#243'n:'
      end
      object Label3: TLabel
        Left = 4
        Top = 36
        Width = 48
        Height = 13
        Caption = 'Categoria:'
      end
      object edTaskName: TEdit
        Left = 68
        Top = 4
        Width = 293
        Height = 21
        TabOrder = 0
      end
      object cbTaskEnabled: TCheckBox
        Left = 368
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Habilitada'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object meTaskDescription: TMemo
        Left = 4
        Top = 84
        Width = 357
        Height = 73
        ScrollBars = ssVertical
        TabOrder = 4
      end
      object edTaskCategory: TComboBox
        Left = 68
        Top = 32
        Width = 233
        Height = 21
        ItemHeight = 13
        TabOrder = 2
      end
      object btDeleteCategory: TButton
        Left = 304
        Top = 32
        Width = 57
        Height = 21
        Caption = 'Borrar'
        TabOrder = 3
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'Pasos'
      ImageIndex = 1
      DesignSize = (
        452
        217)
      object gtSteps: TDBGrid
        Left = 4
        Top = 4
        Width = 444
        Height = 177
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsStep
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = btStepChangeClick
        Columns = <
          item
            Expanded = False
            FieldName = 'NAME'
            Title.Caption = 'Nombre'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ENABLED'
            Title.Caption = 'Habilitada'
            Visible = True
          end>
      end
      object btStepAdd: TButton
        Left = 203
        Top = 188
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Agregar ...'
        TabOrder = 1
        OnClick = btStepAddClick
      end
      object btStepChange: TButton
        Left = 287
        Top = 188
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Cambiar ...'
        TabOrder = 2
        OnClick = btStepChangeClick
      end
      object btStepDelete: TButton
        Left = 371
        Top = 188
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Borrar ...'
        TabOrder = 3
        OnClick = btStepDeleteClick
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Ejecuci'#243'n'
      ImageIndex = 2
      DesignSize = (
        452
        217)
      object grSchedules: TDBGrid
        Left = 4
        Top = 4
        Width = 444
        Height = 177
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsSchedule
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = btScheduleChangeClick
        Columns = <
          item
            Expanded = False
            FieldName = 'NAME'
            Title.Caption = 'Nombre'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ENABLED'
            Title.Caption = 'Habilitado'
            Visible = True
          end>
      end
      object btScheduleAdd: TButton
        Left = 203
        Top = 188
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Agregar ...'
        TabOrder = 1
        OnClick = btScheduleAddClick
      end
      object btScheduleChange: TButton
        Left = 287
        Top = 188
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Cambiar ...'
        TabOrder = 2
        OnClick = btScheduleChangeClick
      end
      object btScheduleDelete: TButton
        Left = 371
        Top = 188
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Borrar ...'
        TabOrder = 3
        OnClick = btScheduleDeleteClick
      end
    end
  end
  object btCancel: TButton
    Left = 388
    Top = 256
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 2
    OnClick = btCancelClick
  end
  object btAccept: TButton
    Left = 304
    Top = 256
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Aceptar'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btAcceptClick
  end
  object taStep: TIBTransaction
    DefaultDatabase = daMain.dbAgent
    Left = 316
    Top = 48
  end
  object qryStep: TIBQuery
    Database = daMain.dbAgent
    Transaction = taStep
    SQL.Strings = (
      'select * from qry_steps(:task_id)')
    Left = 244
    Top = 48
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'task_id'
        ParamType = ptUnknown
      end>
  end
  object dsStep: TDataSource
    AutoEdit = False
    DataSet = qryStep
    Left = 172
    Top = 48
  end
  object qrySchedule: TIBQuery
    Database = daMain.dbAgent
    Transaction = taSchedule
    SQL.Strings = (
      'select * from qry_schedules(:task_id)')
    Left = 244
    Top = 92
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'task_id'
        ParamType = ptUnknown
      end>
  end
  object dsSchedule: TDataSource
    AutoEdit = False
    DataSet = qrySchedule
    Left = 172
    Top = 92
  end
  object taSchedule: TIBTransaction
    DefaultDatabase = daMain.dbAgent
    Left = 316
    Top = 92
  end
  object qryCategory: TIBQuery
    Database = daMain.dbAgent
    Transaction = taCategory
    SQL.Strings = (
      'select * from qry_category_list')
    Left = 244
    Top = 136
  end
  object doCategoryCheck: TIBStoredProc
    Database = daMain.dbAgent
    Transaction = taCategory
    StoredProcName = 'DO_CATEGORY_CHECK'
    Left = 172
    Top = 136
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
  object taCategory: TIBTransaction
    DefaultDatabase = daMain.dbAgent
    Left = 316
    Top = 136
  end
end
