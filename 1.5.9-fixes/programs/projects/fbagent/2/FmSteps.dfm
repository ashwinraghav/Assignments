object foSteps: TfoSteps
  Left = 370
  Top = 239
  Width = 475
  Height = 347
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'foSteps'
  Color = clBtnFace
  Constraints.MinHeight = 260
  Constraints.MinWidth = 352
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    467
    320)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 8
    Top = 8
    Width = 454
    Height = 272
    ActivePage = Tareas
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabIndex = 0
    TabOrder = 0
    object Tareas: TTabSheet
      Caption = 'Tareas'
      DesignSize = (
        446
        244)
      object grTask: TDBGrid
        Left = 8
        Top = 8
        Width = 430
        Height = 160
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsTask
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object gbDescription: TGroupBox
        Left = 8
        Top = 175
        Width = 430
        Height = 61
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Descripci'#243'n'
        TabOrder = 1
        DesignSize = (
          430
          61)
        object meDescription: TDBMemo
          Left = 8
          Top = 15
          Width = 414
          Height = 38
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          Color = clBtnFace
          DataField = 'DESCRIPTION'
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object btAdd: TButton
    Left = 133
    Top = 289
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Agregar ...'
    TabOrder = 1
  end
  object btChange: TButton
    Left = 217
    Top = 289
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cambiar ...'
    TabOrder = 2
  end
  object btDelete: TButton
    Left = 301
    Top = 289
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Borrar ...'
    TabOrder = 3
  end
  object btClose: TButton
    Left = 385
    Top = 289
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cerrar'
    TabOrder = 4
  end
  object dsTask: TDataSource
    AutoEdit = False
    DataSet = quTask
    Left = 208
    Top = 72
  end
  object quTask: TIBQuery
    Database = daMain.dbAgent
    Transaction = daMain.taMain
    BufferChunks = 1000
    CachedUpdates = False
    SQL.Strings = (
      'select * from qry_step where id_task = :id_task')
    Left = 272
    Top = 72
    ParamData = <
      item
        DataType = ftInterface
        Name = 'id_task'
        ParamType = ptInput
      end>
  end
end
