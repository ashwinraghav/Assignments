object foMain: TfoMain
  Left = 349
  Top = 304
  Width = 478
  Height = 313
  ActiveControl = grTask
  BorderIcons = [biSystemMenu]
  Caption = 'FbAgent'
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
  OnCreate = FormCreate
  DesignSize = (
    470
    286)
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 4
    Top = 4
    Width = 460
    Height = 244
    ActivePage = Tareas
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object Tareas: TTabSheet
      Caption = 'Tareas'
      DesignSize = (
        452
        216)
      object grTask: TDBGrid
        Left = 4
        Top = 4
        Width = 445
        Height = 136
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsTask
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        OnDblClick = btChangeClick
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
          end
          item
            Expanded = False
            FieldName = 'CATEGORY_NAME'
            Title.Caption = 'Categoria'
            Visible = True
          end>
      end
      object gbDescription: TGroupBox
        Left = 4
        Top = 144
        Width = 445
        Height = 69
        Anchors = [akLeft, akRight, akBottom]
        Caption = 'Descripci'#243'n'
        TabOrder = 1
        DesignSize = (
          445
          69)
        object meDescription: TDBMemo
          Left = 8
          Top = 15
          Width = 429
          Height = 46
          TabStop = False
          Anchors = [akLeft, akTop, akRight, akBottom]
          BorderStyle = bsNone
          Color = clBtnFace
          DataField = 'DESCRIPTION'
          DataSource = dsTask
          ScrollBars = ssVertical
          TabOrder = 0
        end
      end
    end
  end
  object btAdd: TButton
    Left = 135
    Top = 255
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Agregar ...'
    TabOrder = 1
    OnClick = btAddClick
  end
  object btChange: TButton
    Left = 219
    Top = 255
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Cambiar ...'
    TabOrder = 2
    OnClick = btChangeClick
  end
  object btDelete: TButton
    Left = 303
    Top = 255
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Borrar ...'
    TabOrder = 3
    OnClick = btDeleteClick
  end
  object btClose: TButton
    Left = 387
    Top = 255
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Cerrar'
    TabOrder = 4
    OnClick = btCloseClick
  end
  object dsTask: TDataSource
    AutoEdit = False
    DataSet = quTask
    Left = 208
    Top = 72
  end
  object quTask: TIBQuery
    Database = daMain.dbAgent
    Transaction = taTask
    SQL.Strings = (
      'select * from qry_task_list')
    Left = 256
    Top = 72
  end
  object taTask: TIBTransaction
    DefaultDatabase = daMain.dbAgent
    Left = 304
    Top = 72
  end
end
