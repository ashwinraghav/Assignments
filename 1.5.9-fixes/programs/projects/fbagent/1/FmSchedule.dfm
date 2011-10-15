object foSchedule: TfoSchedule
  Left = 345
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Editar ejecuci'#243'n'
  ClientHeight = 294
  ClientWidth = 326
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object gbExecKind: TGroupBox
    Left = 4
    Top = 72
    Width = 317
    Height = 77
    Caption = 'Tipo de ejecuci'#243'n'
    TabOrder = 1
    object Label1: TLabel
      Left = 208
      Top = 20
      Width = 44
      Height = 13
      Caption = 'a la hora:'
    end
    object rbExecKindDay: TRadioButton
      Left = 8
      Top = 20
      Width = 105
      Height = 17
      Caption = 'se ejecuta el d'#237'a:'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object meExecTime: TMaskEdit
      Left = 256
      Top = 16
      Width = 53
      Height = 21
      EditMask = '!90:00:00;1;_'
      MaxLength = 8
      TabOrder = 1
      Text = '  :  :  '
    end
    object dtExecDay: TDateTimePicker
      Left = 112
      Top = 16
      Width = 89
      Height = 21
      Date = 37645.537111064800000000
      Time = 37645.537111064800000000
      TabOrder = 2
    end
    object rbExecKindRec: TRadioButton
      Left = 8
      Top = 48
      Width = 121
      Height = 17
      Caption = 'ejecuci'#243'n recurrente'
      TabOrder = 3
    end
    object btRecChange: TButton
      Left = 132
      Top = 44
      Width = 75
      Height = 25
      Caption = 'cambiar...'
      TabOrder = 4
      OnClick = btRecChangeClick
    end
  end
  object gbExec: TGroupBox
    Left = 4
    Top = 0
    Width = 317
    Height = 69
    Caption = 'Ejecuci'#243'n'
    TabOrder = 0
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 40
      Height = 13
      Caption = 'Nombre:'
    end
    object edName: TEdit
      Left = 56
      Top = 16
      Width = 253
      Height = 21
      TabOrder = 0
    end
    object cbEnabled: TCheckBox
      Left = 56
      Top = 44
      Width = 97
      Height = 17
      Caption = 'Habilitada'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
  object btAccept: TButton
    Left = 160
    Top = 264
    Width = 77
    Height = 25
    Caption = '&Aceptar'
    Default = True
    ModalResult = 1
    TabOrder = 2
    OnClick = btAcceptClick
  end
  object btCancel: TButton
    Left = 244
    Top = 264
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object rbExecDel: TRadioGroup
    Left = 4
    Top = 152
    Width = 317
    Height = 105
    Caption = 'Borrar la ejecuci'#243'n automaticamente'
    ItemIndex = 0
    Items.Strings = (
      'No borrar nunca'
      'Borrar s'#243'lo si termina bien'
      'Borrar s'#243'lo si termina mal'
      'Borrar cuando no se va a ejecutar mas')
    TabOrder = 3
  end
  object trRecurrence: TIBTransaction
    DefaultDatabase = daMain.dbAgent
    Left = 244
    Top = 200
  end
  object qryRecurrence: TIBQuery
    Database = daMain.dbAgent
    Transaction = trRecurrence
    SQL.Strings = (
      'select * from QRY_RECURRENCE(:SCHEDULE_ID)')
    Left = 244
    Top = 164
    ParamData = <
      item
        DataType = ftUnknown
        Name = 'SCHEDULE_ID'
        ParamType = ptUnknown
      end>
  end
end
