object foStep: TfoStep
  Left = 335
  Top = 116
  BorderStyle = bsDialog
  Caption = 'Paso'
  ClientHeight = 310
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 4
    Top = 228
    Width = 92
    Height = 13
    Caption = 'Si termina con '#233'xito'
  end
  object Label3: TLabel
    Left = 4
    Top = 256
    Width = 91
    Height = 13
    Caption = 'Si termina con error'
  end
  object cbOnSuccess: TComboBox
    Left = 104
    Top = 224
    Width = 225
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      'Pasar al paso siguiente'
      'Terminar la tarea con '#233'xito'
      'Terminar la tarea con error')
  end
  object cbOnFailure: TComboBox
    Left = 104
    Top = 252
    Width = 225
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    Items.Strings = (
      'Pasar al paso siguiente'
      'Terminar la tarea con '#233'xito'
      'Terminar la tarea con error')
  end
  object gbAction: TGroupBox
    Left = 4
    Top = 0
    Width = 401
    Height = 217
    Caption = 'Acci'#243'n'
    TabOrder = 0
    object Label4: TLabel
      Left = 8
      Top = 20
      Width = 40
      Height = 13
      Caption = 'Nombre:'
    end
    object Label11: TLabel
      Left = 8
      Top = 44
      Width = 74
      Height = 13
      Caption = 'Tipo de acci'#243'n:'
    end
    object cbActionKind: TComboBox
      Left = 88
      Top = 40
      Width = 301
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'Ejecutar Comando SQL'
      OnClick = cbActionKindClick
      Items.Strings = (
        'Ejecutar Comando SQL'
        'Ejecutar Aplicacion')
    end
    object nbActionKind: TNotebook
      Left = 8
      Top = 68
      Width = 385
      Height = 141
      PageIndex = 1
      TabOrder = 2
      object TPage
        Left = 0
        Top = 0
        Caption = 'Sql'
        object Label7: TLabel
          Left = 0
          Top = 4
          Width = 47
          Height = 13
          Caption = 'Conexi'#243'n:'
        end
        object Label8: TLabel
          Left = 0
          Top = 32
          Width = 39
          Height = 13
          Caption = 'Usuario:'
        end
        object Label9: TLabel
          Left = 192
          Top = 32
          Width = 30
          Height = 13
          Caption = 'Clave:'
        end
        object Label10: TLabel
          Left = 0
          Top = 56
          Width = 48
          Height = 13
          Caption = 'Comando:'
        end
        object edSqlConnection: TEdit
          Left = 80
          Top = 0
          Width = 305
          Height = 21
          TabOrder = 0
        end
        object edUserName: TEdit
          Left = 80
          Top = 28
          Width = 105
          Height = 21
          MaxLength = 30
          TabOrder = 1
        end
        object edUserPassword: TEdit
          Left = 232
          Top = 28
          Width = 105
          Height = 21
          AutoSize = False
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Marlett'
          Font.Style = [fsBold]
          MaxLength = 30
          ParentFont = False
          PasswordChar = 'h'
          TabOrder = 2
        end
        object meSqlCommand: TMemo
          Left = 0
          Top = 72
          Width = 385
          Height = 69
          ScrollBars = ssVertical
          TabOrder = 3
          WantReturns = False
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'CommandLine'
        object Label5: TLabel
          Left = 0
          Top = 0
          Width = 48
          Height = 13
          Caption = 'Comando:'
        end
        object Label6: TLabel
          Left = 0
          Top = 120
          Width = 81
          Height = 13
          Caption = 'Codigo de salida:'
        end
        object meCommandLine: TMemo
          Left = 0
          Top = 16
          Width = 385
          Height = 93
          ScrollBars = ssVertical
          TabOrder = 0
          WantReturns = False
        end
        object edSuccessCode: TEdit
          Left = 92
          Top = 116
          Width = 121
          Height = 21
          TabOrder = 1
        end
      end
    end
    object edName: TEdit
      Left = 88
      Top = 12
      Width = 301
      Height = 21
      TabOrder = 0
    end
  end
  object btAccept: TButton
    Left = 244
    Top = 280
    Width = 77
    Height = 25
    Caption = '&Aceptar'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btAcceptClick
  end
  object btCancel: TButton
    Left = 328
    Top = 280
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 5
    OnClick = btCancelClick
  end
  object cbEnabled: TCheckBox
    Left = 336
    Top = 223
    Width = 69
    Height = 17
    Caption = 'Habilitada'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
