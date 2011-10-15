object foConectar: TfoConectar
  Left = 383
  Top = 345
  BorderStyle = bsDialog
  Caption = 'Conectar...'
  ClientHeight = 72
  ClientWidth = 337
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
  object Label1: TLabel
    Left = 12
    Top = 16
    Width = 45
    Height = 13
    Caption = 'Direcci'#243'n'
  end
  object Label2: TLabel
    Left = 12
    Top = 44
    Width = 31
    Height = 13
    Caption = 'Puerto'
  end
  object edAddress: TEdit
    Left = 68
    Top = 12
    Width = 149
    Height = 21
    TabOrder = 0
  end
  object edPort: TEdit
    Left = 68
    Top = 40
    Width = 73
    Height = 21
    TabOrder = 1
  end
  object btOk: TButton
    Left = 252
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Aceptar'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object btCancel: TButton
    Left = 252
    Top = 40
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 3
  end
end
