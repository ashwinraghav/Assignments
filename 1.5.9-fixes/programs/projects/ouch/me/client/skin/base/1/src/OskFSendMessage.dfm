object SendMessageForm: TSendMessageForm
  Left = 271
  Top = 177
  Width = 593
  Height = 414
  Caption = 'Ouch! - Enviar Mensaje...'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object spHistory: TSplitter
    Left = 0
    Top = 134
    Width = 585
    Height = 5
    Cursor = crVSplit
    Align = alTop
    Color = clBtnText
    ParentColor = False
  end
  object pnHeader: TPanel
    Left = 0
    Top = 0
    Width = 585
    Height = 25
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 6
      Top = 7
      Width = 39
      Height = 13
      Caption = 'Destino:'
    end
    object btDialogo: TSpeedButton
      Left = 531
      Top = 4
      Width = 48
      Height = 19
      AllowAllUp = True
      GroupIndex = 1
      Down = True
      Caption = 'Diálogo'
      OnClick = btDialogoClick
    end
    object ComboBox1: TComboBox
      Left = 55
      Top = 3
      Width = 214
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object pnHistory: TPanel
    Left = 0
    Top = 25
    Width = 585
    Height = 109
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object Memo2: TMemo
      Left = 4
      Top = 4
      Width = 577
      Height = 101
      TabStop = False
      Align = alClient
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnMessage: TPanel
    Left = 0
    Top = 139
    Width = 585
    Height = 220
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 2
    object meMessage: TMemo
      Left = 4
      Top = 4
      Width = 577
      Height = 212
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
  object pnFooter: TPanel
    Left = 0
    Top = 359
    Width = 585
    Height = 28
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    object btSend: TButton
      Left = 4
      Top = 1
      Width = 75
      Height = 25
      Caption = '&Enviar'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object btClear: TButton
      Left = 84
      Top = 1
      Width = 75
      Height = 25
      Caption = '&Borrar'
      TabOrder = 1
      OnClick = btClearClick
    end
    object btCancel: TButton
      Left = 164
      Top = 1
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancelar'
      ModalResult = 2
      TabOrder = 2
    end
  end
end
