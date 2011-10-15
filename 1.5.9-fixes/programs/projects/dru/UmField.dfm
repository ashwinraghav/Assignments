object foField: TfoField
  Left = 410
  Top = 443
  BorderStyle = bsDialog
  Caption = 'Create field'
  ClientHeight = 125
  ClientWidth = 178
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 12
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object Label2: TLabel
    Left = 12
    Top = 36
    Width = 28
    Height = 13
    Caption = 'Type:'
  end
  object Label3: TLabel
    Left = 12
    Top = 60
    Width = 23
    Height = 13
    Caption = 'Size:'
  end
  object edName: TEdit
    Left = 52
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object cbType: TComboBox
    Left = 52
    Top = 32
    Width = 121
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnClick = cbTypeClick
    Items.Strings = (
      'String'
      'Integer'
      'LongWord'
      'Byte'
      'Word'
      'LongWord'
      'Boolean'
      'Float'
      'Currency'
      'DateTime'
      'Memo'
      'Guid'
      ' ')
  end
  object edSize: TEdit
    Left = 52
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 2
  end
  object btOk: TButtonEx
    Left = 52
    Top = 88
    Width = 57
    Height = 29
    AutoHighlight = True
    Spacing = 4
    Centered = True
    AutoRepeat = False
    TextVertical = False
    Caption = 'Ok'
    Default = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 1
    ParentFont = False
    TabOrder = 3
  end
  object btCancel: TButtonEx
    Left = 116
    Top = 88
    Width = 57
    Height = 29
    AutoHighlight = True
    Spacing = 4
    Centered = True
    AutoRepeat = False
    Cancel = True
    TextVertical = False
    Caption = 'Cancel'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ModalResult = 2
    ParentFont = False
    TabOrder = 4
  end
end
