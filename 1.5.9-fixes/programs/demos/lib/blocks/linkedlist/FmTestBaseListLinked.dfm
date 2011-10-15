object FormTestLinked: TFormTestLinked
  Left = 434
  Top = 475
  Width = 432
  Height = 284
  Caption = 'Abstract Linked List Tester'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LinkedAdd: TButton
    Left = 8
    Top = 16
    Width = 109
    Height = 25
    Caption = '&Add'
    TabOrder = 0
    OnClick = LinkedAddClick
  end
  object Listbox: TListBox
    Left = 144
    Top = 16
    Width = 265
    Height = 209
    ItemHeight = 13
    TabOrder = 1
  end
  object LinkedEnum: TButton
    Left = 8
    Top = 41
    Width = 109
    Height = 25
    Caption = '&Enum'
    TabOrder = 2
    OnClick = LinkedEnumClick
  end
  object LinkedRemove: TButton
    Left = 8
    Top = 66
    Width = 109
    Height = 25
    Caption = '&Remove'
    TabOrder = 3
    OnClick = LinkedRemoveClick
  end
  object LinkedClear: TButton
    Left = 8
    Top = 91
    Width = 109
    Height = 25
    Caption = '&Clear'
    TabOrder = 4
    OnClick = LinkedClearClick
  end
end
