object FormTestCloneMemory: TFormTestCloneMemory
  Left = 302
  Top = 256
  Width = 549
  Height = 296
  Caption = 'How to clone a object (see code for details)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btClone: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = '&Test It'
    TabOrder = 0
    OnClick = btCloneClick
  end
  object edWrite: TEdit
    Left = 96
    Top = 16
    Width = 126
    Height = 21
    TabOrder = 1
    Text = 'Hello world, I'#39'm a stream!'
  end
  object edRead: TEdit
    Left = 224
    Top = 16
    Width = 126
    Height = 21
    TabOrder = 2
  end
end
