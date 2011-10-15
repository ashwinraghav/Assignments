object FormTestFileListEnumeration: TFormTestFileListEnumeration
  Left = 332
  Top = 273
  Width = 454
  Height = 296
  Caption = 'FormTestFileListEnumeration'
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
  object ListBox1: TListBox
    Left = 137
    Top = 0
    Width = 309
    Height = 269
    Align = alClient
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 137
    Height = 269
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 6
      Top = 8
      Width = 75
      Height = 25
      Caption = 'leer'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Edit1: TEdit
      Left = 6
      Top = 48
      Width = 121
      Height = 21
      TabOrder = 1
      Text = '%SystemRoot%\*.*'
    end
    object CheckBox1: TCheckBox
      Left = 10
      Top = 88
      Width = 97
      Height = 17
      Caption = 'recursivo'
      TabOrder = 2
    end
  end
end
