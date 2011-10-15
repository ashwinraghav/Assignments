object Form1: TForm1
  Left = 368
  Top = 330
  Width = 453
  Height = 273
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 160
    Top = 72
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Label2: TLabel
    Left = 160
    Top = 104
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object ListBox1: TListBox
    Left = 16
    Top = 12
    Width = 121
    Height = 225
    ItemHeight = 13
    TabOrder = 1
  end
  object Button1: TButton
    Left = 156
    Top = 20
    Width = 75
    Height = 25
    Caption = 'Test'
    TabOrder = 0
    OnClick = Button1Click
  end
  object CheckBox1: TCheckBox
    Left = 156
    Top = 148
    Width = 97
    Height = 17
    Caption = 'Locked'
    TabOrder = 2
  end
end
