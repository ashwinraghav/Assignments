object Form1: TForm1
  Left = 300
  Top = 446
  Width = 445
  Height = 188
  Caption = 'cliente'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'activar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 52
    Width = 75
    Height = 25
    Caption = 'desactivar'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 132
    Top = 16
    Width = 75
    Height = 25
    Caption = 'send'
    Default = True
    TabOrder = 2
    OnClick = Button3Click
  end
  object ListBox1: TListBox
    Left = 232
    Top = 12
    Width = 161
    Height = 117
    ItemHeight = 13
    TabOrder = 3
  end
end
