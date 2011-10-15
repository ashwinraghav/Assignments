object Form1: TForm1
  Left = 269
  Top = 454
  Width = 702
  Height = 269
  Caption = 'Form1'
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
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 11
    Height = 13
    Caption = 'url'
  end
  object Label2: TLabel
    Left = 124
    Top = 80
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Label3: TLabel
    Left = 124
    Top = 144
    Width = 32
    Height = 13
    Caption = 'Label3'
  end
  object Edit1: TEdit
    Left = 36
    Top = 12
    Width = 633
    Height = 21
    TabOrder = 0
  end
  object Button1: TButton
    Left = 36
    Top = 44
    Width = 75
    Height = 25
    Caption = 'read'
    Default = True
    TabOrder = 1
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 228
    Top = 44
    Width = 441
    Height = 185
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button2: TButton
    Left = 36
    Top = 76
    Width = 75
    Height = 25
    Caption = 'read arg'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 36
    Top = 108
    Width = 75
    Height = 25
    Caption = 'write arg'
    TabOrder = 4
    OnClick = Button3Click
  end
  object read: TButton
    Left = 36
    Top = 140
    Width = 75
    Height = 25
    Caption = 'level1/level2'
    TabOrder = 5
    OnClick = readClick
  end
end
