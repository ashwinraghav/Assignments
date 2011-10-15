object FormTestEvents: TFormTestEvents
  Left = 191
  Top = 243
  Width = 499
  Height = 331
  Caption = 'Demonstration of how to capture events from a IStringList'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Display: TMemo
    Left = 8
    Top = 8
    Width = 369
    Height = 289
    TabOrder = 0
  end
  object Button1: TButton
    Left = 380
    Top = 7
    Width = 106
    Height = 25
    Caption = 'ADD'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 380
    Top = 43
    Width = 106
    Height = 25
    Caption = 'DELETE'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 380
    Top = 79
    Width = 106
    Height = 25
    Caption = 'CLEAR'
    TabOrder = 3
    OnClick = Button3Click
  end
end
