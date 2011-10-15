object Form2: TForm2
  Left = 244
  Top = 319
  Width = 658
  Height = 308
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    650
    281)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 4
    Top = 36
    Width = 641
    Height = 241
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object Button1: TButton
    Left = 4
    Top = 4
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object edPassword: TEdit
    Left = 88
    Top = 4
    Width = 121
    Height = 21
    PasswordChar = '*'
    TabOrder = 2
    Text = 'panfleto'
  end
end
