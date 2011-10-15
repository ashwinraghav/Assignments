object Form1: TForm1
  Left = 297
  Top = 304
  Width = 601
  Height = 279
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    593
    252)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 4
    Top = 36
    Width = 581
    Height = 209
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssVertical
    TabOrder = 2
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
    TabOrder = 1
    Text = 'panfleto'
  end
end
