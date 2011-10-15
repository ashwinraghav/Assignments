object FormHolors: TFormHolors
  Left = 312
  Top = 108
  Width = 870
  Height = 640
  Caption = 'FormHolors1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Generico: TButton
    Left = 18
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Generico'
    TabOrder = 0
  end
  object Vectores: TButton
    Left = 16
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Vectores'
    TabOrder = 1
    OnClick = VectoresClick
  end
end
