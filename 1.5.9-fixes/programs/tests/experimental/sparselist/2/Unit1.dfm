object Form1: TForm1
  Left = 192
  Top = 103
  Width = 870
  Height = 640
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    862
    613)
  PixelsPerInch = 96
  TextHeight = 13
  object Edit1: TEdit
    Left = 8
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 0
  end
  object Add: TButton
    Left = 131
    Top = 3
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = AddClick
  end
  object Memo: TMemo
    Left = 8
    Top = 32
    Width = 847
    Height = 577
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
  object Enum: TButton
    Left = 209
    Top = 3
    Width = 75
    Height = 25
    Caption = 'Enum'
    TabOrder = 3
    OnClick = EnumClick
  end
  object Next: TButton
    Left = 288
    Top = 2
    Width = 75
    Height = 25
    Caption = 'Next'
    TabOrder = 4
    OnClick = NextClick
  end
end
