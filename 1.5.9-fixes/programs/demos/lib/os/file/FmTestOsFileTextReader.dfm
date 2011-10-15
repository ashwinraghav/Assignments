object FormTestTextReader: TFormTestTextReader
  Left = 236
  Top = 269
  Width = 378
  Height = 334
  Anchors = [akLeft, akBottom]
  Caption = 'FormTestTextReader'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    370
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 369
    Height = 273
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object btReadText: TButton
    Left = 4
    Top = 276
    Width = 75
    Height = 25
    Caption = 'Read Text!'
    TabOrder = 1
    OnClick = btReadTextClick
  end
end
