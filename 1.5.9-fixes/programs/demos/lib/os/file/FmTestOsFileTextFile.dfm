object FormTestTextFile: TFormTestTextFile
  Left = 294
  Top = 259
  Width = 533
  Height = 315
  Caption = 'FormTestTextFile'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    525
    288)
  PixelsPerInch = 96
  TextHeight = 13
  object btReadMemo: TButton
    Left = 80
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'read'
    TabOrder = 0
    OnClick = btReadMemoClick
  end
  object btWriteMemo: TButton
    Left = 4
    Top = 256
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'write'
    TabOrder = 1
    OnClick = btWriteMemoClick
  end
  object edMemo: TMemo
    Left = 0
    Top = 0
    Width = 521
    Height = 249
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 2
  end
end
