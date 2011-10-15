object FormTestLibraryClient: TFormTestLibraryClient
  Left = 214
  Top = 321
  Width = 553
  Height = 255
  Caption = 'FormTestLibraryClient'
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
    545
    228)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 20
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Test!'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 112
    Top = 20
    Width = 405
    Height = 193
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
end
