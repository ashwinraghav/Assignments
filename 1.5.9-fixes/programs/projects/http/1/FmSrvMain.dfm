object FormHttpServer: TFormHttpServer
  Left = 282
  Top = 239
  Width = 727
  Height = 404
  Caption = 'FormHttpServer'
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
    719
    377)
  PixelsPerInch = 96
  TextHeight = 13
  object lbClients: TListBox
    Left = 8
    Top = 8
    Width = 161
    Height = 361
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
end
