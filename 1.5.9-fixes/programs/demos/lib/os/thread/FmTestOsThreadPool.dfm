object FormTestThreadPool: TFormTestThreadPool
  Left = 242
  Top = 275
  Width = 410
  Height = 142
  Caption = 'FormTestThreadPool'
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
  object Signal: TButton
    Left = 44
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Signal'
    TabOrder = 0
    OnClick = SignalClick
  end
  object Spawn: TButton
    Left = 148
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Spawn'
    TabOrder = 1
    OnClick = SpawnClick
  end
end
