object Form2: TForm2
  Left = 187
  Top = 165
  Width = 870
  Height = 640
  Caption = 'Form2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    860
    611)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 104
    Top = 18
    Width = 153
    Height = 21
    TabOrder = 1
    Text = 'HCC5060'
  end
  object Display: TMemo
    Left = 40
    Top = 72
    Width = 769
    Height = 489
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Display')
    TabOrder = 2
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 312
    Top = 16
  end
end
