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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    862
    613)
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 24
    Top = 16
    Width = 75
    Height = 25
    Caption = 'one ping'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Edit1: TEdit
    Left = 104
    Top = 18
    Width = 109
    Height = 21
    TabOrder = 1
    Text = 'hcc2619'
  end
  object Display: TMemo
    Left = 36
    Top = 84
    Width = 769
    Height = 489
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Display')
    ScrollBars = ssVertical
    TabOrder = 2
  end
  object Button2: TButton
    Left = 24
    Top = 44
    Width = 73
    Height = 25
    Caption = 'timer'
    TabOrder = 3
    OnClick = Button2Click
  end
  object CheckBox1: TCheckBox
    Left = 376
    Top = 16
    Width = 137
    Height = 17
    Caption = 'resolver direccion'
    TabOrder = 4
  end
  object Button3: TButton
    Left = 104
    Top = 44
    Width = 75
    Height = 25
    Caption = 'simple'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 312
    Top = 16
  end
end
