object FormTest1: TFormTest1
  Left = 8
  Top = 9
  Width = 638
  Height = 425
  Caption = 'FormTest1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnSidebar: TPanel
    Left = 0
    Top = 0
    Width = 113
    Height = 398
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object edViewX: TFloatEdit
      Left = 19
      Top = 132
      Width = 77
      Height = 21
      Decimals = 2
      TabOrder = 0
    end
    object edViewY: TFloatEdit
      Left = 19
      Top = 156
      Width = 77
      Height = 21
      Decimals = 2
      TabOrder = 1
    end
    object edViewZ: TFloatEdit
      Left = 19
      Top = 179
      Width = 77
      Height = 21
      Decimals = 2
      TabOrder = 2
    end
    object Apply: TButton
      Left = 5
      Top = 204
      Width = 75
      Height = 25
      Caption = 'Apply'
      TabOrder = 3
      OnClick = ApplyClick
    end
    object edField: TFloatEdit
      Left = 4
      Top = 4
      Width = 105
      Height = 21
      Decimals = 2
      TabOrder = 4
      Value = 40.000000000000000000
    end
    object edZnear: TFloatEdit
      Left = 4
      Top = 28
      Width = 105
      Height = 21
      Decimals = 2
      TabOrder = 5
      Value = 1.000000000000000000
    end
    object edZfar: TFloatEdit
      Left = 4
      Top = 51
      Width = 105
      Height = 21
      Decimals = 2
      TabOrder = 6
      Value = 100.000000000000000000
    end
    object btDecX: TButton
      Left = 0
      Top = 132
      Width = 18
      Height = 23
      Caption = '<'
      TabOrder = 7
      OnClick = btDecXClick
    end
    object btDecY: TButton
      Left = 0
      Top = 155
      Width = 18
      Height = 23
      Caption = '<'
      TabOrder = 8
      OnClick = btDecYClick
    end
    object btDecZ: TButton
      Left = 0
      Top = 179
      Width = 18
      Height = 23
      Caption = '<'
      TabOrder = 9
      OnClick = btDecZClick
    end
    object btIncX: TButton
      Left = 97
      Top = 132
      Width = 16
      Height = 23
      Caption = '>'
      TabOrder = 10
      OnClick = btIncXClick
    end
    object btIncY: TButton
      Left = 97
      Top = 155
      Width = 16
      Height = 23
      Caption = '>'
      TabOrder = 11
      OnClick = btIncYClick
    end
    object btIncZ: TButton
      Left = 97
      Top = 179
      Width = 16
      Height = 23
      Caption = '>'
      TabOrder = 12
      OnClick = btIncZClick
    end
  end
  object Timer: TTimer
    Interval = 50
    OnTimer = TimerTimer
    Left = 752
    Top = 24
  end
end
