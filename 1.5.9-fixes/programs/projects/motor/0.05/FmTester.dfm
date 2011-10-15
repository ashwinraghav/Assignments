object FormMotor: TFormMotor
  Left = 97
  Top = 285
  Width = 803
  Height = 571
  Caption = 'FormMotor'
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
  DesignSize = (
    795
    544)
  PixelsPerInch = 96
  TextHeight = 13
  object Painter: TPaintBox
    Left = 328
    Top = 48
    Width = 451
    Height = 478
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentColor = False
    ParentFont = False
    OnPaint = PainterPaint
  end
  object Reset: TButton
    Left = 16
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 0
    OnClick = ResetClick
  end
  object Run: TButton
    Left = 120
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Run'
    TabOrder = 1
    OnClick = RunClick
  end
  object lvGlobals: TListView
    Left = 16
    Top = 43
    Width = 305
    Height = 483
    Anchors = [akLeft, akTop, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Tag'
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    ReadOnly = True
    RowSelect = True
    ParentColor = True
    TabOrder = 2
    ViewStyle = vsReport
  end
  object Step: TButton
    Left = 208
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Step'
    TabOrder = 3
    OnClick = StepClick
  end
  object Stepper: TTimer
    Enabled = False
    Interval = 10
    OnTimer = StepClick
    Left = 312
  end
  object Update: TTimer
    Enabled = False
    Interval = 50
    OnTimer = UpdateTimer
    Left = 368
  end
end
