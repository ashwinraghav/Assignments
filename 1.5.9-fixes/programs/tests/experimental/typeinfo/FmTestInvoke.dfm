object FormTestInvoke: TFormTestInvoke
  Left = 252
  Top = 457
  Width = 513
  Height = 284
  ActiveControl = cbProperties
  Caption = 'Test'
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
  object stParams: TLabel
    Left = 80
    Top = 140
    Width = 3
    Height = 13
  end
  object Test: TButton
    Left = 0
    Top = 40
    Width = 75
    Height = 25
    Caption = 'Test'
    Default = True
    TabOrder = 0
    OnClick = TestClick
  end
  object cbProperties: TComboBox
    Left = 77
    Top = 41
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
  end
  object edValue: TEdit
    Left = 0
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '123456.789'
  end
  object edResult: TEdit
    Left = 0
    Top = 72
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object Invoke: TButton
    Left = 1
    Top = 112
    Width = 75
    Height = 25
    Caption = 'Invo&ke'
    TabOrder = 4
    OnClick = InvokeClick
  end
  object cbMethods: TComboBox
    Left = 78
    Top = 113
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 5
    OnSelect = cbMethodsSelect
  end
  object edArguments: TEdit
    Left = 78
    Top = 163
    Width = 419
    Height = 21
    TabOrder = 6
  end
  object Button1: TButton
    Left = 360
    Top = 32
    Width = 75
    Height = 17
    Caption = 'Button1'
    TabOrder = 7
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 208
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 8
  end
end
