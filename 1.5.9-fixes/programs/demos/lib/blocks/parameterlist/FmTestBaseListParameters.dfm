object FormTestParameters: TFormTestParameters
  Left = 178
  Top = 243
  Width = 640
  Height = 308
  Caption = 'ParameterList Demo & Tester'
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
  object ParamAdd: TButton
    Left = 8
    Top = 8
    Width = 128
    Height = 25
    Caption = '&Define'
    TabOrder = 0
    OnClick = ParamDefineClick
  end
  object Enumerate: TButton
    Left = 8
    Top = 84
    Width = 128
    Height = 25
    Caption = '&Enumerate'
    TabOrder = 1
    OnClick = EnumerateClick
  end
  object Server: TButton
    Left = 8
    Top = 110
    Width = 128
    Height = 25
    Caption = 'Slice &Server'#39's Params'
    TabOrder = 2
    OnClick = SliceClick
  end
  object Local: TButton
    Left = 8
    Top = 136
    Width = 128
    Height = 25
    Caption = 'Slice &Local'#39's Params'
    TabOrder = 3
    OnClick = SliceClick
  end
  object Display: TMemo
    Left = 144
    Top = 8
    Width = 481
    Height = 265
    TabOrder = 4
  end
  object ParamMerge: TButton
    Left = 8
    Top = 188
    Width = 129
    Height = 25
    Caption = '&Merge'
    TabOrder = 5
    OnClick = ParamMergeClick
  end
  object ParamLookup: TButton
    Left = 8
    Top = 162
    Width = 129
    Height = 25
    Caption = 'Loo&kup'
    TabOrder = 6
    OnClick = ParamLookupClick
  end
  object ParamClear: TButton
    Left = 8
    Top = 58
    Width = 128
    Height = 25
    Caption = '&Clear'
    TabOrder = 7
    OnClick = ParamClearClick
  end
  object ParamCreate: TButton
    Left = 8
    Top = 33
    Width = 128
    Height = 25
    Caption = 'C&reate'
    TabOrder = 8
    OnClick = ParamCreateClick
  end
end
