object FormTestBase: TFormTestBase
  Left = 218
  Top = 265
  Width = 361
  Height = 182
  ActiveControl = edCount
  Caption = 'Test the base implementation of the containers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Vector: TButton
    Left = 0
    Top = 0
    Width = 75
    Height = 25
    Hint = 'Creates a vector<Double>'
    Caption = '&Vector'
    TabOrder = 0
    OnClick = VectorClick
  end
  object List: TButton
    Left = 0
    Top = 24
    Width = 75
    Height = 25
    Hint = 'Creates a list<Double>'
    Caption = '&List'
    TabOrder = 1
    OnClick = ListClick
  end
  object Add: TButton
    Left = 0
    Top = 78
    Width = 75
    Height = 25
    Hint = 'Adds <N> doubles to the container'
    Caption = '&Add'
    TabOrder = 2
    OnClick = AddClick
  end
  object edCount: TEdit
    Left = 80
    Top = 0
    Width = 121
    Height = 21
    Hint = 'Number of elements to insert'
    TabOrder = 3
    Text = '1000000'
    OnExit = edCountExit
  end
  object edAdd: TEdit
    Left = 75
    Top = 80
    Width = 121
    Height = 21
    Hint = 'Duration of the operation expressed in [seconds]'
    ReadOnly = True
    TabOrder = 4
  end
  object Clear: TButton
    Left = 0
    Top = 103
    Width = 75
    Height = 25
    Hint = 'Clears the container'
    Caption = '&Clear'
    TabOrder = 5
    OnClick = ClearClick
  end
  object edClear: TEdit
    Left = 75
    Top = 105
    Width = 121
    Height = 21
    Hint = 'Duration of the operation expressed in [seconds]'
    ReadOnly = True
    TabOrder = 6
  end
  object Enum: TButton
    Left = 0
    Top = 128
    Width = 75
    Height = 25
    Hint = 'Enumerates the container'
    Caption = '&Enum'
    TabOrder = 7
    OnClick = EnumClick
  end
  object edEnum: TEdit
    Left = 75
    Top = 130
    Width = 121
    Height = 21
    Hint = 'Duration of the operation expressed in [seconds]'
    ReadOnly = True
    TabOrder = 8
  end
end
