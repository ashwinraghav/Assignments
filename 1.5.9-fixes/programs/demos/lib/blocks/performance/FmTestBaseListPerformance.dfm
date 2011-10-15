object FormTestPerformance: TFormTestPerformance
  Left = 100
  Top = 350
  Width = 584
  Height = 229
  Caption = 
    'Comparison of performance between Delphi'#39's TList and Sil.IList (' +
    'see Code)'
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
  object Label1: TLabel
    Left = 376
    Top = 101
    Width = 60
    Height = 13
    Caption = 'milisegundos'
  end
  object Label2: TLabel
    Left = 376
    Top = 37
    Width = 60
    Height = 13
    Caption = 'milisegundos'
  end
  object btDelphiListAdd: TButton
    Left = 3
    Top = 2
    Width = 122
    Height = 25
    Caption = 'Delphi.TList - Addition'
    TabOrder = 0
    OnClick = btDelphiListAddClick
  end
  object btSilListAdd: TButton
    Left = 127
    Top = 2
    Width = 122
    Height = 25
    Caption = 'Sil.IList - Addition'
    TabOrder = 1
    OnClick = btSilListAddClick
  end
  object btSilListEnum: TButton
    Left = 129
    Top = 66
    Width = 122
    Height = 25
    Caption = 'Sil.IList - Enum'
    TabOrder = 2
    OnClick = btSilListEnumClick
  end
  object edLapse1: TEdit
    Left = 3
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 3
  end
  object edLapse2: TEdit
    Left = 127
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 4
  end
  object edLapse4: TEdit
    Left = 129
    Top = 98
    Width = 121
    Height = 21
    TabOrder = 5
  end
  object btDelphiListEnum: TButton
    Left = 4
    Top = 66
    Width = 122
    Height = 25
    Caption = 'Delphi.TList - for'
    TabOrder = 6
    OnClick = btDelphiListEnumClick
  end
  object edLapse3: TEdit
    Left = 4
    Top = 98
    Width = 121
    Height = 21
    TabOrder = 7
  end
  object btSilContainerAdd: TButton
    Left = 250
    Top = 2
    Width = 122
    Height = 25
    Caption = 'Sil.Container - Addition'
    TabOrder = 8
    OnClick = btSilContainerAddClick
  end
  object btSilContainerEnum: TButton
    Left = 252
    Top = 66
    Width = 122
    Height = 25
    Caption = 'Sil.Container - Enum'
    TabOrder = 9
    OnClick = btSilContainerEnumClick
  end
  object edLapse5: TEdit
    Left = 250
    Top = 34
    Width = 121
    Height = 21
    TabOrder = 10
  end
  object edLapse6: TEdit
    Left = 252
    Top = 98
    Width = 121
    Height = 21
    TabOrder = 11
  end
end
