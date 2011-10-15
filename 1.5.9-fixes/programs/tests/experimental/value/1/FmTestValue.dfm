object FormTestValueMain: TFormTestValueMain
  Left = 90
  Top = 205
  Width = 706
  Height = 302
  Caption = 'FormTestValueMain'
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
    698
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object btSmallint: TButton
    Left = 80
    Top = 34
    Width = 75
    Height = 25
    Caption = 'Smallint'
    TabOrder = 0
    OnClick = btSmallintClick
  end
  object btAnsiString: TButton
    Left = 80
    Top = 64
    Width = 75
    Height = 25
    Caption = 'AnsiString'
    TabOrder = 1
    OnClick = btAnsiStringClick
  end
  object edSmallint: TEdit
    Left = 160
    Top = 36
    Width = 534
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
  end
  object edAnsiString: TEdit
    Left = 159
    Top = 66
    Width = 534
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 3
  end
  object btCreate: TButton
    Left = 2
    Top = 34
    Width = 75
    Height = 25
    Caption = '&Create'
    TabOrder = 4
    OnClick = btCreateClick
  end
  object btDestroy: TButton
    Left = 2
    Top = 64
    Width = 75
    Height = 25
    Caption = '&Destroy'
    TabOrder = 5
    OnClick = btDestroyClick
  end
  object edValue: TEdit
    Left = 163
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 6
    Text = '1234'
  end
  object cbOrigin: TComboBox
    Left = 11
    Top = 5
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 7
    Items.Strings = (
      'Empty'
      'Null'
      'Smallint'
      'LongInt'
      'Single'
      'Double'
      'Currency'
      'Date'
      'WideString'
      'Dispatch'
      'Error'
      'WordBool'
      'Variant'
      'Interface'
      'Decimal'
      'Extended'
      'ShortInt'
      'Byte'
      'Word'
      'LongWord'
      'LargeInt'
      'LargeWord'
      'Integer'
      'Cardinal'
      'Void'
      'HRESULT'
      'Pointer'
      'Safearray'
      'Dynarray'
      'Userdefined'
      'PAnsiChar'
      'PWideChar'
      'GUID'
      'Class'
      'Object'
      'Boolean'
      'LongBool'
      'AnsiChar'
      'WideChar'
      'AnsiString')
  end
  object btApply: TButton
    Left = 283
    Top = 5
    Width = 75
    Height = 25
    Caption = 'Apply'
    TabOrder = 8
    OnClick = btApplyClick
  end
  object btMemory: TButton
    Left = 618
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Memory'
    TabOrder = 9
    OnClick = btMemoryClick
  end
end
