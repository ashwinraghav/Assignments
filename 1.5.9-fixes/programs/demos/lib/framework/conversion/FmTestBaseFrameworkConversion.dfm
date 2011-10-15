object FormTestConvert: TFormTestConvert
  Left = 144
  Top = 373
  Width = 1016
  Height = 410
  Caption = 'FormTestConvert'
  Color = clBtnFace
  Constraints.MinHeight = 122
  Constraints.MinWidth = 438
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    1008
    383)
  PixelsPerInch = 96
  TextHeight = 13
  object cbTarget: TComboBox
    Left = 280
    Top = 8
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 2
    OnExit = edValueExit
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
  object edValue: TEdit
    Left = 8
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 0
    OnExit = edValueExit
  end
  object btCanConvert: TButton
    Left = 8
    Top = 354
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&CanConvert?'
    TabOrder = 3
    OnClick = btCanConvertClick
  end
  object btConvertNow: TButton
    Left = 88
    Top = 354
    Width = 80
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Con&vert!'
    TabOrder = 4
    OnClick = btConvertNowClick
  end
  object cbOrigin: TComboBox
    Left = 134
    Top = 8
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 1
    OnExit = edValueExit
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
  object edResult: TMemo
    Left = 8
    Top = 34
    Width = 995
    Height = 317
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 5
    WordWrap = False
  end
  object btBuiidMap: TButton
    Left = 168
    Top = 355
    Width = 80
    Height = 24
    Anchors = [akLeft, akBottom]
    Caption = '&Map'
    TabOrder = 6
    OnClick = btBuiidMapClick
  end
  object sgMap: TStringGrid
    Left = 8
    Top = 34
    Width = 993
    Height = 319
    Anchors = [akLeft, akTop, akRight, akBottom]
    Color = clWhite
    DefaultColWidth = 60
    DefaultRowHeight = 16
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goThumbTracking]
    ParentShowHint = False
    ShowHint = True
    TabOrder = 7
    Visible = False
    OnMouseMove = sgMapMouseMove
  end
end
