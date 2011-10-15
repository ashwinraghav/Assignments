object FormTestToken: TFormTestToken
  Left = 287
  Top = 330
  Width = 468
  Height = 367
  Caption = 'Example of different string tokenization procedures'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 11
    Width = 63
    Height = 13
    Alignment = taRightJustify
    Caption = '&Separator: '
    FocusControl = edSeparator
  end
  object Label2: TLabel
    Left = 37
    Top = 37
    Width = 74
    Height = 13
    Alignment = taRightJustify
    Caption = 'String &Value: '
    FocusControl = edValue
  end
  object Label3: TLabel
    Left = 68
    Top = 66
    Width = 42
    Height = 13
    Alignment = taRightJustify
    Caption = '&Result: '
    FocusControl = Tokens
  end
  object edValue: TEdit
    Left = 111
    Top = 34
    Width = 329
    Height = 24
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    Text = 'A,.,aaaa,,cccc,'
  end
  object Tokens: TListBox
    Left = 111
    Top = 66
    Width = 145
    Height = 265
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ItemHeight = 16
    ParentFont = False
    TabOrder = 1
  end
  object DoEnumerate: TButton
    Left = 271
    Top = 66
    Width = 178
    Height = 31
    Caption = 'Use Sil.Str.&Enumerate!'
    TabOrder = 2
    OnClick = DoEnumerateClick
  end
  object edSeparator: TEdit
    Left = 111
    Top = 8
    Width = 121
    Height = 24
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    Text = ','
  end
  object DoTokenize: TButton
    Left = 271
    Top = 106
    Width = 178
    Height = 31
    Caption = 'Use Sil.Str.&Token!'
    TabOrder = 4
    OnClick = DoTokenizeClick
  end
  object DoTokenArray: TButton
    Left = 271
    Top = 146
    Width = 178
    Height = 31
    Caption = 'Use Sil.Str.Token&Array!'
    TabOrder = 5
    OnClick = DoTokenArrayClick
  end
  object DoStringList: TButton
    Left = 271
    Top = 186
    Width = 178
    Height = 31
    Caption = 'Use Sil.List.String&List'
    TabOrder = 6
    OnClick = DoStringListClick
  end
end
