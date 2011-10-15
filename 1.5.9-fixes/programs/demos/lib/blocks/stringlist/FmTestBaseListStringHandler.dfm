object FormTestHandler: TFormTestHandler
  Left = 100
  Top = 180
  Width = 606
  Height = 305
  Caption = 'Tester of various Lists & Type Handlers (see Code ...)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object Listbox: TListBox
    Left = 0
    Top = 0
    Width = 309
    Height = 273
    ItemHeight = 13
    TabOrder = 0
  end
  object btStringList: TButton
    Left = 320
    Top = 0
    Width = 131
    Height = 28
    Hint = 'Demonstrates the use oif a StringList whose items are strings'
    Caption = 'StringList<string>'
    TabOrder = 1
    OnClick = btStringListClick
  end
  object btStringPtr: TButton
    Left = 472
    Top = 0
    Width = 103
    Height = 25
    Hint = 'Tests the functionality of the StringHandler (see Code ...)'
    Caption = 'StringHandler'
    TabOrder = 2
    OnClick = btStringPtrClick
  end
  object btVariantPtr: TButton
    Left = 472
    Top = 25
    Width = 103
    Height = 25
    Hint = 'Tests the functionality of the VariantHandler (see Code ...)'
    Caption = 'VariantHandler'
    TabOrder = 3
    OnClick = btVariantPtrClick
  end
  object btVariantList: TButton
    Left = 320
    Top = 30
    Width = 131
    Height = 28
    Hint = 'Demonstrates the use oif a StringList whose items are Variants'
    Caption = 'StringList<Variant>'
    TabOrder = 4
    OnClick = btVariantListClick
  end
  object btInterfaceList: TButton
    Left = 320
    Top = 61
    Width = 131
    Height = 28
    Hint = 'Demonstrates the use oif a StringList whose items are interfaces'
    Caption = 'StringList<IUnkown>'
    TabOrder = 5
    OnClick = btInterfaceListClick
  end
  object btInterfacePtr: TButton
    Left = 472
    Top = 50
    Width = 103
    Height = 25
    Hint = 'Tests the functionality of the InterfaceHandler (see Code ...)'
    Caption = 'InterfaceHandler'
    TabOrder = 6
    OnClick = btInterfacePtrClick
  end
  object btVoidPtr: TButton
    Left = 472
    Top = 75
    Width = 103
    Height = 25
    Hint = 'Tests the functionality of the PointerHandler (see Code ...)'
    Caption = 'VoidHandler'
    TabOrder = 7
    OnClick = btVoidPtrClick
  end
  object Button1: TButton
    Left = 320
    Top = 153
    Width = 131
    Height = 28
    Hint = 
      'Demonstrates the use oif a PointerList with a custom item Handle' +
      'r'
    Caption = 'PointerList<Custom>'
    TabOrder = 8
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 320
    Top = 183
    Width = 131
    Height = 28
    Hint = 
      'Demonstrates the use of a PointerList with an Object item Handle' +
      'r'
    Caption = 'PointerList<TObject>'
    TabOrder = 9
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 320
    Top = 214
    Width = 131
    Height = 28
    Hint = 
      'Demonstrates the use oif a PointerList with a String item Handle' +
      'r'
    Caption = 'PointerList<String>'
    TabOrder = 10
    OnClick = Button3Click
  end
  object Button4: TButton
    Left = 320
    Top = 245
    Width = 131
    Height = 28
    Hint = 
      'Demonstrates the use oif a PointerList with a IUnknown item Hand' +
      'ler'
    Caption = 'PointerList<IUnknown>'
    TabOrder = 11
    OnClick = Button4Click
  end
  object Button5: TButton
    Left = 320
    Top = 91
    Width = 131
    Height = 28
    Hint = 'Demonstrates the use oif a StringList whose items are Objects'
    Caption = 'StringList<TObject>'
    TabOrder = 12
    OnClick = Button5Click
  end
  object Button6: TButton
    Left = 320
    Top = 122
    Width = 131
    Height = 28
    Hint = 
      'Demonstrates the use oif a StringList whose items are plain poin' +
      'ters'
    Caption = 'StringList<Pointer>'
    TabOrder = 13
    OnClick = Button6Click
  end
end
