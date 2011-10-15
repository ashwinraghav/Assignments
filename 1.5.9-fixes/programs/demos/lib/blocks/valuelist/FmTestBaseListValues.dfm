object FormTestValuelist: TFormTestValuelist
  Left = 30
  Top = 171
  Width = 768
  Height = 359
  ActiveControl = edValueName
  Caption = 'IValueList demonstration program'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    760
    332)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 9
    Top = 6
    Width = 61
    Height = 13
    Caption = 'Value &Name:'
    FocusControl = edValueName
  end
  object Label2: TLabel
    Left = 113
    Top = 6
    Width = 57
    Height = 13
    Caption = 'Value &Type:'
    FocusControl = cbValueType
  end
  object Label3: TLabel
    Left = 262
    Top = 6
    Width = 53
    Height = 13
    Caption = 'Value &Size:'
    FocusControl = edValueSize
  end
  object Label4: TLabel
    Left = 382
    Top = 6
    Width = 30
    Height = 13
    Caption = '&Value:'
    FocusControl = edValue
  end
  object Display: TMemo
    Left = 4
    Top = 48
    Width = 753
    Height = 281
    Anchors = [akLeft, akTop, akRight, akBottom]
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 8
    OnDblClick = DisplayClear
  end
  object cbValueType: TComboBox
    Left = 112
    Top = 22
    Width = 145
    Height = 21
    Hint = 'Value type, only used at creation time'
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
  end
  object edValueName: TEdit
    Left = 8
    Top = 22
    Width = 94
    Height = 21
    Hint = 
      'This is the name to give to the value and to lookup the value in' +
      ' modification'
    TabOrder = 0
  end
  object edValueSize: TEdit
    Left = 260
    Top = 22
    Width = 121
    Height = 21
    Hint = 
      'Value size, only used if type if of string kind and required at ' +
      'creation time'
    TabOrder = 2
  end
  object btAddValue: TButton
    Left = 506
    Top = 19
    Width = 59
    Height = 25
    Hint = 
      'Adds a new value to the list taking the definition from the valu' +
      'es in the Edit boxes'
    Caption = '&Add'
    TabOrder = 4
    OnClick = btAddValueClick
  end
  object edValue: TEdit
    Left = 383
    Top = 21
    Width = 121
    Height = 21
    Hint = 'Text to assign to the value in creation and modification'
    TabOrder = 3
  end
  object btEnum: TButton
    Left = 695
    Top = 19
    Width = 59
    Height = 25
    Hint = 'Lists the values contained in the list'
    Caption = '&Enumerate'
    TabOrder = 7
    OnClick = btEnumClick
  end
  object btModifyValue: TButton
    Left = 565
    Top = 19
    Width = 59
    Height = 25
    Hint = 'Changes the value of the item indicated by its name.'
    Caption = '&Modify'
    TabOrder = 5
    OnClick = btModifyValueClick
  end
  object btDeleteValue: TButton
    Left = 624
    Top = 19
    Width = 59
    Height = 25
    Hint = 'Deletes the item indicated by its name.'
    Caption = '&Delete'
    TabOrder = 6
    OnClick = btDeleteValueClick
  end
end
