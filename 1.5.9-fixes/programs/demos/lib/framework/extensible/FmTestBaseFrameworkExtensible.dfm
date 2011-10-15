object FormTestExtensible: TFormTestExtensible
  Left = 69
  Top = 146
  Width = 769
  Height = 394
  Caption = 
    'This is a demonstration of how is used an TSilExtensibleObject (' +
    'see Code for details!)'
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
  OnDestroy = FormDestroy
  DesignSize = (
    761
    367)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 176
    Top = 6
    Width = 580
    Height = 355
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object TestEnumerate: TButton
    Left = 0
    Top = 216
    Width = 75
    Height = 25
    Hint = 
      'Try to enumerate the main object. If it doesn'#39't have the IEnumer' +
      'able this will raise an EIntfCast exception, so don'#39't worry ... ' +
      'add IEnumerable and try again!!'
    Caption = '&Enumerate'
    TabOrder = 1
    OnClick = TestEnumerateClick
  end
  object AddEnumerable: TButton
    Left = 0
    Top = 8
    Width = 171
    Height = 25
    Hint = 'Extend the main object with the Enumerable capability ...'
    Caption = '&1 - Add: Enumerable'
    TabOrder = 2
    OnClick = AddEnumerableClick
  end
  object RemoveEnumerable: TButton
    Left = 0
    Top = 35
    Width = 171
    Height = 25
    Hint = 'Removes the IEnumerable capability ...'
    Caption = '&2 - Remove: Enumerable'
    TabOrder = 3
    OnClick = RemoveEnumerableClick
  end
  object AddConnectable: TButton
    Left = 0
    Top = 67
    Width = 171
    Height = 25
    Hint = 
      'Extends the main object to provide the connectable capability ..' +
      '.'
    Caption = '&3 - Add: Connectable'
    TabOrder = 4
    OnClick = AddConnectableClick
  end
  object RemoveConnectable: TButton
    Left = 0
    Top = 94
    Width = 171
    Height = 25
    Hint = 'Removes the connectable capability ...'
    Caption = '&4 - Remove: Connectable'
    TabOrder = 5
    OnClick = RemoveConnectableClick
  end
  object TestDisconnect: TButton
    Left = 76
    Top = 241
    Width = 75
    Height = 25
    Hint = 'Disconnect from it.'
    Caption = '&Disconnect'
    TabOrder = 6
    OnClick = TestDisconnectClick
  end
  object TestConnect: TButton
    Left = 0
    Top = 241
    Width = 75
    Height = 25
    Hint = 'Try to connect to the main object. '
    Caption = '&Connect'
    TabOrder = 7
    OnClick = TestConnectClick
  end
end
