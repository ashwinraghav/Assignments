object FormTestTypeinfo: TFormTestTypeinfo
  Left = 55
  Top = 81
  Width = 634
  Height = 574
  ActiveControl = btDumpInterface
  Caption = 
    'TypeInfo demo: shows how to read an interface'#39's type info and ho' +
    'w to invoke a method by name'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  DesignSize = (
    626
    547)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 22
    Top = 48
    Width = 586
    Height = 475
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = [fsBold]
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object btDumpInterface: TButton
    Left = 24
    Top = 8
    Width = 99
    Height = 25
    Hint = 'Click this to see a dump of an interface from its type info'
    Caption = '&1 Dump Interface'
    TabOrder = 1
    OnClick = btDumpInterfaceClick
  end
  object Button2: TButton
    Left = 128
    Top = 8
    Width = 99
    Height = 25
    Hint = 
      'Click this to see how works the invocation of an interface metho' +
      'd by name.'
    Caption = '&2 Invoke Method'
    TabOrder = 2
    OnClick = Button2Click
  end
end
