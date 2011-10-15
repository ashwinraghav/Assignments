object FormTestOsNamedPipes: TFormTestOsNamedPipes
  Left = 148
  Top = 311
  Width = 852
  Height = 528
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
    844
    501)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 176
    Top = 72
    Width = 638
    Height = 402
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'Memo')
    TabOrder = 0
  end
  object Connect: TButton
    Left = 8
    Top = 32
    Width = 75
    Height = 25
    Caption = 'Connect'
    TabOrder = 1
    OnClick = ConnectClick
  end
  object Edit1: TEdit
    Left = 223
    Top = 26
    Width = 170
    Height = 21
    TabOrder = 2
    Text = '1'
  end
  object ListBox1: TListBox
    Left = 8
    Top = 72
    Width = 161
    Height = 401
    ItemHeight = 13
    TabOrder = 3
  end
  object edPC: TEdit
    Left = 88
    Top = 25
    Width = 121
    Height = 21
    TabOrder = 4
    Text = 'Nombre PC'
  end
  object Listen: TButton
    Left = 8
    Top = 0
    Width = 75
    Height = 25
    Caption = '&Listen'
    TabOrder = 5
    OnClick = ListenClick
  end
  object Write: TButton
    Left = 398
    Top = 23
    Width = 75
    Height = 25
    Caption = '&Write'
    TabOrder = 6
    OnClick = WriteClick
  end
end
