object FormTestAggregation: TFormTestAggregation
  Left = 37
  Top = 412
  Width = 992
  Height = 296
  Caption = 'Shows how the aggregation object works'
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
    984
    269)
  PixelsPerInch = 96
  TextHeight = 13
  object Lock: TButton
    Left = 0
    Top = 216
    Width = 75
    Height = 25
    Caption = '&Lock'
    TabOrder = 0
    OnClick = LockClick
  end
  object Memo: TMemo
    Left = 176
    Top = 6
    Width = 805
    Height = 258
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object AddLockable: TButton
    Left = 0
    Top = 8
    Width = 171
    Height = 25
    Caption = '&1 - Add: Lockable'
    TabOrder = 2
    OnClick = AddLockableClick
  end
  object RemoveLockable: TButton
    Left = 0
    Top = 35
    Width = 171
    Height = 25
    Caption = '&2 - Remove: Lockable'
    TabOrder = 3
    OnClick = RemoveLockableClick
  end
  object AddConnectable: TButton
    Left = 0
    Top = 70
    Width = 171
    Height = 25
    Caption = '&3 - Add: Connectable'
    TabOrder = 4
    OnClick = AddConnectableClick
  end
  object RemoveConnectable: TButton
    Left = 0
    Top = 97
    Width = 171
    Height = 25
    Caption = '&4 - Remove: Connectable'
    TabOrder = 5
    OnClick = RemoveConnectableClick
  end
  object Connect: TButton
    Left = 0
    Top = 241
    Width = 75
    Height = 25
    Caption = '&Connect'
    TabOrder = 6
    OnClick = ConnectClick
  end
  object Disconnect: TButton
    Left = 76
    Top = 241
    Width = 75
    Height = 25
    Caption = '&Disconnect'
    TabOrder = 7
    OnClick = DisconnectClick
  end
end
