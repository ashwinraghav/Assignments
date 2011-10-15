object FormTestQueues: TFormTestQueues
  Left = 348
  Top = 397
  Width = 591
  Height = 306
  Caption = 'FormTestQueues'
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
    583
    279)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 126
    Top = 7
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object ListBox1: TListBox
    Left = 124
    Top = 24
    Width = 329
    Height = 225
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    TabOrder = 0
  end
  object btAdd: TButton
    Left = 16
    Top = 97
    Width = 75
    Height = 25
    Caption = 'Add'
    TabOrder = 1
    OnClick = btAddInterfacesClick
  end
  object btReset: TButton
    Left = 16
    Top = 68
    Width = 75
    Height = 25
    Caption = 'Reset'
    TabOrder = 2
    OnClick = btResetInterfacesClick
  end
  object btAddLoop: TButton
    Left = 16
    Top = 128
    Width = 75
    Height = 25
    Caption = 'Add Loop'
    TabOrder = 3
    OnClick = btAddLoopClick
  end
  object btStartStop: TButton
    Left = 16
    Top = 158
    Width = 75
    Height = 25
    Caption = 'Stop'
    TabOrder = 4
    OnClick = btStartStopClick
  end
  object ckInterfaces: TRadioButton
    Left = 16
    Top = 8
    Width = 77
    Height = 17
    Caption = 'Interfaces'
    TabOrder = 5
    OnClick = ckModoChanged
  end
  object ckPointers: TRadioButton
    Left = 16
    Top = 28
    Width = 77
    Height = 17
    Caption = 'Pointers'
    TabOrder = 6
    OnClick = ckModoChanged
  end
  object tmRefreshMemory: TTimer
    OnTimer = tmRefreshMemoryTimer
    Left = 512
    Top = 72
  end
end
