object FormTestOsProcessList: TFormTestOsProcessList
  Left = 135
  Top = 361
  Width = 576
  Height = 407
  ActiveControl = edProcessName
  Caption = 'Process List Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 41
    Width = 568
    Height = 339
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btRefresh: TButton
      Left = 12
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Refresh'
      Default = True
      TabOrder = 0
      OnClick = btRefreshClick
    end
    object edProcessName: TEdit
      Left = 112
      Top = 9
      Width = 142
      Height = 21
      TabOrder = 1
      Text = '*explo*'
    end
  end
end
