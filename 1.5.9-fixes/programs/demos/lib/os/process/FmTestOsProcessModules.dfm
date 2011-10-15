object FormTestOsProcessModules: TFormTestOsProcessModules
  Left = 137
  Top = 197
  Width = 614
  Height = 407
  ActiveControl = Mask
  Caption = 'FormTestOsProcessModules'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    606
    380)
  PixelsPerInch = 96
  TextHeight = 13
  object Show: TButton
    Left = 4
    Top = 4
    Width = 75
    Height = 25
    Caption = 'Show'
    Default = True
    TabOrder = 0
    OnClick = ShowClick
  end
  object Mask: TEdit
    Left = 81
    Top = 5
    Width = 121
    Height = 21
    TabOrder = 1
    Text = '*expl*'
  end
  object Processes: TListView
    Left = 5
    Top = 32
    Width = 212
    Height = 342
    Anchors = [akLeft, akTop, akBottom]
    Columns = <>
    ReadOnly = True
    RowSelect = True
    TabOrder = 2
    ViewStyle = vsReport
    OnSelectItem = ProcessesSelectItem
  end
  object Modules: TListView
    Left = 221
    Top = 33
    Width = 378
    Height = 342
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <>
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
  end
end
