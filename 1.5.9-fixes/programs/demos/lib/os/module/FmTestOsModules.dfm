object FormTestModules: TFormTestModules
  Left = 217
  Top = 268
  Width = 576
  Height = 450
  Caption = 'FormTestModules'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lvList: TListView
    Left = 0
    Top = 0
    Width = 568
    Height = 382
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'Base'
      end
      item
        AutoSize = True
        Caption = 'Path'
      end
      item
        AutoSize = True
        Caption = 'Version'
      end
      item
        AutoSize = True
        Caption = 'Description'
      end
      item
        AutoSize = True
        Caption = 'Copyright'
      end>
    ReadOnly = True
    RowSelect = True
    ParentColor = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ShowModuleClick
    OnSelectItem = lvListSelectItem
  end
  object pnBottom: TPanel
    Left = 0
    Top = 382
    Width = 568
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btRefresh: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Refresh'
      TabOrder = 0
      OnClick = btRefreshClick
    end
    object edModuleName: TEdit
      Left = 175
      Top = 10
      Width = 130
      Height = 21
      TabOrder = 1
      Text = 'ntdll.dll'
    end
    object btLookup: TButton
      Left = 96
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Lookup'
      TabOrder = 2
      OnClick = ShowModuleClick
    end
  end
end
