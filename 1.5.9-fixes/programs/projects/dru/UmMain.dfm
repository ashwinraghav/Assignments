object foMain: TfoMain
  Left = 258
  Top = 337
  Width = 788
  Height = 396
  Caption = 'DataRowset Utility'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 83
    Height = 369
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object btOpen: TButtonEx
      Left = 4
      Top = 4
      Width = 73
      Height = 41
      AutoHighlight = True
      Spacing = 4
      Centered = True
      AutoRepeat = False
      TextVertical = False
      Caption = 'Open Table ...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = btOpenClick
    end
    object brExit: TButtonEx
      Left = 4
      Top = 224
      Width = 73
      Height = 41
      AutoHighlight = True
      Spacing = 4
      Centered = True
      AutoRepeat = False
      TextVertical = False
      Caption = 'Exit'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 5
      OnClick = brExitClick
    end
    object btCreateTable: TButtonEx
      Left = 4
      Top = 48
      Width = 73
      Height = 41
      AutoHighlight = True
      Spacing = 4
      Centered = True
      AutoRepeat = False
      TextVertical = False
      Caption = 'Create table ...'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 1
      OnClick = btCreateTableClick
    end
    object btSaveAs: TButtonEx
      Left = 4
      Top = 136
      Width = 73
      Height = 41
      AutoHighlight = True
      Spacing = 4
      Centered = True
      AutoRepeat = False
      TextVertical = False
      Caption = 'Save table as ...'
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      OnClick = btSaveAsClick
    end
    object btClose: TButtonEx
      Left = 4
      Top = 180
      Width = 73
      Height = 41
      AutoHighlight = True
      Spacing = 4
      Centered = True
      AutoRepeat = False
      TextVertical = False
      Caption = 'Close table'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 4
      OnClick = btCloseClick
    end
    object btSaveTable: TButtonEx
      Left = 4
      Top = 92
      Width = 73
      Height = 41
      AutoHighlight = True
      Spacing = 4
      Centered = True
      AutoRepeat = False
      TextVertical = False
      Caption = 'Save table'
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 2
      OnClick = btSaveTableClick
    end
  end
  object Panel4: TPanel
    Left = 83
    Top = 0
    Width = 697
    Height = 369
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 242
      Width = 697
      Height = 6
      Cursor = crVSplit
      Align = alBottom
      Beveled = True
    end
    object Panel7: TPanel
      Left = 0
      Top = 0
      Width = 697
      Height = 49
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object btCreateField: TButtonEx
        Left = 4
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Create field ...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = btCreateFieldClick
      end
      object btDeleteField: TButtonEx
        Left = 152
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Delete field ...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = btDeleteFieldClick
      end
      object btFieldUp: TButtonEx
        Left = 228
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Move up'
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 3
      end
      object btFieldDown: TButtonEx
        Left = 304
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Move down'
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
      end
      object btChangeField: TButtonEx
        Left = 76
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Change field ...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = btChangeFieldClick
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 322
      Width = 697
      Height = 47
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 1
      object btCreateIndex: TButtonEx
        Left = 0
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Create index ...'
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 0
        OnClick = btCreateIndexClick
      end
      object btDeleteIndex: TButtonEx
        Left = 152
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Delete index ...'
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 1
        OnClick = btDeleteIndexClick
      end
      object btChangeIndex: TButtonEx
        Left = 76
        Top = 4
        Width = 73
        Height = 41
        AutoHighlight = True
        Spacing = 4
        Centered = True
        AutoRepeat = False
        TextVertical = False
        Caption = 'Change index ...'
        Enabled = False
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 2
        OnClick = btChangeIndexClick
      end
    end
    object sgFields: TStringGrid
      Left = 0
      Top = 49
      Width = 697
      Height = 193
      Align = alClient
      ColCount = 3
      DefaultRowHeight = 18
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goThumbTracking]
      TabOrder = 2
      OnDblClick = btChangeFieldClick
      ColWidths = (
        128
        130
        64)
    end
    object sgIndexes: TStringGrid
      Left = 0
      Top = 248
      Width = 697
      Height = 74
      Align = alBottom
      ColCount = 3
      DefaultRowHeight = 18
      FixedCols = 0
      RowCount = 2
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goDrawFocusSelected, goThumbTracking]
      TabOrder = 3
      OnDblClick = btChangeIndexClick
      ColWidths = (
        128
        194
        64)
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'd'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 237
    Top = 165
  end
  object SaveDialog: TSaveDialog
    DefaultExt = 'd'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 301
    Top = 165
  end
end
