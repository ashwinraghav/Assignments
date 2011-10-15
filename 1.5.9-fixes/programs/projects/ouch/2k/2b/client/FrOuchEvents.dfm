object FrameOuchEvents: TFrameOuchEvents
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  Color = 13625832
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  Visible = False
  object vtList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 443
    Height = 277
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.Background = 13165798
    Header.Font.Charset = ANSI_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Verdana'
    Header.Font.Style = []
    Header.Options = [hoColumnResize, hoShowSortGlyphs, hoVisible]
    Header.SortDirection = sdDescending
    Header.Style = hsXPStyle
    HintAnimation = hatNone
    HintMode = hmDefault
    IncrementalSearchDirection = sdForward
    ParentColor = True
    ParentFont = False
    ScrollBarOptions.ScrollBarStyle = sbm3D
    TabOrder = 0
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoHideButtons, toAutoDeleteMovedNodes]
    TreeOptions.PaintOptions = [toHideFocusRect, toHotTrack, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    OnCompareNodes = vtListCompareNodes
    OnFreeNode = vtListFreeNode
    OnGetText = vtListGetText
    OnHeaderClick = vtListHeaderClick
    OnInitNode = vtListInitNode
    Columns = <
      item
        Alignment = taRightJustify
        Color = 15395562
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 0
        Width = 70
        WideText = 'Hora'
      end
      item
        Color = 15659762
        Options = [coAllowClick, coDraggable, coEnabled, coParentBidiMode, coResizable, coShowDropMark, coVisible]
        Position = 1
        WideText = 'Operacion'
      end
      item
        Position = 2
        Width = 2000
        WideText = 'Mensaje'
      end>
  end
end
