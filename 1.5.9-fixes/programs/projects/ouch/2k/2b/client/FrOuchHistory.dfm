object FrameOuchHistory: TFrameOuchHistory
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
  object vtList: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 443
    Height = 277
    Align = alClient
    BorderStyle = bsNone
    Header.AutoSizeIndex = -1
    Header.Font.Charset = ANSI_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Verdana'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag, hoHotTrack, hoShowHint, hoShowSortGlyphs, hoVisible]
    Header.Style = hsXPStyle
    HintAnimation = hatNone
    HintMode = hmDefault
    IncrementalSearchDirection = sdForward
    ParentColor = True
    ScrollBarOptions.AlwaysVisible = True
    ScrollBarOptions.ScrollBarStyle = sbm3D
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoScrollOnExpand, toAutoSort, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toHideFocusRect, toShowDropmark, toShowHorzGridLines, toShowVertGridLines, toThemeAware, toUseBlendedImages]
    TreeOptions.SelectionOptions = [toExtendedFocus, toFullRowSelect, toLevelSelectConstraint, toMultiSelect]
    OnFreeNode = vtListFreeNode
    OnGetText = vtListGetText
    OnInitNode = vtListInitNode
    Columns = <>
  end
end
