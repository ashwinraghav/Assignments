object FrameTree: TFrameTree
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  object vwTree: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 443
    Height = 277
    Align = alClient
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    BorderWidth = 2
    DragMode = dmAutomatic
    DragOperations = [doCopy, doMove, doLink]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'MS Sans Serif'
    Header.Font.Style = []
    Header.MainColumn = -1
    Header.Options = [hoColumnResize, hoDrag]
    Header.Style = hsThickButtons
    HintAnimation = hatNone
    HintMode = hmDefault
    IncrementalSearchDirection = sdForward
    NodeDataSize = 4
    ParentColor = True
    TabOrder = 0
    OnExpanding = vwTreeExpanding
    OnFocusChanged = vwTreeFocusChanged
    OnFreeNode = vwTreeFreeNode
    OnGetText = vwTreeGetText
    OnInitNode = vwTreeInitNode
    Columns = <>
  end
end
