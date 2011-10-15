object FrameView: TFrameView
  Left = 0
  Top = 0
  Width = 443
  Height = 277
  Align = alClient
  TabOrder = 0
  OnResize = FrameResize
  object spMiddle: TSplitter
    Left = 0
    Top = 185
    Width = 443
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object pnTree: TPanel
    Left = 0
    Top = 0
    Width = 443
    Height = 185
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 2
    ParentColor = True
    TabOrder = 0
  end
  object pnFiles: TPanel
    Left = 0
    Top = 188
    Width = 443
    Height = 89
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    ParentColor = True
    TabOrder = 1
  end
end
