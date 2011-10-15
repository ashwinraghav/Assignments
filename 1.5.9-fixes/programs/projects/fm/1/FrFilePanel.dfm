object FrameFiles: TFrameFiles
  Left = 0
  Top = 0
  Width = 549
  Height = 439
  Align = alClient
  TabOrder = 0
  object vwList: TVirtualStringTree
    Left = 0
    Top = 23
    Width = 549
    Height = 416
    Align = alClient
    BevelInner = bvNone
    BevelKind = bkFlat
    BorderStyle = bsNone
    BorderWidth = 2
    DragMode = dmAutomatic
    DragOperations = [doCopy, doMove, doLink]
    Header.AutoSizeIndex = 0
    Header.Font.Charset = ANSI_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -11
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = [fsBold]
    Header.Height = 18
    Header.Options = [hoColumnResize, hoDblClickResize, hoDrag, hoShowSortGlyphs, hoVisible]
    Header.Style = hsXPStyle
    HintAnimation = hatNone
    HintMode = hmDefault
    Images = imFileIcons
    IncrementalSearchDirection = sdForward
    ParentColor = True
    TabOrder = 0
    TreeOptions.AnimationOptions = [toAnimatedToggle]
    TreeOptions.AutoOptions = [toAutoDropExpand, toAutoExpand, toAutoScroll, toAutoScrollOnExpand, toAutoTristateTracking, toAutoDeleteMovedNodes]
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toEditable, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning]
    TreeOptions.PaintOptions = [toShowDropmark, toThemeAware, toUseBlendedImages, toGhostedIfUnfocused]
    TreeOptions.SelectionOptions = [toFullRowSelect, toMultiSelect]
    OnFreeNode = vwListFreeNode
    OnGetText = vwListGetText
    OnGetImageIndex = vwListGetImageIndex
    OnInitNode = vwListInitNode
    Columns = <
      item
        Position = 0
        Width = 143
        WideText = 'Nombre'
      end
      item
        Position = 1
        Width = 100
        WideText = 'Tipo'
      end
      item
        Alignment = taRightJustify
        Position = 2
        WideText = 'Tama'#241'o'
      end
      item
        Position = 3
        WideText = 'Atributos'
      end
      item
        Position = 4
        Width = 100
        WideText = 'Modificado'
      end
      item
        Position = 5
        Width = 100
        WideText = 'Creado'
      end>
    WideDefaultText = ''
  end
  object pnPath: TPanel
    Left = 0
    Top = 0
    Width = 549
    Height = 23
    Align = alTop
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    DesignSize = (
      549
      23)
    object stPath: TEdit
      Left = 2
      Top = 3
      Width = 541
      Height = 16
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      BevelInner = bvNone
      BevelOuter = bvNone
      BorderStyle = bsNone
      Color = 15659762
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Verdana'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object imFileIcons: TImageList
    Left = 64
    Top = 80
  end
end
