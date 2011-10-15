object FormModuleProperties: TFormModuleProperties
  Left = 318
  Top = 257
  Width = 441
  Height = 358
  BorderWidth = 5
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lvProperties: TListView
    Left = 0
    Top = 0
    Width = 423
    Height = 260
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'Value'
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object pnButtons: TPanel
    Left = 0
    Top = 260
    Width = 423
    Height = 61
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    DesignSize = (
      423
      61)
    object btOK: TSilButton
      Left = 179
      Top = 10
      Width = 65
      Height = 48
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333333333333333333333330000333333333333333333333333F33333333333
        00003333344333333333333333388F3333333333000033334224333333333333
        338338F3333333330000333422224333333333333833338F3333333300003342
        222224333333333383333338F3333333000034222A22224333333338F338F333
        8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
        33333338F83338F338F33333000033A33333A222433333338333338F338F3333
        0000333333333A222433333333333338F338F33300003333333333A222433333
        333333338F338F33000033333333333A222433333333333338F338F300003333
        33333333A222433333333333338F338F00003333333333333A22433333333333
        3338F38F000033333333333333A223333333333333338F830000333333333333
        333A333333333333333338330000333333333333333333333333333333333333
        0000}
      GlyphCount = 2
      Centered = False
      Spacing = 0
      Shape = bsFlat
      ColorFace = clBtnFace
      ColorShadow = clBtnShadow
      ColorHighlight = clBtnHighlight
      AutoRepeat = False
      Cancel = True
      TextVertical = False
      ModalResult = 1
      AutoHighlight = True
      HighlightDelay = 100
      Color = clBtnFace
      Anchors = []
      Caption = '&OK'
      Default = True
      ParentColor = False
      TabOrder = 0
    end
  end
end
