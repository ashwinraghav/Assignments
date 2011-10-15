inherited FormOuchHistory: TFormOuchHistory
  Left = 302
  Top = 613
  Width = 546
  Height = 344
  ActiveControl = pnClientArea
  BorderStyle = bsSizeToolWin
  Caption = 'Mensajes ...'
  Color = 12312541
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnButtons: TPanel
    Left = 0
    Top = 276
    Width = 538
    Height = 41
    Align = alBottom
    BorderWidth = 4
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentColor = True
    ParentFont = False
    TabOrder = 0
    object btClose: TSilButton
      Left = 429
      Top = 5
      Width = 104
      Height = 31
      Layout = blCenter
      Glyph.Data = {
        DE010000424DDE01000000000000760000002800000024000000120000000100
        0400000000006801000000000000000000001000000010000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        333333333333333333333333000033338833333333333333333F333333333333
        0000333911833333983333333388F333333F3333000033391118333911833333
        38F38F333F88F33300003339111183911118333338F338F3F8338F3300003333
        911118111118333338F3338F833338F3000033333911111111833333338F3338
        3333F8330000333333911111183333333338F333333F83330000333333311111
        8333333333338F3333383333000033333339111183333333333338F333833333
        00003333339111118333333333333833338F3333000033333911181118333333
        33338333338F333300003333911183911183333333383338F338F33300003333
        9118333911183333338F33838F338F33000033333913333391113333338FF833
        38F338F300003333333333333919333333388333338FFF830000333333333333
        3333333333333333333888330000333333333333333333333333333333333333
        0000}
      GlyphCount = 2
      Centered = True
      Spacing = 4
      Shape = bsFlat
      ColorFace = 14413550
      ColorShadow = 10465970
      ColorHighlight = clWhite
      AutoRepeat = False
      Cancel = True
      TextVertical = False
      ModalResult = 2
      AutoHighlight = True
      HighlightDelay = 150
      Color = 14413550
      Align = alRight
      Caption = '&Cerrar'
      ParentColor = False
      TabOrder = 0
      OnClick = btCloseClick
    end
  end
  object pnHeading: TPanel
    Left = 0
    Top = 0
    Width = 538
    Height = 30
    Align = alTop
    Alignment = taLeftJustify
    BorderWidth = 4
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentColor = True
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      538
      30)
    object Label1: TLabel
      Left = 5
      Top = 5
      Width = 93
      Height = 20
      Align = alLeft
      Alignment = taRightJustify
      Caption = ' Mensajes de: '
      Layout = tlCenter
    end
    object edAccount: TEdit
      Left = 96
      Top = 4
      Width = 438
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      AutoSize = False
      Color = 14413550
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      TabOrder = 0
    end
  end
  object pnClientArea: TPanel
    Left = 0
    Top = 30
    Width = 538
    Height = 246
    Align = alClient
    BorderWidth = 4
    ParentBackground = True
    ParentColor = True
    TabOrder = 2
  end
end
