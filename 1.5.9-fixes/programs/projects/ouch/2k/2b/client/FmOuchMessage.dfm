inherited FormOuchMessage: TFormOuchMessage
  Left = 300
  Top = 226
  Width = 658
  Height = 380
  ActiveControl = edMessage
  Caption = 'Mensaje'
  Constraints.MinHeight = 172
  Constraints.MinWidth = 406
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnMemo: TPanel
    Left = 0
    Top = 62
    Width = 650
    Height = 254
    Align = alClient
    BorderWidth = 4
    ParentColor = True
    TabOrder = 2
    object edMessage: TMemo
      Left = 5
      Top = 5
      Width = 640
      Height = 244
      Align = alClient
      Color = 14413550
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
    end
  end
  object pnButtons: TPanel
    Left = 0
    Top = 316
    Width = 650
    Height = 37
    Align = alBottom
    BorderWidth = 2
    ParentColor = True
    TabOrder = 3
    object ckSendBlindCopy: TCheckBox
      Left = 14
      Top = 10
      Width = 70
      Height = 17
      Caption = '&Sin Copia'
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 3
      Visible = False
    end
    object btClose: TSilButton
      Left = 543
      Top = 3
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
      TabOrder = 2
      OnClick = btCloseClick
    end
    object btRecvReply: TSilButton
      Left = 439
      Top = 3
      Width = 104
      Height = 31
      Layout = blCenter
      Centered = True
      Spacing = 4
      Shape = bsFlat
      ColorFace = 14413550
      ColorShadow = 10465970
      ColorHighlight = clWhite
      AutoRepeat = False
      Cancel = True
      TextVertical = False
      AutoHighlight = True
      HighlightDelay = 150
      Color = 14413550
      Align = alRight
      Caption = '&Contestar'
      Default = True
      ParentColor = False
      TabOrder = 1
      Visible = False
      OnClick = btRecvReplyClick
    end
    object btSendEnviar: TSilButton
      Left = 335
      Top = 3
      Width = 104
      Height = 31
      Layout = blCenter
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
      Centered = True
      Spacing = 4
      Shape = bsFlat
      ColorFace = 14413550
      ColorShadow = 10465970
      ColorHighlight = clWhite
      AutoRepeat = False
      Cancel = True
      TextVertical = False
      ModalResult = 1
      AutoHighlight = True
      HighlightDelay = 150
      Color = 14413550
      Align = alRight
      Caption = '&Env'#237'ar'
      Default = True
      ParentColor = False
      TabOrder = 0
      Visible = False
      OnClick = btSendEnviarClick
    end
  end
  object pnRecvHeader: TPanel
    Left = 0
    Top = 0
    Width = 650
    Height = 31
    Align = alTop
    ParentColor = True
    TabOrder = 1
    Visible = False
    DesignSize = (
      650
      31)
    object lbFrom: TLabel
      Left = 8
      Top = 8
      Width = 18
      Height = 13
      Alignment = taRightJustify
      Caption = 'De:'
      FocusControl = edFrom
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbDate: TLabel
      Left = 410
      Top = 8
      Width = 15
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'El: '
      FocusControl = edDate
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lbTime: TLabel
      Left = 531
      Top = 8
      Width = 33
      Height = 13
      Alignment = taRightJustify
      Anchors = [akTop, akRight]
      Caption = 'A las: '
      FocusControl = edTime
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edFrom: TEdit
      Left = 33
      Top = 5
      Width = 368
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
    object edDate: TEdit
      Left = 429
      Top = 8
      Width = 94
      Height = 15
      Anchors = [akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 1
      Text = 'Lun 10/22/2000'
    end
    object edTime: TEdit
      Left = 565
      Top = 8
      Width = 74
      Height = 15
      Anchors = [akTop, akRight]
      BorderStyle = bsNone
      Font.Charset = ANSI_CHARSET
      Font.Color = clMaroon
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = True
      ParentFont = False
      ReadOnly = True
      TabOrder = 2
      Text = '99:99:99'
    end
  end
  object pnSendHeader: TPanel
    Left = 0
    Top = 31
    Width = 650
    Height = 31
    Align = alTop
    ParentColor = True
    TabOrder = 0
    Visible = False
    DesignSize = (
      650
      31)
    object lbTo: TLabel
      Left = 15
      Top = 8
      Width = 11
      Height = 13
      Alignment = taRightJustify
      Caption = 'A:'
      FocusControl = edTo
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object edTo: TEdit
      Left = 32
      Top = 5
      Width = 612
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
end
