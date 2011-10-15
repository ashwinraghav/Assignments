object MainForm: TMainForm
  Left = 48
  Top = 497
  Width = 1164
  Height = 220
  AlphaBlendValue = 0
  Caption = 'MainForm'
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 6790114
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333
    333FFFFFFFFF3333333BFBFBFBFB3333333FFFFFFFFF3333330BFBFBFBFB3333
    3010FFFFFFFF333330170BFBFBFB3333301170FFFFFF33330711190BFBFB3333
    08819990FFFF333088FF9999033333088FFFF00033333088FFF003333333088F
    FF033333333388FFF093333333338FFF0933333333330FF03333333333330000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  KeyPreview = True
  OldCreateOrder = False
  PopupMenu = Popup
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object spSplitter: TSplitter
    Left = 0
    Top = 134
    Width = 1156
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
  end
  object lvMensajes: TListView
    Left = 0
    Top = 0
    Width = 1156
    Height = 134
    Align = alClient
    BorderStyle = bsNone
    Columns = <
      item
        Caption = 'ID'
        Width = 0
      end
      item
        Caption = 'Mensaje'
        Width = 3000
      end>
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    ParentColor = True
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = lvMensajesDblClick
    OnSelectItem = lvMensajesSelectItem
  end
  object edMensaje: TMemo
    Left = 0
    Top = 139
    Width = 1156
    Height = 54
    Align = alBottom
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WantReturns = False
  end
  object Popup: TPopupMenu
    OnPopup = PopupPopup
    Left = 40
    Top = 16
    object itLogState: TMenuItem
      Caption = 'Parar!'
      ShortCut = 16472
      OnClick = itLogStateClick
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object miCopy: TMenuItem
      Caption = 'Copiar'
      ShortCut = 16451
      OnClick = miCopyClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object Minimizar1: TMenuItem
      Caption = '&Minimizar'
      ShortCut = 27
      OnClick = Minimizar1Click
    end
    object itRestore: TMenuItem
      Caption = '&Restaurar'
      OnClick = RestoreClick
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object itAlwaysOnTop: TMenuItem
      Caption = 'Siempre Visible'
      ShortCut = 16468
      OnClick = itAlwaysOnTopClick
    end
    object itClear: TMenuItem
      Caption = '&Limpiar'
      ShortCut = 16460
      OnClick = itClearClick
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object itFiltros: TMenuItem
      Caption = 'Filtros...'
      ShortCut = 16454
      OnClick = itFiltrosClick
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object itControles: TMenuItem
      Caption = '&Controles'
      object itSelectFont: TMenuItem
        Caption = 'Tipo de &Letra'
        OnClick = itSelectFontClick
      end
      object itForegroundColor: TMenuItem
        Caption = 'Color de &Texto'
        OnClick = itForegroundColorClick
      end
      object itBackgroundColor: TMenuItem
        Caption = 'Color de &Fondo'
        OnClick = itBackgroundColorClick
      end
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Salir1: TMenuItem
      Caption = 'Salir'
      ShortCut = 32856
      OnClick = Salir1Click
    end
  end
  object Taskbar: TWinTaskbar
    Icon.Data = {
      0000010001001010100000000000280100001600000028000000100000002000
      00000100040000000000C0000000000000000000000000000000000000000000
      000000008000008000000080800080000000800080008080000080808000C0C0
      C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF003333
      333FFFFFFFFF3333333BFBFBFBFB3333333FFFFFFFFF3333330BFBFBFBFB3333
      3010FFFFFFFF333330170BFBFBFB3333301170FFFFFF33330711190BFBFB3333
      08819990FFFF333088FF9999033333088FFFF00033333088FFF003333333088F
      FF033333333388FFF093333333338FFF0933333333330FF03333333333330000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    OnClick = RestoreClick
    PopUpMenu = Popup
    ShowIcon = False
    HideApp = False
    DblClickRestores = False
    Left = 96
    Top = 16
  end
  object HintTimer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = HintTimerTimer
    Left = 164
    Top = 19
  end
  object dgColor: TColorDialog
    Options = [cdFullOpen, cdAnyColor]
    Left = 232
    Top = 16
  end
  object dgFont: TFontDialog
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    Left = 288
    Top = 16
  end
end
