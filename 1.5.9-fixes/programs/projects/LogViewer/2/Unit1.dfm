object Form1: TForm1
  Left = 404
  Top = 369
  Width = 663
  Height = 229
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clLime
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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 655
    Height = 202
    Align = alClient
    ParentColor = True
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WantReturns = False
    WordWrap = False
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
end
