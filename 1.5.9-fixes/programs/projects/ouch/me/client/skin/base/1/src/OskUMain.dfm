object OuchSkinBase: TOuchSkinBase
  Left = 506
  Top = 159
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Ouch! Odyssey'
  ClientHeight = 656
  ClientWidth = 257
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object pnHeader: TPanel
    Left = 0
    Top = 0
    Width = 257
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    Visible = False
  end
  object pnUsers: TPanel
    Left = 0
    Top = 41
    Width = 257
    Height = 574
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 4
    TabOrder = 1
    object treeUsers: TTreeNT
      Left = 4
      Top = 4
      Width = 249
      Height = 566
      Align = alClient
      Indent = 19
      InsertMarkColor = clSilver
      ItemHeight = 16
      ScrollTime = 0
      TabOrder = 0
    end
  end
  object pnFooter: TPanel
    Left = 0
    Top = 615
    Width = 257
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    Visible = False
  end
  object Taskbar: TWinTaskbar
    Hint = 'Configuración'
    Icon.Data = {
      0000010001001010100000000000280100001600000028000000100000002000
      00000100040000000000C0000000000000000000000000000000000000000000
      000000008000008000000080800080000000800080008080000080808000C0C0
      C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
      00000000000000008FF80B7800000000883883000000000003B8330000000000
      03B333300000000003B300000000000003B333333000000008BB333300000000
      08B3033300000003388888330000000BBB3FFF080000000BBBB370F80000000B
      BBBBBB00000000038BBBB300000000003BBB300000000000000000000000C007
      0000E0070000F00F0000F01F0000F80F0000F8030000F0030000E0070000E007
      0000E0070000C0070000C0070000C00F0000E03F0000C03F0000E0FF0000}
    OnClick = TaskbarClick
    PopUpMenu = PopupMenu1
    ShowIcon = False
    HideApp = False
    DblClickRestores = False
    Left = 93
    Top = 415
  end
  object PopupMenu1: TPopupMenu
    Left = 124
    Top = 416
    object Cerrar1: TMenuItem
      Caption = 'Cerrar'
      OnClick = Cerrar1Click
    end
  end
end
