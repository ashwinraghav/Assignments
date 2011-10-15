object FormUserInfo: TFormUserInfo
  Left = 403
  Top = 316
  Width = 308
  Height = 219
  Caption = 'FormUserInfo'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCloseQuery = FormCloseQuery
  PixelsPerInch = 96
  TextHeight = 13
  object lbID: TLabel
    Left = 1
    Top = 2
    Width = 296
    Height = 13
    Alignment = taCenter
    AutoSize = False
  end
  object Label1: TLabel
    Left = 45
    Top = 28
    Width = 29
    Height = 13
    Alignment = taRightJustify
    Caption = 'Nick: '
  end
  object Label2: TLabel
    Left = 14
    Top = 52
    Width = 60
    Height = 13
    Alignment = taRightJustify
    Caption = 'Full name: '
  end
  object Label3: TLabel
    Left = 44
    Top = 75
    Width = 30
    Height = 13
    Alignment = taRightJustify
    Caption = 'mail: '
  end
  object Label4: TLabel
    Left = 14
    Top = 98
    Width = 60
    Height = 13
    Alignment = taRightJustify
    Caption = 'password: '
  end
  object Label5: TLabel
    Left = 13
    Top = 122
    Width = 61
    Height = 13
    Alignment = taRightJustify
    Caption = 'confirmar: '
  end
  object edNick: TEdit
    Left = 77
    Top = 24
    Width = 177
    Height = 21
    TabOrder = 0
  end
  object edFullName: TEdit
    Left = 77
    Top = 48
    Width = 177
    Height = 21
    TabOrder = 1
  end
  object edMail: TEdit
    Left = 77
    Top = 72
    Width = 177
    Height = 21
    TabOrder = 2
  end
  object Aceptar: TButtonEx
    Left = 110
    Top = 156
    Width = 94
    Height = 31
    AutoHighlight = False
    Spacing = 4
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
    AutoRepeat = False
    TextVertical = False
    Caption = 'Aceptar'
    Default = True
    ModalResult = 1
    TabOrder = 3
  end
  object Cancelar: TButtonEx
    Left = 203
    Top = 156
    Width = 94
    Height = 31
    AutoHighlight = False
    Spacing = 4
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
    AutoRepeat = False
    Cancel = True
    TextVertical = False
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 4
  end
  object edPassword: TEdit
    Left = 77
    Top = 96
    Width = 177
    Height = 21
    PasswordChar = '*'
    TabOrder = 5
  end
  object edConfirm: TEdit
    Left = 77
    Top = 120
    Width = 177
    Height = 21
    PasswordChar = '*'
    TabOrder = 6
  end
end
