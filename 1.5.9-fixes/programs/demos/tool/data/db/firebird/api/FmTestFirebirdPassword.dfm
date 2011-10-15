object FormTestFirebirdPassword: TFormTestFirebirdPassword
  Left = 213
  Top = 702
  ActiveControl = edDatabase
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Connection specification ...'
  ClientHeight = 171
  ClientWidth = 408
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnBackgroung: TSilGradientPanel
    Left = 0
    Top = 0
    Width = 408
    Height = 130
    Align = alClient
    ColorBegin = 12109767
    ColorEnd = 13556698
    Style = gsButterfly
    BevelWidth = 2
    BorderWidth = 2
    TabOrder = 0
    object Label1: TLabel
      Left = 32
      Top = 38
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = 'Database: '
    end
    object Label2: TLabel
      Left = 25
      Top = 60
      Width = 67
      Height = 13
      Alignment = taRightJustify
      Caption = 'User Name: '
    end
    object Label3: TLabel
      Left = 32
      Top = 84
      Width = 60
      Height = 13
      Alignment = taRightJustify
      Caption = 'Password: '
    end
    object edUserName: TEdit
      Left = 95
      Top = 58
      Width = 170
      Height = 21
      Color = 13556698
      TabOrder = 1
    end
    object edPassword: TEdit
      Left = 95
      Top = 82
      Width = 170
      Height = 19
      Color = 13556698
      Font.Charset = SYMBOL_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Marlett'
      Font.Style = [fsBold]
      ParentFont = False
      PasswordChar = 'h'
      TabOrder = 2
    end
    object edDatabase: TEdit
      Left = 95
      Top = 34
      Width = 234
      Height = 21
      Color = 13556698
      TabOrder = 0
    end
  end
  object pnBottom: TSilGradientPanel
    Left = 0
    Top = 130
    Width = 408
    Height = 41
    Align = alBottom
    ColorBegin = 12109767
    ColorEnd = 13556698
    BorderWidth = 2
    TabOrder = 1
    object btOK: TSil3dButton
      Left = 241
      Top = 3
      Width = 82
      Height = 35
      Elevation = 15
      Shape = ksRoundRect
      AutoBackgroundColor = True
      Transparent = False
      TransparentColor = clFuchsia
      AutoRepeat = False
      TextVertical = False
      Color = 13556698
      Align = alRight
      Caption = 'OK'
      Default = True
      ParentColor = False
      TabOrder = 0
    end
    object btCancel: TSil3dButton
      Left = 323
      Top = 3
      Width = 82
      Height = 35
      Elevation = 15
      Shape = ksRoundRect
      AutoBackgroundColor = True
      Transparent = False
      TransparentColor = clFuchsia
      AutoRepeat = False
      Cancel = True
      TextVertical = False
      Color = 13556698
      Align = alRight
      Caption = 'Cancel'
      ParentColor = False
      TabOrder = 1
    end
  end
end
