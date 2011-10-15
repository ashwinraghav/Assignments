inherited FormOuchLogon: TFormOuchLogon
  Left = 298
  Top = 484
  ActiveControl = edNick
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Ingreso'
  ClientHeight = 171
  ClientWidth = 298
  OldCreateOrder = True
  ShowHint = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  inline Button: TFrameOuchButtons
    Left = 0
    Top = 133
    Width = 298
    Height = 38
    Align = alBottom
    AutoScroll = False
    TabOrder = 0
    inherited pnButtons: TPanel
      Width = 298
      inherited Bevel1: TBevel
        Left = 181
      end
      inherited Bevel2: TBevel
        Left = 289
      end
      inherited btCerrar: TSilButton
        Left = 185
        Hint = 'Cancela la operaci'#243'n ...'
        OnClick = CancelClick
      end
      inherited btAceptar: TSilButton
        Left = 77
        Hint = 'Trata de autenticarse con el nick y la clave ingresadas ...'
        OnClick = AcceptClick
      end
    end
  end
  object pnClientArea: TPanel
    Left = 0
    Top = 0
    Width = 298
    Height = 133
    Align = alClient
    BevelInner = bvLowered
    BorderWidth = 3
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentColor = True
    ParentFont = False
    TabOrder = 1
    DesignSize = (
      298
      133)
    object pnControls: TPanel
      Left = 59
      Top = 16
      Width = 179
      Height = 102
      Anchors = []
      BevelOuter = bvNone
      ParentColor = True
      TabOrder = 0
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 32
        Height = 13
        Caption = '&Nick:'
        FocusControl = edNick
      end
      object Label1: TLabel
        Left = 1
        Top = 43
        Width = 66
        Height = 13
        Caption = '&Password:'
        FocusControl = edPassword
      end
      object edPassword: TEdit
        Left = 3
        Top = 57
        Width = 177
        Height = 20
        Hint = 
          'Ingrese la clave correspondiente al usuario ... el programa le p' +
          'edir'#225' confirmaci'#243'n si se va a dar de alta un usuario nuevo ...'
        Color = 14413550
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Wingdings'
        Font.Style = []
        ParentFont = False
        PasswordChar = 'l'
        TabOrder = 1
      end
      object edNick: TEdit
        Left = 3
        Top = 16
        Width = 177
        Height = 21
        Hint = 
          'Ingrese el nick de su usuario, o elija uno si va a crear una cue' +
          'nta nueva ...'
        Color = 14413550
        TabOrder = 0
      end
      object CheckBox1: TCheckBox
        Left = 2
        Top = 80
        Width = 175
        Height = 17
        Hint = 
          'Marque est'#225' opci'#243'n si no desea que el programa le pida la clave ' +
          'cada vez que se inicia ...'
        Caption = '&Memorizar'
        TabOrder = 2
        Visible = False
      end
    end
  end
end
