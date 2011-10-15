object FormTestOsSocketResolveAddress: TFormTestOsSocketResolveAddress
  Left = 276
  Top = 253
  Width = 656
  Height = 318
  Caption = 'FormTestOsSocketResolveAddress'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Display: TMemo
    Left = 0
    Top = 0
    Width = 648
    Height = 250
    Align = alClient
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 250
    Width = 648
    Height = 41
    Align = alBottom
    TabOrder = 1
    object btResolve: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = '&Resolve'
      Default = True
      TabOrder = 0
      OnClick = btResolveClick
    end
    object edAddress: TEdit
      Left = 104
      Top = 10
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'localhost'
    end
  end
end
