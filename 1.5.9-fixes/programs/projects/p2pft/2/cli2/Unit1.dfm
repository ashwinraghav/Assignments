object Form1: TForm1
  Left = 300
  Top = 446
  Width = 653
  Height = 306
  Caption = 'cliente'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button1: TButton
    Left = 16
    Top = 16
    Width = 75
    Height = 25
    Caption = 'activar'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 16
    Top = 52
    Width = 75
    Height = 25
    Caption = 'desactivar'
    TabOrder = 1
    OnClick = Button2Click
  end
  object Panel1: TPanel
    Left = 252
    Top = 0
    Width = 393
    Height = 279
    Align = alRight
    Caption = 'Panel1'
    TabOrder = 2
    object ListBox1: TListBox
      Left = 1
      Top = 21
      Width = 391
      Height = 257
      Align = alClient
      ItemHeight = 13
      Items.Strings = (
        '\')
      Sorted = True
      TabOrder = 0
      OnDblClick = ListBox1DblClick
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 391
      Height = 20
      Align = alTop
      TabOrder = 1
    end
  end
end
