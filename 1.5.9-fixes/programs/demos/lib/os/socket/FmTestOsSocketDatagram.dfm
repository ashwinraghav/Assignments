object Form1: TForm1
  Left = 344
  Top = 608
  Width = 396
  Height = 84
  Caption = 'test1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 4
    Top = 36
    Width = 58
    Height = 13
    Caption = 'retardo [ms]:'
  end
  object Label4: TLabel
    Left = 64
    Top = 36
    Width = 32
    Height = 13
    Caption = 'Label4'
  end
  object Label1: TLabel
    Left = 96
    Top = 8
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object Button1: TButton
    Left = 4
    Top = 4
    Width = 75
    Height = 25
    Caption = 'cerrar'
    TabOrder = 0
    OnClick = Button1Click
  end
end
