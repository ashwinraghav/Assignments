object Form1: TForm1
  Left = 249
  Top = 201
  Width = 512
  Height = 286
  ActiveControl = Button1
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    504
    259)
  PixelsPerInch = 96
  TextHeight = 13
  object Memo: TMemo
    Left = 22
    Top = 48
    Width = 464
    Height = 187
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
  end
  object Button1: TButton
    Left = 24
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button&1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button&2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 208
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Button&3'
    TabOrder = 3
    OnClick = Button3Click
  end
end
