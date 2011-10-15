object Form1: TForm1
  Left = 387
  Top = 381
  Width = 568
  Height = 247
  Caption = 'Form1'
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
  object msecs: TLabel
    Left = 104
    Top = 128
    Width = 30
    Height = 13
    Caption = 'msecs'
  end
  object Label2: TLabel
    Left = 8
    Top = 20
    Width = 20
    Height = 13
    Caption = 'host'
  end
  object Label3: TLabel
    Left = 192
    Top = 20
    Width = 18
    Height = 13
    Caption = 'port'
  end
  object Label4: TLabel
    Left = 368
    Top = 20
    Width = 54
    Height = 13
    Caption = 'packet size'
  end
  object send: TButton
    Left = 8
    Top = 124
    Width = 75
    Height = 25
    Caption = 'send'
    TabOrder = 0
    OnClick = sendClick
  end
  object Edit1: TEdit
    Left = 48
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 1
    Text = 'localhost'
  end
  object Edit2: TEdit
    Left = 224
    Top = 16
    Width = 121
    Height = 21
    TabOrder = 2
    Text = '12345'
  end
  object Button1: TButton
    Left = 8
    Top = 56
    Width = 75
    Height = 25
    Caption = 'create server'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 96
    Top = 72
    Width = 75
    Height = 25
    Caption = 'destroy'
    TabOrder = 4
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 8
    Top = 88
    Width = 75
    Height = 25
    Caption = 'create client'
    TabOrder = 5
    OnClick = Button3Click
  end
  object Edit3: TEdit
    Left = 436
    Top = 16
    Width = 77
    Height = 21
    TabOrder = 6
    Text = '1024'
  end
  object edTime: TEdit
    Left = 139
    Top = 125
    Width = 121
    Height = 21
    TabOrder = 7
  end
  object Memo1: TMemo
    Left = 320
    Top = 72
    Width = 185
    Height = 89
    ScrollBars = ssVertical
    TabOrder = 8
  end
end
