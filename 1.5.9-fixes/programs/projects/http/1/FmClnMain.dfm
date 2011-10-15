object FormHttpClient: TFormHttpClient
  Left = 186
  Top = 201
  Width = 777
  Height = 540
  Caption = 'FormHttpClient'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    769
    513)
  PixelsPerInch = 96
  TextHeight = 13
  object edUrl: TEdit
    Left = 8
    Top = 8
    Width = 723
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    Text = 'http://atenea/data/doc/info/ntdll/ntdll.htm'
  end
  object Get: TButton
    Left = 737
    Top = 6
    Width = 27
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Get'
    Default = True
    TabOrder = 1
    OnClick = GetClick
  end
  object edContent: TMemo
    Left = 8
    Top = 56
    Width = 753
    Height = 449
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 2
  end
  object edContentType: TEdit
    Left = 8
    Top = 31
    Width = 121
    Height = 21
    TabOrder = 3
  end
end
