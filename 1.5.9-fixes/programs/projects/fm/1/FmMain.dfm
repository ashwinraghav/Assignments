object FormMain: TFormMain
  Left = 164
  Top = 273
  Width = 856
  Height = 528
  Caption = 'FormMain'
  Color = 11784398
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object spCenter: TSplitter
    Left = 414
    Top = 0
    Height = 501
  end
  object pnLeft: TPanel
    Left = 0
    Top = 0
    Width = 414
    Height = 501
    Align = alLeft
    BevelOuter = bvNone
    BorderWidth = 2
    ParentColor = True
    TabOrder = 0
  end
  object pnRight: TPanel
    Left = 417
    Top = 0
    Width = 431
    Height = 501
    Align = alClient
    BevelOuter = bvNone
    BorderWidth = 2
    ParentColor = True
    TabOrder = 1
  end
end
