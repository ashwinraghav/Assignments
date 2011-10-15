object FormTestOsTimers: TFormTestOsTimers
  Left = 130
  Top = 334
  Width = 546
  Height = 201
  Caption = 'FormTestOsTimers'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object lbTime: TLabel
    Left = 0
    Top = 0
    Width = 538
    Height = 174
    Align = alClient
    Alignment = taCenter
    Caption = '00:00:00'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -96
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Layout = tlCenter
  end
end
