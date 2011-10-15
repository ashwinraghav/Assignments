object FormTestEvents: TFormTestEvents
  Left = 320
  Top = 243
  Width = 394
  Height = 202
  Caption = 'FormTestEvents'
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
    386
    175)
  PixelsPerInch = 96
  TextHeight = 13
  object btDispatchEvent: TButton
    Left = 2
    Top = 8
    Width = 85
    Height = 25
    Caption = '&Dispatch'
    TabOrder = 0
    OnClick = btDispatchEventClick
  end
  object Memo: TMemo
    Left = 88
    Top = 8
    Width = 289
    Height = 161
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
  end
  object edText: TEdit
    Left = 0
    Top = 34
    Width = 86
    Height = 21
    TabOrder = 2
    Text = 'This is an event!'
  end
end
