object AppHint: TAppHint
  Left = 330
  Top = 305
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 148
  ClientWidth = 216
  Color = clInfoBk
  TransparentColor = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  KeyPreview = True
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Text: TMemo
    Left = 0
    Top = 0
    Width = 216
    Height = 148
    Align = alClient
    BorderStyle = bsNone
    ParentColor = True
    ReadOnly = True
    TabOrder = 0
  end
  object HideTimer: TTimer
    Enabled = False
    OnTimer = HideTimerTimer
    Left = 86
    Top = 40
  end
end
