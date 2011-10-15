object FormTestEnvironmentExpand: TFormTestEnvironmentExpand
  Left = 130
  Top = 294
  Width = 680
  Height = 338
  Caption = 'FormTestEnvironmentExpand'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel: TPanel
    Left = 0
    Top = 0
    Width = 672
    Height = 43
    Align = alTop
    TabOrder = 0
  end
  object lvPath: TListView
    Left = 0
    Top = 84
    Width = 672
    Height = 227
    Align = alClient
    BevelInner = bvSpace
    BevelOuter = bvNone
    BorderStyle = bsNone
    Columns = <
      item
        Caption = '#'
      end
      item
        AutoSize = True
        Caption = 'Path'
      end
      item
        AutoSize = True
        Caption = 'Expanded'
      end>
    TabOrder = 1
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 43
    Width = 672
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 2
    DesignSize = (
      672
      41)
    object Label1: TLabel
      Left = 46
      Top = 11
      Width = 32
      Height = 13
      Alignment = taRightJustify
      Caption = 'PATH:'
    end
    object edPath: TEdit
      Left = 80
      Top = 8
      Width = 573
      Height = 21
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
      OnChange = edPathChange
    end
  end
end
