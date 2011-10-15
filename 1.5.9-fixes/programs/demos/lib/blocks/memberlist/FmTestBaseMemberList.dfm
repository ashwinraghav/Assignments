object FormTestBaseMemberList: TFormTestBaseMemberList
  Left = 434
  Top = 457
  Width = 526
  Height = 348
  Caption = 'FormTestBaseMemberList'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    518
    321)
  PixelsPerInch = 96
  TextHeight = 13
  object lbMember: TLabel
    Left = 171
    Top = 298
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
  end
  object veList: TValueListEditor
    Left = -1
    Top = -1
    Width = 519
    Height = 289
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnSelectCell = veListSelectCell
    ColWidths = (
      150
      363)
  end
  object Enumerate: TButton
    Left = 0
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Enumerate'
    TabOrder = 1
    OnClick = EnumerateClick
  end
  object edValue: TEdit
    Left = 176
    Top = 294
    Width = 121
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
  end
  object SetValue: TButton
    Left = 304
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&SetValue'
    Default = True
    TabOrder = 3
    OnClick = SetValueClick
  end
  object ChangeValues: TButton
    Left = 432
    Top = 294
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'ChangeValues'
    TabOrder = 4
    OnClick = ChangeValuesClick
  end
end
