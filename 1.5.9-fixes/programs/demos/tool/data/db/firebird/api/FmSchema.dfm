object FormSchema: TFormSchema
  Left = 192
  Top = 81
  Width = 783
  Height = 540
  Caption = 'FormSchema'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object vwSchema: TTreeView
    Left = 0
    Top = 0
    Width = 273
    Height = 513
    Align = alLeft
    Indent = 19
    TabOrder = 0
    OnCollapsing = vwSchemaCollapsing
    OnDeletion = vwSchemaDeletion
    OnExpanding = vwSchemaExpanding
  end
end
