object FormTestFileListEvents: TFormTestFileListEvents
  Left = 70
  Top = 138
  Width = 647
  Height = 467
  Caption = 'FormTestFileListEvents'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 639
    Height = 37
    Align = alTop
    TabOrder = 0
    object btStartSearch: TButton
      Left = 4
      Top = 4
      Width = 95
      Height = 25
      Caption = 'Start search ...'
      Default = True
      TabOrder = 0
      OnClick = btStartSearchClick
    end
    object edFileMask: TEdit
      Left = 104
      Top = 7
      Width = 249
      Height = 21
      TabOrder = 1
      Text = '%SystemRoot%\*.dll'
    end
    object ckRecursive: TCheckBox
      Left = 357
      Top = 9
      Width = 76
      Height = 15
      Caption = 'Recursive?'
      TabOrder = 2
    end
  end
  object lvFiles: TListView
    Left = 0
    Top = 37
    Width = 639
    Height = 403
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'FileName'
      end
      item
        AutoSize = True
        Caption = 'Path'
      end
      item
        AutoSize = True
        Caption = 'Timestamp'
      end
      item
        Alignment = taRightJustify
        AutoSize = True
        Caption = 'Size'
      end>
    TabOrder = 1
    ViewStyle = vsReport
  end
end
