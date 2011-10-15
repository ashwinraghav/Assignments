object FormTestOsSocketDumpServices: TFormTestOsSocketDumpServices
  Left = 66
  Top = 127
  Width = 926
  Height = 484
  Caption = 'FormTestOsSocketDumpServices'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lvList: TListView
    Left = 0
    Top = 0
    Width = 918
    Height = 416
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Name'
      end
      item
        AutoSize = True
        Caption = 'Protocol'
      end
      item
        AutoSize = True
        Caption = 'Aliases'
      end
      item
        AutoSize = True
        Caption = 'Comment'
      end
      item
        AutoSize = True
        Caption = 'Port'
      end>
    ReadOnly = True
    RowSelect = True
    ParentColor = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 416
    Width = 918
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btDumpServices: TButton
      Left = 9
      Top = 8
      Width = 99
      Height = 25
      Caption = 'Dump Services'
      Default = True
      TabOrder = 0
      OnClick = btDumpServicesClick
    end
  end
end
