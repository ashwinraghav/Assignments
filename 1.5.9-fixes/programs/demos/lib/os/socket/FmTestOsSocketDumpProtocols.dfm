object FormTestOsSocketDumpProtocols: TFormTestOsSocketDumpProtocols
  Left = 84
  Top = 155
  Width = 926
  Height = 578
  Caption = 'FormTestOsSocketDumpProtocols'
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
    Height = 510
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = '#'
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
      end>
    ReadOnly = True
    RowSelect = True
    ParentColor = True
    TabOrder = 0
    ViewStyle = vsReport
  end
  object Panel1: TPanel
    Left = 0
    Top = 510
    Width = 918
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btDumpProtocols: TButton
      Left = 9
      Top = 8
      Width = 99
      Height = 25
      Caption = 'Dump Protocols'
      Default = True
      TabOrder = 0
      OnClick = btDumpProtocolsClick
    end
  end
end
