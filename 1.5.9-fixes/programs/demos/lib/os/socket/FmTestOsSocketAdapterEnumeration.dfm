object FormTestOsSocketAdapterEnumeration: TFormTestOsSocketAdapterEnumeration
  Left = 276
  Top = 253
  Width = 656
  Height = 318
  Caption = 'FormTestOsSocketAdapterEnumeration'
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
    Top = 250
    Width = 648
    Height = 41
    Align = alBottom
    TabOrder = 0
    object btEnumerate: TButton
      Left = 8
      Top = 8
      Width = 134
      Height = 25
      Caption = '&Enumerate Adapters'
      Default = True
      TabOrder = 0
      OnClick = btEnumerateClick
    end
  end
  object lvList: TListView
    Left = 0
    Top = 0
    Width = 648
    Height = 250
    Align = alClient
    Columns = <
      item
        AutoSize = True
        Caption = 'Host'
      end
      item
        AutoSize = True
        Caption = 'Address'
      end
      item
        AutoSize = True
        Caption = 'Netmask'
      end
      item
        AutoSize = True
        Caption = 'Network'
      end
      item
        AutoSize = True
        Caption = 'Broadcast'
      end
      item
        AutoSize = True
        Caption = 'Type'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
  end
end
