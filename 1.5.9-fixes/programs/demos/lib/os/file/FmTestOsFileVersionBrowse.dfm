object FormTestFileVersion: TFormTestFileVersion
  Left = 107
  Top = 183
  Width = 870
  Height = 767
  Caption = 'FormTestFileVersion'
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
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 740
    Align = alClient
    BorderWidth = 10
    Caption = 'Panel1'
    TabOrder = 0
    object lvFiles: TListView
      Left = 11
      Top = 11
      Width = 840
      Height = 718
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'Path'
        end
        item
          AutoSize = True
          Caption = 'Nombre'
        end
        item
          AutoSize = True
          Caption = 'Modificado'
        end
        item
          AutoSize = True
          Caption = 'Version'
        end
        item
          Caption = 'Description'
        end
        item
          AutoSize = True
          Caption = 'Company'
        end>
      RowSelect = True
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
end
