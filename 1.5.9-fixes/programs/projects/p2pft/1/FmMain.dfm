object foMain: TfoMain
  Left = 551
  Top = 258
  Width = 303
  Height = 423
  Caption = 'P2P FileTransfer 1.0.0-beta'
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
    Width = 295
    Height = 33
    Align = alTop
    TabOrder = 0
    DesignSize = (
      295
      33)
    object cbPeers: TComboBox
      Left = 4
      Top = 4
      Width = 237
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbPeersChange
    end
    object btConnect: TButton
      Left = 244
      Top = 4
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '+'
      TabOrder = 1
      OnClick = btConnectClick
    end
    object btDisconnect: TButton
      Left = 268
      Top = 4
      Width = 21
      Height = 21
      Anchors = [akTop, akRight]
      Caption = '-'
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 33
    Width = 295
    Height = 363
    Align = alClient
    TabOrder = 1
    DesignSize = (
      295
      363)
    object lvFiles: TListView
      Left = 8
      Top = 8
      Width = 277
      Height = 345
      Anchors = [akLeft, akTop, akRight, akBottom]
      Columns = <
        item
          Caption = 'Progreso'
        end
        item
          Caption = 'Nombre'
        end
        item
          Caption = 'Tama'#241'o'
        end
        item
          Caption = 'Fecha'
        end>
      TabOrder = 0
      ViewStyle = vsReport
    end
  end
end
