object FormTestSparseList: TFormTestSparseList
  Left = 282
  Top = 218
  Width = 593
  Height = 446
  Caption = 'Demo de TRandomList'
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
  object Splitter1: TSplitter
    Left = 0
    Top = 193
    Width = 585
    Height = 3
    Cursor = crVSplit
    Align = alTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 92
    Width = 585
    Height = 101
    Align = alTop
    BorderWidth = 5
    Caption = 'Panel1'
    TabOrder = 0
    object TR: TListBox
      Left = 6
      Top = 6
      Width = 573
      Height = 89
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 196
    Width = 585
    Height = 223
    Align = alClient
    BorderWidth = 5
    Caption = 'Panel2'
    TabOrder = 1
    object LB: TListBox
      Left = 6
      Top = 6
      Width = 573
      Height = 211
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object Panel3: TPanel
    Left = 0
    Top = 0
    Width = 585
    Height = 92
    Align = alTop
    Caption = 'Panel3'
    TabOrder = 2
    object Label1: TLabel
      Left = 135
      Top = 11
      Width = 52
      Height = 13
      Caption = 'Elementos:'
    end
    object Label2: TLabel
      Left = 249
      Top = 11
      Width = 55
      Height = 13
      Caption = 'Iteraciones:'
    end
    object Velocidad: TButton
      Left = 18
      Top = 5
      Width = 90
      Height = 25
      Caption = 'Velocidad'
      TabOrder = 0
      OnClick = VelocidadClick
    end
    object edElems: TEdit
      Left = 190
      Top = 7
      Width = 50
      Height = 21
      TabOrder = 1
      Text = '100'
    end
    object edIterac: TEdit
      Left = 307
      Top = 7
      Width = 50
      Height = 21
      TabOrder = 2
      Text = '10000'
    end
    object chkPerturb: TCheckBox
      Left = 372
      Top = 9
      Width = 109
      Height = 17
      Caption = 'Dato divergente'
      TabOrder = 3
    end
    object Operaciones: TButton
      Left = 18
      Top = 33
      Width = 90
      Height = 25
      Caption = 'Op. como TList'
      TabOrder = 4
      OnClick = OperacionesClick
    end
    object chkOpExchange: TCheckBox
      Left = 138
      Top = 37
      Width = 72
      Height = 17
      Caption = 'Exchange'
      TabOrder = 5
    end
    object chkOpExtract: TCheckBox
      Left = 220
      Top = 37
      Width = 57
      Height = 17
      Caption = 'Extract'
      TabOrder = 6
    end
    object chkOpMove: TCheckBox
      Left = 345
      Top = 37
      Width = 57
      Height = 17
      Caption = 'Move'
      TabOrder = 7
    end
    object Random: TButton
      Left = 18
      Top = 61
      Width = 90
      Height = 25
      Caption = 'Op. Random'
      TabOrder = 8
      OnClick = RandomClick
    end
  end
end
