object FormTestContainer: TFormTestContainer
  Left = 147
  Top = 62
  Width = 870
  Height = 640
  Caption = 'SilContainer demonstration (see Code)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  DesignSize = (
    862
    613)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 19
    Width = 22
    Height = 13
    Alignment = taRightJustify
    Caption = 'First:'
  end
  object Label2: TLabel
    Left = 175
    Top = 19
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = 'Last:'
  end
  object Label3: TLabel
    Left = 339
    Top = 19
    Width = 25
    Height = 13
    Alignment = taRightJustify
    Caption = 'Step:'
  end
  object Label4: TLabel
    Left = 14
    Top = 51
    Width = 23
    Height = 13
    Alignment = taRightJustify
    Caption = 'Item:'
  end
  object Memo: TMemo
    Left = 200
    Top = 128
    Width = 641
    Height = 457
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object edFirst: TEdit
    Left = 43
    Top = 16
    Width = 121
    Height = 21
    Hint = 
      'Index of the first item to which apply algorithms. Empty represe' +
      'nts the List.First or the List.Last according to the sign of Ste' +
      'p'
    TabOrder = 1
  end
  object edLast: TEdit
    Left = 203
    Top = 16
    Width = 121
    Height = 21
    Hint = 
      'Index of the last item to which apply algorithms. . Empty repres' +
      'ents the List.First or the List.Last according to the sign of St' +
      'ep'
    TabOrder = 2
  end
  object edDelta: TEdit
    Left = 368
    Top = 16
    Width = 122
    Height = 21
    Hint = 
      'Step of the enumeration. Can be negative to represent reverse it' +
      'eration.'
    TabOrder = 3
  end
  object Enumerate: TButton
    Left = 497
    Top = 15
    Width = 77
    Height = 25
    Hint = 'Enumerate all the items from <First> to <Last> using <Step>'
    Caption = 'Enumerate'
    TabOrder = 4
    OnClick = EnumerateClick
  end
  object ListBox: TListBox
    Left = 16
    Top = 128
    Width = 177
    Height = 457
    Hint = 'Show all the items within the container'
    ItemHeight = 13
    TabOrder = 5
  end
  object Refresh: TButton
    Left = 17
    Top = 95
    Width = 75
    Height = 25
    Hint = 'Updates the ListBox with the current contents of the container'
    Caption = 'Refresh'
    TabOrder = 6
    OnClick = RefreshClick
  end
  object edItem: TEdit
    Left = 43
    Top = 49
    Width = 121
    Height = 21
    TabOrder = 7
  end
  object Find: TButton
    Left = 252
    Top = 48
    Width = 75
    Height = 25
    Hint = 'Find the requested item in the container'
    Caption = 'Find'
    TabOrder = 8
    OnClick = FindClick
  end
  object Contains: TButton
    Left = 328
    Top = 48
    Width = 75
    Height = 25
    Hint = 'Ask the container to see if it has the item requested'
    Caption = 'Contains'
    TabOrder = 9
    OnClick = ContainsClick
  end
  object Exchange: TButton
    Left = 579
    Top = 15
    Width = 75
    Height = 25
    Hint = 'Exchange the items pointed by <First> and <Last>'
    Caption = 'Exchange'
    TabOrder = 10
    OnClick = ExchangeClick
  end
  object Remove: TButton
    Left = 659
    Top = 15
    Width = 75
    Height = 25
    Hint = 'Remove all the items from <First> to <Last> using <Step>'
    Caption = 'Remove'
    TabOrder = 11
    OnClick = RemoveClick
  end
  object Move: TButton
    Left = 736
    Top = 15
    Width = 75
    Height = 25
    Hint = 'Move the item pointed by <First> over the item pointed by <Last>'
    Caption = 'Move'
    TabOrder = 12
    OnClick = MoveClick
  end
  object Add: TButton
    Left = 176
    Top = 48
    Width = 75
    Height = 25
    Hint = 'Add an item to the container'
    Caption = 'Add'
    TabOrder = 13
    OnClick = AddClick
  end
end
