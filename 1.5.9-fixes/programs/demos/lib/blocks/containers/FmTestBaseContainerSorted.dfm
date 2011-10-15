object FormTestSorted: TFormTestSorted
  Left = 306
  Top = 239
  Width = 562
  Height = 370
  Caption = 'Simple example of a sorted vector container (see Code)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    554
    343)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 39
    Width = 29
    Height = 13
    Alignment = taRightJustify
    Caption = 'Time: '
  end
  object Label2: TLabel
    Left = 123
    Top = 39
    Width = 49
    Height = 13
    Caption = 'microSecs'
  end
  object lbCount: TLabel
    Left = 208
    Top = 320
    Width = 3
    Height = 13
    Hint = 'The actual number of elements in the container'
    Anchors = [akLeft, akBottom]
  end
  object btAdd: TButton
    Left = 16
    Top = 64
    Width = 75
    Height = 25
    Hint = 'Adds a new string in the vector'
    Caption = '&Add'
    TabOrder = 0
    OnClick = btAddClick
  end
  object ckSorted: TCheckBox
    Left = 18
    Top = 15
    Width = 97
    Height = 17
    Hint = 'Select the mode of operation'
    Caption = 'Ordenada?'
    Checked = True
    State = cbChecked
    TabOrder = 1
    OnClick = ckSortedClick
  end
  object edValue: TEdit
    Left = 97
    Top = 65
    Width = 121
    Height = 21
    Hint = 
      'Write the value that you want to insert. If empty inserts a rand' +
      'om value'
    TabOrder = 2
  end
  object lbItems: TListBox
    Left = 280
    Top = 8
    Width = 265
    Height = 329
    Anchors = [akLeft, akTop, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 3
    OnClick = lbItemsClick
  end
  object btRefresh: TButton
    Left = 177
    Top = 9
    Width = 102
    Height = 25
    Hint = 'Iterates through the vector in the forward direction'
    Caption = '&Refresh Direct'
    TabOrder = 4
    OnClick = btRefreshClick
  end
  object btClear: TButton
    Left = 16
    Top = 89
    Width = 75
    Height = 25
    Hint = 'Clears the whole vector'
    Caption = '&Clear'
    TabOrder = 5
    OnClick = btClearClick
  end
  object btDelete: TButton
    Left = 16
    Top = 114
    Width = 75
    Height = 25
    Hint = 'Deletes the item pointed by the selected index in the list box'
    Caption = '&Delete'
    TabOrder = 6
    OnClick = btDeleteClick
  end
  object btSort: TButton
    Left = 16
    Top = 164
    Width = 75
    Height = 25
    Hint = 'Sorts the vector using quick sort algoithm'
    Caption = '&Sort'
    TabOrder = 7
    OnClick = btSortClick
  end
  object btRemove: TButton
    Left = 16
    Top = 139
    Width = 75
    Height = 25
    Hint = 'Removes the item indicated by the value in the edit box'
    Caption = 'Re&move'
    TabOrder = 8
    OnClick = btRemoveClick
  end
  object btRefreshLast: TButton
    Left = 177
    Top = 34
    Width = 102
    Height = 25
    Hint = 'Iterates through the vector in reverse order'
    Caption = 'Re&fresh Reverse'
    TabOrder = 9
    OnClick = btRefreshLastClick
  end
  object edLapse: TEdit
    Left = 40
    Top = 36
    Width = 79
    Height = 21
    Hint = 
      'The time that took to execute the last operation expressed in mi' +
      'croseconds'
    TabOrder = 10
  end
end
