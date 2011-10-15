object FormThreadSort: TFormThreadSort
  Left = 288
  Top = 250
  BorderStyle = bsDialog
  Caption = 'Sorting'
  ClientHeight = 473
  ClientWidth = 564
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    564
    473)
  PixelsPerInch = 96
  TextHeight = 13
  object pbBubbleSort: TPaintBox
    Left = 8
    Top = 24
    Width = 177
    Height = 375
    Anchors = [akLeft, akTop, akBottom]
    OnPaint = pbBubbleSortPaint
  end
  object pbSelectionSort: TPaintBox
    Left = 192
    Top = 24
    Width = 177
    Height = 375
    Anchors = [akLeft, akTop, akBottom]
    OnPaint = pbSelectionSortPaint
  end
  object pbQuickSort: TPaintBox
    Left = 376
    Top = 24
    Width = 177
    Height = 375
    Anchors = [akLeft, akTop, akBottom]
    OnPaint = pbQuickSortPaint
  end
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 65
    Height = 13
    Caption = 'Bubble Sort'
  end
  object Label2: TLabel
    Left = 192
    Top = 8
    Width = 79
    Height = 13
    Caption = 'Selection Sort'
  end
  object Label3: TLabel
    Left = 376
    Top = 8
    Width = 58
    Height = 13
    Caption = 'Quick Sort'
  end
  object lbCount: TLabel
    Left = 10
    Top = 402
    Width = 3
    Height = 13
    Anchors = [akLeft, akBottom]
  end
  object lbTimeBubble: TLabel
    Left = 178
    Top = 402
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
  end
  object lbTimeSelection: TLabel
    Left = 362
    Top = 402
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
  end
  object lbTimeQuick: TLabel
    Left = 546
    Top = 402
    Width = 3
    Height = 13
    Alignment = taRightJustify
    Anchors = [akLeft, akBottom]
  end
  object btStart: TButton
    Left = 480
    Top = 432
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start!'
    TabOrder = 0
    OnClick = btStartClick
  end
end
