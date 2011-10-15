object FormTestFileReadwrite: TFormTestFileReadwrite
  Left = 76
  Top = 127
  Width = 728
  Height = 464
  Caption = 'FormTestFileReadwrite'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    720
    437)
  PixelsPerInch = 96
  TextHeight = 13
  object btWriteFile: TButton
    Left = 0
    Top = 413
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = 'Write Memo!'
    TabOrder = 0
    OnClick = btWriteFileClick
  end
  object edFileName: TEdit
    Left = 0
    Top = 386
    Width = 345
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 1
    Text = 'C:\temp.txt'
  end
  object WriteMemo: TMemo
    Left = 0
    Top = 0
    Width = 345
    Height = 385
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      '- Simple demo of file reading a writing facilities.'
      ''
      ''
      'This is some simple text to write to the new created file.'
      ''
      'Press Write Memo! when you ready.'
      ''
      
        'Next, press Read Memo and see the contents of the file written o' +
        'n the '
      'Read Memo on the right.'
      ''
      '')
    TabOrder = 2
  end
  object btReadFile: TButton
    Left = 352
    Top = 413
    Width = 75
    Height = 23
    Anchors = [akLeft, akBottom]
    Caption = '&Read Again'
    TabOrder = 3
    OnClick = btReadFileClick
  end
  object ReadMemo: TMemo
    Left = 352
    Top = 0
    Width = 365
    Height = 385
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
  end
end
