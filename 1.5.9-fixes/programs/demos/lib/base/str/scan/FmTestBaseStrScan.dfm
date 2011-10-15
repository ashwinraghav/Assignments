object FormTestScan1: TFormTestScan1
  Left = 116
  Top = 311
  Width = 722
  Height = 220
  Caption = 'Sil.Str.Scan Tester & Demo (see Code)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  DesignSize = (
    714
    193)
  PixelsPerInch = 96
  TextHeight = 13
  object Scan1: TButton
    Left = 8
    Top = 5
    Width = 75
    Height = 25
    Hint = 'Simple string scanning of fields separated by '#39'/'#39
    Anchors = [akLeft]
    Caption = 'Scan &1'
    TabOrder = 0
    OnClick = Scan1Click
  end
  object Scan2: TButton
    Left = 8
    Top = 31
    Width = 75
    Height = 25
    Hint = 'Scanning of fields of fixed width'
    Anchors = [akLeft]
    Caption = 'Scan &2'
    TabOrder = 5
    OnClick = Scan2Click
  end
  object Scan3: TButton
    Left = 8
    Top = 57
    Width = 75
    Height = 25
    Hint = 'Combination of witdth with separators '
    Anchors = [akLeft]
    Caption = 'Scan &3'
    TabOrder = 9
    OnClick = Scan3Click
  end
  object Scan4: TButton
    Left = 8
    Top = 84
    Width = 75
    Height = 25
    Hint = 'Scanning fields with embedded spaces'
    Anchors = [akLeft]
    Caption = 'Scan &4'
    TabOrder = 12
    OnClick = Scan4Click
  end
  object Input1: TEdit
    Left = 88
    Top = 6
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 1
    Text = '01/02/03 hola'
  end
  object Result1: TEdit
    Left = 456
    Top = 6
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 3
  end
  object Format1: TEdit
    Left = 229
    Top = 6
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 2
    Text = '%d/%d/%d %s'
  end
  object Input2: TEdit
    Left = 88
    Top = 33
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 4
    Text = '20010203'
  end
  object Result2: TEdit
    Left = 456
    Top = 33
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 7
  end
  object Format2: TEdit
    Left = 229
    Top = 33
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 6
    Text = '%4d%2d%2d'
  end
  object Input3: TEdit
    Left = 88
    Top = 58
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 8
    Text = '2001/02/03'
  end
  object Result3: TEdit
    Left = 456
    Top = 58
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 11
  end
  object Format3: TEdit
    Left = 229
    Top = 58
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 10
    Text = '%4d/%2d/%2d'
  end
  object Input4: TEdit
    Left = 88
    Top = 84
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 13
    Text = '2001  02  03 hello world'
  end
  object Result4: TEdit
    Left = 456
    Top = 84
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 15
  end
  object Format4: TEdit
    Left = 229
    Top = 84
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 14
    Text = '%4d  %2d  %2d %4s %s'
  end
  object Scan5: TButton
    Left = 8
    Top = 110
    Width = 75
    Height = 25
    Hint = 'Scanning float numbers'
    Anchors = [akLeft]
    Caption = 'Scan &5'
    TabOrder = 16
    OnClick = Scan5Click
  end
  object Input5: TEdit
    Left = 88
    Top = 111
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 17
    Text = '-39.56789012-1234567890'
  end
  object Result5: TEdit
    Left = 456
    Top = 111
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 19
  end
  object Format5: TEdit
    Left = 229
    Top = 111
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 18
    Text = '%12f %10s'
  end
  object Scan6: TButton
    Left = 8
    Top = 136
    Width = 75
    Height = 25
    Hint = 'Scanning strings separated by some field splitting character'
    Anchors = [akLeft]
    Caption = 'Scan &6'
    TabOrder = 20
    OnClick = Scan6Click
  end
  object Input6: TEdit
    Left = 88
    Top = 137
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 21
    Text = 'one_two_three'
  end
  object Result6: TEdit
    Left = 456
    Top = 137
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 23
  end
  object Format6: TEdit
    Left = 229
    Top = 137
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 22
    Text = '%s_%s_%s'
  end
  object Scan7: TButton
    Left = 8
    Top = 163
    Width = 75
    Height = 25
    Hint = 
      'Same as above but the variables are fields of a record type (see' +
      ' OnClick code ...)'
    Anchors = [akLeft]
    Caption = 'Scan &7'
    TabOrder = 24
    OnClick = Scan7Click
  end
  object Input7: TEdit
    Left = 88
    Top = 163
    Width = 137
    Height = 21
    Anchors = [akLeft]
    TabOrder = 25
    Text = '20010203'
  end
  object Result7: TEdit
    Left = 456
    Top = 163
    Width = 249
    Height = 21
    Anchors = [akLeft, akRight]
    ReadOnly = True
    TabOrder = 27
  end
  object Format7: TEdit
    Left = 229
    Top = 163
    Width = 218
    Height = 21
    Anchors = [akLeft]
    TabOrder = 26
    Text = '%4d%2d%2d'
  end
end
