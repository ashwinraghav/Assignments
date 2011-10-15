object FormTestCrossrefs: TFormTestCrossrefs
  Left = 44
  Top = 305
  Width = 716
  Height = 374
  Caption = 'Tester of the ICrossReference mechanism. (See code for details)'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnDestroy = FormDestroy
  DesignSize = (
    708
    347)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 191
    Top = 7
    Width = 513
    Height = 26
    AutoSize = False
    Caption = 
      'Creates the first object and keeps it in a variable in the main ' +
      'form.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object Label2: TLabel
    Left = 191
    Top = 39
    Width = 513
    Height = 26
    AutoSize = False
    Caption = 
      'Creates the second taking the first as a argument. The second ma' +
      'kes a Reference to the first and keeps and indirect pointer to i' +
      't.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object Label3: TLabel
    Left = 191
    Top = 71
    Width = 513
    Height = 26
    AutoSize = False
    Caption = 
      'If you check the state of the reference now you'#39'll see that it s' +
      'till valid.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object Label4: TLabel
    Left = 189
    Top = 97
    Width = 513
    Height = 43
    AutoSize = False
    Caption = 
      'When you release the first it will be destroyed because the seco' +
      'nd only taken an indirect pointer. So, in normal cases the secon' +
      'd will be left with an invalid pointer. But, as a result of the ' +
      'cross ref mechanism the reference is cleaned automatically.'
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object Label5: TLabel
    Left = 187
    Top = 141
    Width = 513
    Height = 18
    AutoSize = False
    Caption = 'The you can release the second now. '
    Font.Charset = ANSI_CHARSET
    Font.Color = clMaroon
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object btCreateFirst: TButton
    Left = 8
    Top = 8
    Width = 175
    Height = 25
    Caption = '&A - Create First Object'
    TabOrder = 0
    OnClick = btCreateFirstClick
  end
  object btCreateSecond: TButton
    Left = 8
    Top = 40
    Width = 175
    Height = 25
    Caption = '&B - Create Second Object'
    TabOrder = 1
    OnClick = btCreateSecondClick
  end
  object btDestroyFirst: TButton
    Left = 8
    Top = 104
    Width = 175
    Height = 25
    Caption = '&C - Release First Object'
    TabOrder = 2
    OnClick = btDestroyFirstClick
  end
  object btDestroySecond: TButton
    Left = 8
    Top = 136
    Width = 175
    Height = 25
    Caption = '&D - Release Second Object'
    TabOrder = 3
    OnClick = btDestroySecondClick
  end
  object btCheckRreference: TButton
    Left = 8
    Top = 72
    Width = 177
    Height = 25
    Caption = 'Check &Reference'
    TabOrder = 4
    OnClick = btCheckRreferenceClick
  end
  object Memo: TMemo
    Left = 8
    Top = 168
    Width = 695
    Height = 173
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 5
  end
end
