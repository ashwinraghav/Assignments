object FormTestOsProcessBasics: TFormTestOsProcessBasics
  Left = 167
  Top = 279
  Width = 665
  Height = 272
  Caption = 'FormTestOsProcessBasics'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    657
    245)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 15
    Top = 8
    Width = 66
    Height = 13
    Alignment = taRightJustify
    Caption = 'This process: '
  end
  object lbSelfProcess: TLabel
    Left = 84
    Top = 8
    Width = 64
    Height = 13
    Caption = 'lbSelfProcess'
  end
  object edProcessName: TEdit
    Left = 16
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'notepad.exe'
  end
  object ckWaitTermination: TCheckBox
    Left = 144
    Top = 33
    Width = 161
    Height = 17
    Caption = 'Wait for Termination?'
    TabOrder = 1
  end
  object btExecute: TButton
    Left = 16
    Top = 56
    Width = 75
    Height = 25
    Caption = '&Execute'
    TabOrder = 2
    OnClick = btExecuteClick
  end
  object Memo: TMemo
    Left = 16
    Top = 88
    Width = 617
    Height = 145
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
end
