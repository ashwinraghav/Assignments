object FormTestLockers: TFormTestLockers
  Left = 175
  Top = 390
  Width = 662
  Height = 264
  Caption = 
    'Demonstration of a generic locking mechanism (see code for detai' +
    'ls ...)'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    654
    237)
  PixelsPerInch = 96
  TextHeight = 13
  object lbCount: TLabel
    Left = 75
    Top = 89
    Width = 6
    Height = 13
    Caption = '0'
  end
  object Label1: TLabel
    Left = 4
    Top = 88
    Width = 69
    Height = 13
    Alignment = taRightJustify
    Caption = 'Lock counter: '
  end
  object btAddLock: TButton
    Left = 0
    Top = 8
    Width = 130
    Height = 25
    Caption = '&Add New Lock'
    TabOrder = 0
    OnClick = btAddLockClick
  end
  object btRemoveLocks: TButton
    Left = 0
    Top = 34
    Width = 130
    Height = 25
    Caption = '&Remove Last Lock'
    TabOrder = 1
    OnClick = btRemoveLocksClick
  end
  object btExclusiveLock: TButton
    Left = 1
    Top = 60
    Width = 129
    Height = 25
    Caption = '&Get exclusive lock'
    TabOrder = 2
    OnClick = btExclusiveLockClick
  end
  object Memo: TMemo
    Left = 136
    Top = 8
    Width = 511
    Height = 223
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
end
