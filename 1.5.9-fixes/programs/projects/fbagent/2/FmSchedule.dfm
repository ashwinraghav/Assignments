object foSchedule: TfoSchedule
  Left = 345
  Top = 194
  BorderStyle = bsDialog
  Caption = 'Edit Job Schedule'
  ClientHeight = 325
  ClientWidth = 405
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label3: TLabel
    Left = 4
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Name:'
  end
  object gbExecKind: TGroupBox
    Left = 4
    Top = 28
    Width = 397
    Height = 153
    Caption = 'Schedule Type'
    TabOrder = 2
    object Label1: TLabel
      Left = 276
      Top = 20
      Width = 37
      Height = 13
      Caption = 'at time:'
    end
    object Label2: TLabel
      Left = 132
      Top = 20
      Width = 41
      Height = 13
      Caption = 'on date:'
    end
    object rbExecKindDay: TRadioButton
      Left = 8
      Top = 20
      Width = 105
      Height = 17
      Caption = 'One time:'
      Checked = True
      TabOrder = 0
      TabStop = True
      OnClick = DoChangeType
    end
    object dtExecDay: TDateTimePicker
      Left = 180
      Top = 16
      Width = 89
      Height = 21
      Date = 37645.537111064800000000
      Time = 37645.537111064800000000
      TabOrder = 2
    end
    object rbExecKindRec: TRadioButton
      Left = 8
      Top = 48
      Width = 113
      Height = 17
      Caption = 'Recurring schedule'
      TabOrder = 1
      OnClick = DoChangeType
    end
    object btRecChange: TButton
      Left = 132
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Change...'
      TabOrder = 4
      OnClick = btRecChangeClick
    end
    object meExecTime: TDateTimePicker
      Left = 320
      Top = 16
      Width = 53
      Height = 21
      Date = 37645.537111064800000000
      Format = 'HH:mm'
      Time = 37645.537111064800000000
      DateMode = dmUpDown
      Kind = dtkTime
      TabOrder = 3
    end
    object meRecurring: TMemo
      Left = 24
      Top = 76
      Width = 361
      Height = 65
      TabStop = False
      Color = clBtnFace
      ReadOnly = True
      TabOrder = 5
      WantReturns = False
    end
  end
  object btAccept: TButton
    Left = 240
    Top = 296
    Width = 77
    Height = 25
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btAcceptClick
  end
  object btCancel: TButton
    Left = 324
    Top = 296
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
  end
  object rbExecDel: TRadioGroup
    Left = 4
    Top = 184
    Width = 397
    Height = 105
    Caption = 'Automatically delete schedule'
    ItemIndex = 0
    Items.Strings = (
      'Never'
      'On scuccessful completion'
      'On completion failed'
      'On out of scope')
    TabOrder = 3
  end
  object edName: TEdit
    Left = 52
    Top = 4
    Width = 285
    Height = 21
    MaxLength = 64
    TabOrder = 0
  end
  object cbEnabled: TCheckBox
    Left = 344
    Top = 8
    Width = 61
    Height = 17
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 1
  end
end
