object foRecurrence: TfoRecurrence
  Left = 312
  Top = 217
  BorderStyle = bsDialog
  Caption = 'Edit Schedule Recurrence'
  ClientHeight = 402
  ClientWidth = 486
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btAccept: TButton
    Left = 328
    Top = 372
    Width = 73
    Height = 25
    Caption = '&OK'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 3
    OnClick = btAcceptClick
  end
  object btCancel: TButton
    Left = 408
    Top = 372
    Width = 73
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
  object gbDuration: TGroupBox
    Left = 272
    Top = 4
    Width = 209
    Height = 129
    Caption = 'Duration'
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 37
      Height = 13
      Caption = 'Start at'
    end
    object rbDurationNoEnd: TRadioButton
      Left = 8
      Top = 104
      Width = 89
      Height = 17
      Caption = 'Unspecified'
      Checked = True
      TabOrder = 1
      TabStop = True
      OnClick = rbDurationEndClick
    end
    object rbDurationCount: TRadioButton
      Left = 8
      Top = 76
      Width = 109
      Height = 17
      Caption = 'Recurrence count'
      TabOrder = 2
      OnClick = rbDurationEndClick
    end
    object rbDurationEnd: TRadioButton
      Left = 8
      Top = 48
      Width = 37
      Height = 17
      Caption = 'End at'
      TabOrder = 3
      OnClick = rbDurationEndClick
    end
    object dpStartDate: TDateTimePicker
      Left = 88
      Top = 16
      Width = 93
      Height = 21
      Date = 37645.537111064800000000
      Time = 37645.537111064800000000
      TabOrder = 0
    end
    object dpEndDate: TDateTimePicker
      Left = 88
      Top = 44
      Width = 93
      Height = 21
      Date = 37645.537111064800000000
      Time = 37645.537111064800000000
      Enabled = False
      TabOrder = 4
    end
    object edEndCount: TEdit
      Left = 128
      Top = 72
      Width = 37
      Height = 21
      Enabled = False
      TabOrder = 5
      Text = '1'
    end
    object udDurationCount: TUpDown
      Left = 165
      Top = 72
      Width = 15
      Height = 21
      Associate = edEndCount
      Min = 1
      Max = 999
      Position = 1
      TabOrder = 6
    end
  end
  object gbOccurs: TGroupBox
    Left = 4
    Top = 4
    Width = 261
    Height = 129
    Caption = 'Frequency'
    TabOrder = 0
    object laInitTime: TLabel
      Left = 8
      Top = 48
      Width = 78
      Height = 13
      Caption = 'Occurs between'
    end
    object laEndTime: TLabel
      Left = 161
      Top = 48
      Width = 18
      Height = 13
      Caption = 'and'
    end
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 28
      Height = 13
      Caption = 'Every'
    end
    object edEvery: TEdit
      Left = 96
      Top = 16
      Width = 37
      Height = 21
      TabOrder = 0
      Text = '1'
    end
    object coEveryUnit: TComboBox
      Left = 156
      Top = 16
      Width = 89
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 2
      Text = 'minute(s)'
      OnClick = coEveryUnitClick
      Items.Strings = (
        'minute(s)'
        'hour(s)'
        'day(s)')
    end
    object dpEveryEnd: TDateTimePicker
      Left = 192
      Top = 44
      Width = 53
      Height = 21
      Date = 37645.537111064800000000
      Format = 'HH:mm'
      Time = 37645.537111064800000000
      DateMode = dmUpDown
      Kind = dtkTime
      TabOrder = 4
    end
    object udEvery: TUpDown
      Left = 133
      Top = 16
      Width = 15
      Height = 21
      Associate = edEvery
      Min = 1
      Max = 999
      Position = 1
      TabOrder = 1
    end
    object dpEveryIni: TDateTimePicker
      Left = 96
      Top = 44
      Width = 53
      Height = 21
      Date = 37645.537111064800000000
      Format = 'HH:mm'
      Time = 37645.537111064800000000
      DateMode = dmUpDown
      Kind = dtkTime
      TabOrder = 3
    end
  end
  object gbFilters: TGroupBox
    Left = 4
    Top = 136
    Width = 477
    Height = 229
    Caption = 'Filters'
    TabOrder = 2
    object clDayPosition: TCheckListBox
      Left = 88
      Top = 16
      Width = 121
      Height = 97
      Enabled = False
      ItemHeight = 13
      TabOrder = 0
    end
    object clDayOfWeek: TCheckListBox
      Left = 216
      Top = 16
      Width = 121
      Height = 97
      Enabled = False
      ItemHeight = 13
      TabOrder = 1
    end
    object clMonths: TCheckListBox
      Left = 344
      Top = 16
      Width = 121
      Height = 201
      Enabled = False
      ItemHeight = 13
      TabOrder = 2
    end
    object clDayNumber: TCheckListBox
      Left = 88
      Top = 120
      Width = 249
      Height = 97
      Columns = 5
      Enabled = False
      ItemHeight = 13
      TabOrder = 3
    end
    object rbFilterWeek: TRadioButton
      Left = 8
      Top = 44
      Width = 77
      Height = 17
      Caption = 'Week'
      TabOrder = 4
      OnClick = rbFilterNoneClick
    end
    object rbFilterMonth: TRadioButton
      Left = 8
      Top = 72
      Width = 77
      Height = 17
      Caption = 'Month'
      TabOrder = 5
      OnClick = rbFilterNoneClick
    end
    object rbFilterNone: TRadioButton
      Left = 8
      Top = 20
      Width = 77
      Height = 17
      Caption = 'None'
      Checked = True
      TabOrder = 6
      TabStop = True
      OnClick = rbFilterNoneClick
    end
  end
end
