object Form1: TForm1
  Left = 204
  Top = 208
  Width = 352
  Height = 382
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox6: TGroupBox
    Left = 12
    Top = 120
    Width = 317
    Height = 93
    Caption = 'tipo'
    TabOrder = 0
    object RadioButton12: TRadioButton
      Left = 16
      Top = 20
      Width = 81
      Height = 17
      Caption = 'a la hora'
      TabOrder = 0
    end
    object MaskEdit6: TMaskEdit
      Left = 192
      Top = 20
      Width = 53
      Height = 21
      EditMask = '!90:00:00;1;_'
      MaxLength = 8
      TabOrder = 1
      Text = '  :  :  '
    end
    object DateTimePicker1: TDateTimePicker
      Left = 92
      Top = 20
      Width = 89
      Height = 21
      CalAlignment = dtaLeft
      Date = 37645.5371110648
      Time = 37645.5371110648
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 2
    end
    object RadioButton11: TRadioButton
      Left = 16
      Top = 52
      Width = 81
      Height = 17
      Caption = 'recurrente'
      TabOrder = 3
    end
    object Button2: TButton
      Left = 92
      Top = 52
      Width = 75
      Height = 25
      Caption = 'cambiar...'
      TabOrder = 4
      OnClick = Button2Click
    end
  end
  object GroupBox7: TGroupBox
    Left = 12
    Top = 228
    Width = 317
    Height = 113
    Caption = 'Borrar la ejecuci'#243'n automaticamente'
    TabOrder = 1
    object RadioButton13: TRadioButton
      Left = 16
      Top = 40
      Width = 153
      Height = 17
      Caption = 'cuando termina bien'
      TabOrder = 0
    end
    object RadioButton14: TRadioButton
      Left = 16
      Top = 60
      Width = 153
      Height = 17
      Caption = 'cuando termina mal'
      TabOrder = 1
    end
    object RadioButton15: TRadioButton
      Left = 16
      Top = 80
      Width = 157
      Height = 17
      Caption = 'si no se va a ejecutar mas'
      TabOrder = 2
    end
    object RadioButton16: TRadioButton
      Left = 16
      Top = 20
      Width = 113
      Height = 17
      Caption = 'nunca'
      Checked = True
      TabOrder = 3
      TabStop = True
    end
  end
  object GroupBox8: TGroupBox
    Left = 12
    Top = 8
    Width = 317
    Height = 101
    Caption = 'Ejecuci'#243'n'
    TabOrder = 2
    object Label3: TLabel
      Left = 8
      Top = 20
      Width = 35
      Height = 13
      Caption = 'nombre'
    end
    object Edit3: TEdit
      Left = 60
      Top = 20
      Width = 121
      Height = 21
      TabOrder = 0
      Text = 'Edit3'
    end
    object CheckBox1: TCheckBox
      Left = 60
      Top = 52
      Width = 97
      Height = 17
      Caption = 'habilitada'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
  end
end
