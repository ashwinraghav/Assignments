object foSchedule: TfoSchedule
  Left = 456
  Top = 204
  Width = 475
  Height = 428
  Caption = 'foSchedule'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Notebook1: TNotebook
    Left = 161
    Top = 4
    Width = 296
    Height = 217
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Once'
      object GroupBox9: TGroupBox
        Left = 4
        Top = 4
        Width = 289
        Height = 209
        Caption = 'Ejecutar una vez'
        TabOrder = 0
        object Label4: TLabel
          Left = 12
          Top = 48
          Width = 26
          Height = 13
          Caption = 'Hora:'
        end
        object Label1: TLabel
          Left = 12
          Top = 24
          Width = 21
          Height = 13
          Caption = 'D'#237'a:'
        end
        object MaskEdit3: TMaskEdit
          Left = 48
          Top = 44
          Width = 53
          Height = 21
          EditMask = '!90:00:00;1;_'
          MaxLength = 8
          TabOrder = 0
          Text = '  :  :  '
        end
        object DateTimePicker3: TDateTimePicker
          Left = 48
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
          TabOrder = 1
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Hourly'
      object GroupBox15: TGroupBox
        Left = 4
        Top = 4
        Width = 289
        Height = 209
        Caption = 'Ejecutar por tiempo'
        TabOrder = 0
        object Label13: TLabel
          Left = 12
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Cada'
        end
        object Edit9: TEdit
          Left = 48
          Top = 20
          Width = 25
          Height = 21
          TabOrder = 0
          Text = '1'
        end
        object ComboBox7: TComboBox
          Left = 80
          Top = 20
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 1
          Items.Strings = (
            'segundos'
            'minutos'
            'horas')
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Daily'
      object GroupBox10: TGroupBox
        Left = 4
        Top = 4
        Width = 289
        Height = 209
        Caption = 'Ejecutar por d'#237'a'
        TabOrder = 0
        object Label5: TLabel
          Left = 12
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Cada'
        end
        object Label6: TLabel
          Left = 80
          Top = 24
          Width = 25
          Height = 13
          Caption = 'dia(s)'
        end
        object Label10: TLabel
          Left = 12
          Top = 56
          Width = 45
          Height = 13
          Caption = 'A la hora:'
        end
        object Edit4: TEdit
          Left = 48
          Top = 20
          Width = 25
          Height = 21
          TabOrder = 0
          Text = '1'
        end
        object MaskEdit5: TMaskEdit
          Left = 64
          Top = 52
          Width = 53
          Height = 21
          EditMask = '!90:00:00;1;_'
          MaxLength = 8
          TabOrder = 1
          Text = '  :  :  '
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Weekly'
      object GroupBox11: TGroupBox
        Left = 4
        Top = 4
        Width = 289
        Height = 209
        Caption = 'Ejecutar por semana'
        TabOrder = 0
        object Label7: TLabel
          Left = 12
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Cada'
        end
        object Label8: TLabel
          Left = 80
          Top = 24
          Width = 48
          Height = 13
          Caption = 'semana(s)'
        end
        object Label9: TLabel
          Left = 12
          Top = 56
          Width = 42
          Height = 13
          Caption = 'Los dias:'
        end
        object Edit5: TEdit
          Left = 48
          Top = 20
          Width = 25
          Height = 21
          TabOrder = 0
          Text = '1'
        end
        object CheckListBox3: TCheckListBox
          Left = 64
          Top = 52
          Width = 133
          Height = 97
          ItemHeight = 13
          Items.Strings = (
            'domingo'
            'lunes'
            'martes'
            'mi'#233'rcoles'
            'jueves'
            'viernes'
            's'#225'bado')
          TabOrder = 1
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Monthly'
      object GroupBox12: TGroupBox
        Left = 4
        Top = 4
        Width = 289
        Height = 209
        Caption = 'Ejecutar por mes'
        TabOrder = 0
        object RadioButton19: TRadioButton
          Left = 12
          Top = 24
          Width = 45
          Height = 17
          Caption = 'D'#237'a'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object RadioButton20: TRadioButton
          Left = 12
          Top = 52
          Width = 33
          Height = 17
          Caption = 'El'
          TabOrder = 1
        end
        object ComboBox5: TComboBox
          Left = 48
          Top = 48
          Width = 89
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 2
          Items.Strings = (
            'primer'
            'segundo'
            'tercer'
            'cuarto'
            #250'ltimo')
        end
        object ComboBox6: TComboBox
          Left = 144
          Top = 48
          Width = 109
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 3
          Items.Strings = (
            'domingo'
            'lunes'
            'martes'
            'mi'#233'rcoles'
            'jueves'
            'viernes'
            's'#225'bado')
        end
        object Edit7: TEdit
          Left = 56
          Top = 20
          Width = 25
          Height = 21
          TabOrder = 4
          Text = '1'
        end
        object GroupBox13: TGroupBox
          Left = 8
          Top = 78
          Width = 273
          Height = 121
          Caption = 'Meses'
          TabOrder = 5
          object Label11: TLabel
            Left = 96
            Top = 16
            Width = 36
            Height = 13
            Caption = 'mes(es)'
          end
          object RadioButton21: TRadioButton
            Left = 12
            Top = 16
            Width = 49
            Height = 17
            Caption = 'Cada'
            Checked = True
            TabOrder = 0
            TabStop = True
          end
          object Edit6: TEdit
            Left = 64
            Top = 12
            Width = 25
            Height = 21
            TabOrder = 1
            Text = '1'
          end
          object RadioButton22: TRadioButton
            Left = 12
            Top = 40
            Width = 77
            Height = 17
            Caption = 'Los meses:'
            TabOrder = 2
          end
          object CheckListBox5: TCheckListBox
            Left = 100
            Top = 44
            Width = 153
            Height = 65
            ItemHeight = 13
            Items.Strings = (
              'enero'
              'febrero'
              'marzo'
              'abril'
              'mayo'
              'junio'
              'julio'
              'agosto'
              'septiembre'
              'octubre'
              'noviembre'
              'diciembre')
            TabOrder = 3
          end
        end
      end
    end
  end
  object GroupBox14: TGroupBox
    Left = 165
    Top = 220
    Width = 289
    Height = 137
    Caption = 'Duraci'#243'n'
    TabOrder = 1
    object Label12: TLabel
      Left = 12
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Inicio:'
    end
    object DateTimePicker4: TDateTimePicker
      Left = 52
      Top = 20
      Width = 88
      Height = 21
      CalAlignment = dtaLeft
      Date = 37645.5371110648
      Time = 37645.5371110648
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 0
    end
    object MaskEdit7: TMaskEdit
      Left = 148
      Top = 20
      Width = 53
      Height = 21
      EditMask = '!90:00:00;1;_'
      MaxLength = 8
      TabOrder = 1
      Text = '  :  :  '
    end
    object RadioButton23: TRadioButton
      Left = 12
      Top = 52
      Width = 37
      Height = 17
      Caption = 'Fin'
      TabOrder = 2
    end
    object MaskEdit8: TMaskEdit
      Left = 148
      Top = 48
      Width = 53
      Height = 21
      EditMask = '!90:00:00;1;_'
      MaxLength = 8
      TabOrder = 3
      Text = '  :  :  '
    end
    object DateTimePicker5: TDateTimePicker
      Left = 52
      Top = 48
      Width = 88
      Height = 21
      CalAlignment = dtaLeft
      Date = 37645.5371110648
      Time = 37645.5371110648
      DateFormat = dfShort
      DateMode = dmComboBox
      Kind = dtkDate
      ParseInput = False
      TabOrder = 4
    end
    object RadioButton24: TRadioButton
      Left = 12
      Top = 80
      Width = 145
      Height = 17
      Caption = 'Cantidad de ejecuciones'
      TabOrder = 5
    end
    object RadioButton25: TRadioButton
      Left = 12
      Top = 108
      Width = 145
      Height = 17
      Caption = 'Sin fecha de finalizaci'#243'n'
      Checked = True
      TabOrder = 6
      TabStop = True
    end
    object Edit8: TEdit
      Left = 156
      Top = 76
      Width = 25
      Height = 21
      TabOrder = 7
      Text = '1'
    end
  end
  object RadioGroup1: TRadioGroup
    Left = 4
    Top = 4
    Width = 145
    Height = 353
    Caption = 'Tipo de recurrencia'
    ItemIndex = 0
    Items.Strings = (
      'Una vez'
      'Por tiempo'
      'Por d'#237'a'
      'Por semana'
      'Por mes')
    TabOrder = 2
    OnClick = RadioGroup1Click
  end
  object Button1: TButton
    Left = 292
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
  end
  object Button2: TButton
    Left = 376
    Top = 364
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
  end
end
