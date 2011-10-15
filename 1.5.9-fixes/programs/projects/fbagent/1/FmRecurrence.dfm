object foRecurrence: TfoRecurrence
  Left = 300
  Top = 251
  BorderStyle = bsDialog
  Caption = 'Recurrencia'
  ClientHeight = 237
  ClientWidth = 502
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 4
    Top = 8
    Width = 92
    Height = 13
    Caption = 'Tipo de recurrencia'
  end
  object nbDetails: TNotebook
    Left = 4
    Top = 28
    Width = 281
    Height = 173
    TabOrder = 0
    object TPage
      Left = 0
      Top = 0
      Caption = 'Hourly'
      object GroupBox15: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 173
        Align = alClient
        Caption = 'Ejecuci'#243'n'
        TabOrder = 0
        object Label13: TLabel
          Left = 8
          Top = 24
          Width = 25
          Height = 13
          Caption = 'Cada'
        end
        object Label9: TLabel
          Left = 8
          Top = 56
          Width = 42
          Height = 13
          Caption = 'Los dias:'
        end
        object edIncEach: TEdit
          Left = 44
          Top = 20
          Width = 25
          Height = 21
          TabOrder = 0
          Text = '1'
        end
        object cbIncUnit: TComboBox
          Left = 76
          Top = 20
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = 'segundo(s)'
          Items.Strings = (
            'segundo(s)'
            'minuto(s)'
            'hora(s)'
            'd'#237'a(s)')
        end
        object paDays: TPanel
          Left = 56
          Top = 54
          Width = 117
          Height = 45
          BevelOuter = bvLowered
          TabOrder = 2
          object sbDaySun: TSpeedButton
            Left = 4
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'Dom'
          end
          object sbDayMon: TSpeedButton
            Left = 32
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 2
            Caption = 'Lun'
          end
          object sbDayTue: TSpeedButton
            Left = 60
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 3
            Caption = 'Mar'
          end
          object sbDayWed: TSpeedButton
            Left = 88
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 4
            Caption = 'Mie'
          end
          object sbDayThu: TSpeedButton
            Left = 4
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 5
            Caption = 'Jue'
          end
          object sbDayFri: TSpeedButton
            Left = 32
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 6
            Caption = 'Vie'
          end
          object sbDaySat: TSpeedButton
            Left = 60
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 7
            Caption = 'Sab'
          end
        end
        object btWekAll: TButton
          Left = 56
          Top = 104
          Width = 49
          Height = 21
          Caption = 'Todos'
          TabOrder = 3
          OnClick = btWekAllClick
        end
      end
    end
    object TPage
      Left = 0
      Top = 0
      Caption = 'Monthly'
      object gbDetails: TGroupBox
        Left = 0
        Top = 0
        Width = 281
        Height = 173
        Align = alClient
        Caption = 'Ejecuci'#243'n'
        TabOrder = 0
        object Label4: TLabel
          Left = 12
          Top = 84
          Width = 53
          Height = 13
          Caption = 'Los meses:'
        end
        object rbMonByDay: TRadioButton
          Left = 8
          Top = 24
          Width = 45
          Height = 17
          Caption = 'D'#237'a'
          Checked = True
          TabOrder = 0
          TabStop = True
        end
        object rbMonByPos: TRadioButton
          Left = 8
          Top = 52
          Width = 33
          Height = 17
          Caption = 'El'
          TabOrder = 1
        end
        object cbMonDayPos: TComboBox
          Left = 44
          Top = 48
          Width = 89
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 2
          Text = 'primer'
          Items.Strings = (
            'primer'
            'segundo'
            'tercer'
            'cuarto'
            #250'ltimo')
        end
        object cbMonWeekDay: TComboBox
          Left = 140
          Top = 48
          Width = 121
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 3
          Text = 'domingo'
          Items.Strings = (
            'domingo'
            'lunes'
            'martes'
            'mi'#233'rcoles'
            'jueves'
            'viernes'
            's'#225'bado')
        end
        object edMonDay: TEdit
          Left = 52
          Top = 20
          Width = 25
          Height = 21
          TabOrder = 4
          Text = '1'
        end
        object btMonAll: TButton
          Left = 76
          Top = 128
          Width = 49
          Height = 21
          Caption = 'Todos'
          TabOrder = 5
          OnClick = btMonAllClick
        end
        object paMonths: TPanel
          Left = 76
          Top = 78
          Width = 173
          Height = 45
          BevelOuter = bvLowered
          TabOrder = 6
          object SpeedButton1: TSpeedButton
            Left = 4
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 1
            Caption = 'Ene'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton2: TSpeedButton
            Left = 32
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 2
            Caption = 'Feb'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton3: TSpeedButton
            Left = 60
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 3
            Caption = 'Mar'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton4: TSpeedButton
            Left = 88
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 4
            Caption = 'Abr'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton5: TSpeedButton
            Left = 116
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 5
            Caption = 'May'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton6: TSpeedButton
            Left = 144
            Top = 4
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 6
            Caption = 'Jun'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton7: TSpeedButton
            Left = 4
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 7
            Caption = 'Jul'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton8: TSpeedButton
            Left = 32
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 8
            Caption = 'Ago'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton9: TSpeedButton
            Left = 60
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 9
            Caption = 'Sep'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton10: TSpeedButton
            Left = 88
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 10
            Caption = 'Oct'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton11: TSpeedButton
            Left = 116
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 11
            Caption = 'Nov'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
          object SpeedButton12: TSpeedButton
            Left = 144
            Top = 24
            Width = 25
            Height = 17
            AllowAllUp = True
            GroupIndex = 12
            Caption = 'Dic'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'MS Sans Serif'
            Font.Style = []
            ParentFont = False
          end
        end
      end
    end
  end
  object gbDuration: TGroupBox
    Left = 292
    Top = 4
    Width = 205
    Height = 49
    Caption = 'Horario'
    TabOrder = 1
    object Label12: TLabel
      Left = 8
      Top = 20
      Width = 28
      Height = 13
      Caption = 'Inicio:'
    end
    object Label2: TLabel
      Left = 108
      Top = 20
      Width = 17
      Height = 13
      Caption = 'Fin:'
    end
    object meStartTime: TMaskEdit
      Left = 44
      Top = 16
      Width = 53
      Height = 21
      EditMask = '!90:00:00;1;_'
      MaxLength = 8
      TabOrder = 0
      Text = '  :  :  '
    end
    object meEndTime: TMaskEdit
      Left = 132
      Top = 16
      Width = 53
      Height = 21
      EditMask = '!90:00:00;1;_'
      MaxLength = 8
      TabOrder = 1
      Text = '  :  :  '
    end
  end
  object btAccept: TButton
    Left = 336
    Top = 208
    Width = 77
    Height = 25
    Caption = '&Aceptar'
    Default = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ModalResult = 1
    ParentFont = False
    TabOrder = 2
  end
  object btCancel: TButton
    Left = 420
    Top = 208
    Width = 77
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object cbRecurrenceKind: TComboBox
    Left = 104
    Top = 4
    Width = 181
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 4
    Text = 'Diario'
    OnChange = cbRecurrenceKindChange
    Items.Strings = (
      'Diario'
      'Mensual')
  end
  object GroupBox1: TGroupBox
    Left = 292
    Top = 60
    Width = 205
    Height = 141
    Caption = 'Duraci'#243'n'
    TabOrder = 5
    object Label3: TLabel
      Left = 8
      Top = 24
      Width = 28
      Height = 13
      Caption = 'Inicio:'
    end
    object rbDurationNoEnd: TRadioButton
      Left = 8
      Top = 108
      Width = 145
      Height = 17
      Caption = 'Sin fecha de finalizaci'#243'n'
      Checked = True
      TabOrder = 0
      TabStop = True
    end
    object rbDurationCount: TRadioButton
      Left = 8
      Top = 80
      Width = 145
      Height = 17
      Caption = 'Cantidad de ejecuciones'
      TabOrder = 1
    end
    object rbDurationEnd: TRadioButton
      Left = 8
      Top = 52
      Width = 37
      Height = 17
      Caption = 'Fin'
      TabOrder = 2
    end
    object dtStartDate: TDateTimePicker
      Left = 56
      Top = 20
      Width = 88
      Height = 21
      Date = 37645.537111064800000000
      Time = 37645.537111064800000000
      TabOrder = 3
    end
    object dtEndDate: TDateTimePicker
      Left = 56
      Top = 48
      Width = 88
      Height = 21
      Date = 37645.537111064800000000
      Time = 37645.537111064800000000
      TabOrder = 4
      OnChange = dtEndDateChange
    end
    object edEndCount: TEdit
      Left = 152
      Top = 76
      Width = 25
      Height = 21
      TabOrder = 5
      Text = '1'
      OnChange = edEndCountChange
    end
  end
end
