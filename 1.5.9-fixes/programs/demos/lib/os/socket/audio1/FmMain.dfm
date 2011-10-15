object foStreamer: TfoStreamer
  Left = 490
  Top = 437
  BorderStyle = bsDialog
  Caption = 'pcm streamer 0.3.1'
  ClientHeight = 237
  ClientWidth = 508
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label9: TLabel
    Left = 252
    Top = 204
    Width = 74
    Height = 13
    Caption = 'powered by SIL'
  end
  object Label10: TLabel
    Left = 252
    Top = 220
    Width = 129
    Height = 13
    Caption = 'marianop@intercom.com.ar'
  end
  object GroupBox1: TGroupBox
    Left = 252
    Top = 4
    Width = 253
    Height = 197
    Caption = 'conexion'
    TabOrder = 1
    object Label1: TLabel
      Left = 8
      Top = 20
      Width = 23
      Height = 13
      Caption = '&host:'
    end
    object edHost: TEdit
      Left = 36
      Top = 16
      Width = 121
      Height = 21
      TabOrder = 0
    end
    object btAdd: TButton
      Left = 164
      Top = 16
      Width = 75
      Height = 25
      Caption = '&agregar'
      TabOrder = 1
      OnClick = btAddClick
    end
    object lbHosts: TListBox
      Left = 36
      Top = 44
      Width = 121
      Height = 141
      ItemHeight = 13
      TabOrder = 2
    end
    object btRemove: TButton
      Left = 164
      Top = 48
      Width = 75
      Height = 25
      Caption = '&borrar'
      TabOrder = 3
      OnClick = btRemoveClick
    end
    object cbPropagar: TCheckBox
      Left = 164
      Top = 84
      Width = 79
      Height = 17
      Caption = 'Propagar'
      Enabled = False
      TabOrder = 4
    end
  end
  object GroupBox2: TGroupBox
    Left = 4
    Top = 4
    Width = 241
    Height = 153
    Caption = 'formato'
    TabOrder = 0
    object Label2: TLabel
      Left = 12
      Top = 20
      Width = 41
      Height = 13
      Caption = 'samples:'
    end
    object Label3: TLabel
      Left = 12
      Top = 44
      Width = 19
      Height = 13
      Caption = 'bits:'
    end
    object Label4: TLabel
      Left = 12
      Top = 68
      Width = 46
      Height = 13
      Caption = 'channels:'
    end
    object Label6: TLabel
      Left = 12
      Top = 92
      Width = 51
      Height = 13
      Caption = 'buffer size:'
    end
    object Label8: TLabel
      Left = 152
      Top = 92
      Width = 33
      Height = 13
      Caption = 'msecs:'
    end
    object cbSamples: TComboBox
      Left = 84
      Top = 16
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = DoCalcBufferSize
    end
    object cbBits: TComboBox
      Left = 84
      Top = 40
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 1
      OnChange = DoCalcBufferSize
    end
    object cbChannels: TComboBox
      Left = 84
      Top = 64
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 2
      OnChange = DoCalcBufferSize
    end
    object btCapture: TButton
      Left = 12
      Top = 120
      Width = 75
      Height = 25
      Caption = '&capturar'
      TabOrder = 5
      OnClick = btCaptureClick
    end
    object btStop: TButton
      Left = 96
      Top = 120
      Width = 75
      Height = 25
      Caption = '&stop'
      Enabled = False
      TabOrder = 6
      OnClick = btStopClick
    end
    object edBuffer: TEdit
      Left = 84
      Top = 88
      Width = 61
      Height = 21
      Enabled = False
      TabOrder = 3
    end
    object edMSecs: TEdit
      Left = 192
      Top = 88
      Width = 37
      Height = 21
      TabOrder = 4
      Text = '1000'
      OnChange = DoCalcBufferSize
    end
  end
  object GroupBox3: TGroupBox
    Left = 4
    Top = 160
    Width = 241
    Height = 73
    Caption = 'estado'
    TabOrder = 2
    object Label5: TLabel
      Left = 12
      Top = 24
      Width = 53
      Height = 13
      Caption = 'transmitido:'
    end
    object laTransmit: TLabel
      Left = 76
      Top = 24
      Width = 6
      Height = 13
      Caption = '0'
      Enabled = False
    end
    object Label7: TLabel
      Left = 12
      Top = 44
      Width = 40
      Height = 13
      Caption = 'recibido:'
    end
    object laReceive: TLabel
      Left = 76
      Top = 44
      Width = 6
      Height = 13
      Caption = '0'
      Enabled = False
    end
  end
  object btClose: TButton
    Left = 428
    Top = 208
    Width = 77
    Height = 25
    Caption = 'cerrar'
    TabOrder = 3
    OnClick = btCloseClick
  end
end
