object Form1: TForm1
  Left = 366
  Top = 332
  Width = 674
  Height = 359
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Memo1: TMemo
    Left = 200
    Top = 20
    Width = 293
    Height = 281
    Lines.Strings = (
      'Memo1')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button2: TButton
    Left = 4
    Top = 284
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 4
    Top = 224
    Width = 153
    Height = 21
    TabOrder = 2
    Text = 'Edit1'
  end
  object Button7: TButton
    Left = 520
    Top = 16
    Width = 75
    Height = 25
    Caption = 'Button7'
    TabOrder = 3
    OnClick = Button7Click
  end
  object CheckBox1: TCheckBox
    Left = 20
    Top = 176
    Width = 97
    Height = 17
    Caption = 'first run'
    TabOrder = 4
  end
  object Edit2: TEdit
    Left = 4
    Top = 252
    Width = 153
    Height = 21
    TabOrder = 5
    Text = 'Edit2'
  end
  object Button4: TButton
    Left = 12
    Top = 48
    Width = 75
    Height = 25
    Caption = 'Button4'
    TabOrder = 6
    OnClick = Button4Click
  end
  object coAgent: TIBDatabase
    DatabaseName = 'agent'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey'
      'lc_ctype=ISO8859_1')
    LoginPrompt = False
    DefaultTransaction = trRunStat
    AllowStreamedConnected = False
    Left = 120
    Top = 44
  end
  object doRunStatUpd: TIBStoredProc
    Database = coAgent
    Transaction = trRunStat
    StoredProcName = 'DO_RUN_UPD'
    Left = 120
    Top = 96
    ParamData = <
      item
        DataType = ftInteger
        Name = 'RUN_ID'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'LAST_RUN'
        ParamType = ptInput
      end
      item
        DataType = ftDateTime
        Name = 'NEXT_RUN'
        ParamType = ptInput
      end
      item
        DataType = ftInteger
        Name = 'REPEAT'
        ParamType = ptInput
      end>
  end
  object trRunStat: TIBTransaction
    DefaultDatabase = coAgent
    Left = 40
    Top = 96
  end
  object qryRunStat: TIBQuery
    Database = coAgent
    Transaction = trRunStat
    SQL.Strings = (
      'select * from qry_run')
    Left = 120
    Top = 148
  end
end
