object Form1: TForm1
  Left = 100
  Top = 373
  Width = 915
  Height = 492
  Caption = 'Test Firebird native api'
  Color = 13556698
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    907
    465)
  PixelsPerInch = 96
  TextHeight = 13
  object Connect: TButton
    Left = 0
    Top = 8
    Width = 75
    Height = 25
    Caption = '&1-Connect'
    TabOrder = 0
    OnClick = ConnectClick
  end
  object Disconnect: TButton
    Left = 75
    Top = 8
    Width = 75
    Height = 25
    Caption = '&1-Disconnect'
    Enabled = False
    TabOrder = 1
    OnClick = DisconnectClick
  end
  object Start: TButton
    Left = 0
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Start'
    TabOrder = 2
    OnClick = StartClick
  end
  object Commit: TButton
    Left = 75
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Commit'
    TabOrder = 3
    OnClick = CommitClick
  end
  object Rollback: TButton
    Left = 150
    Top = 33
    Width = 75
    Height = 25
    Caption = 'Rollback'
    TabOrder = 4
    OnClick = RollbackClick
  end
  object edSQL: TMemo
    Left = 200
    Top = 64
    Width = 701
    Height = 83
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'select * from RDB$RELATIONS'
      'where RDB$relation_name = ?')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 5
    WantTabs = True
    OnExit = edSQLExit
  end
  object Prepare: TButton
    Left = 0
    Top = 64
    Width = 75
    Height = 25
    Caption = '&2-Prepare'
    TabOrder = 6
    OnClick = PrepareClick
  end
  object Release: TButton
    Left = 75
    Top = 64
    Width = 75
    Height = 25
    Caption = '&2-Release'
    Enabled = False
    TabOrder = 7
    OnClick = ReleaseClick
  end
  object Display: TMemo
    Left = 424
    Top = 169
    Width = 477
    Height = 295
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 8
    WantTabs = True
    OnExit = edSQLExit
  end
  object Describe: TButton
    Left = 0
    Top = 90
    Width = 75
    Height = 25
    Caption = '&Describe'
    TabOrder = 9
    OnClick = DescribeClick
  end
  object Execute: TButton
    Left = 0
    Top = 120
    Width = 75
    Height = 25
    Caption = 'E&xecute'
    TabOrder = 10
    OnClick = ExecuteClick
  end
  object Schema: TButton
    Left = 0
    Top = 146
    Width = 75
    Height = 25
    Caption = '&Schema'
    Enabled = False
    TabOrder = 11
    OnClick = SchemaClick
  end
  object edUserName: TEdit
    Left = 160
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 12
    OnExit = edUserNameExit
  end
  object edPassword: TEdit
    Left = 280
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 13
    OnExit = edPasswordExit
  end
  object edDatabase: TEdit
    Left = 544
    Top = 8
    Width = 353
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 14
    OnExit = edDatabaseExit
  end
  object btBackup: TButton
    Left = 0
    Top = 240
    Width = 75
    Height = 25
    Caption = '&Backup'
    TabOrder = 15
    OnClick = btBackupClick
  end
  object edHostName: TEdit
    Left = 410
    Top = 8
    Width = 132
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 16
    OnExit = edHostNameExit
  end
  object edFileName: TEdit
    Left = 1
    Top = 267
    Width = 121
    Height = 21
    TabOrder = 17
    OnExit = edFileNameExit
  end
  object vaParams: TValueListEditor
    Left = 200
    Top = 169
    Width = 219
    Height = 293
    Anchors = [akLeft, akTop, akBottom]
    TabOrder = 18
    TitleCaptions.Strings = (
      'Parameter'
      'Value')
    ColWidths = (
      72
      141)
  end
  object edServerVersion: TEdit
    Left = 230
    Top = 35
    Width = 667
    Height = 21
    TabOrder = 19
  end
  object btGetInfo: TButton
    Left = 0
    Top = 288
    Width = 75
    Height = 25
    Caption = '&GetInfo'
    TabOrder = 20
    OnClick = btGetInfoClick
  end
  object clInfoItems: TCheckListBox
    Left = 0
    Top = 312
    Width = 198
    Height = 152
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 13
    Items.Strings = (
      'svr_db_info'
      'get_license'
      'get_license_mask'
      'get_config'
      'version'
      'server_version'
      'implementation'
      'capabilities'
      'user_dbpath'
      'get_env'
      'get_env_lock'
      'get_env_msg'
      'get_line'
      'get_to_eof'
      'timeout'
      'get_licensed_users'
      'limbo_trans'
      'running'
      'get_users')
    TabOrder = 21
  end
  object btTest: TButton
    Left = 0
    Top = 214
    Width = 75
    Height = 25
    Caption = 'Test Params'
    TabOrder = 22
  end
  object edStatementKind: TEdit
    Left = 200
    Top = 147
    Width = 217
    Height = 21
    TabOrder = 23
  end
end
