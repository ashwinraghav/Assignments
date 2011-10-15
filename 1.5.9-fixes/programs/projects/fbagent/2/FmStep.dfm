object foStep: TfoStep
  Left = 316
  Top = 187
  BorderStyle = bsDialog
  Caption = 'Edit Job Step'
  ClientHeight = 362
  ClientWidth = 411
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  DesignSize = (
    411
    362)
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 4
    Top = 280
    Width = 86
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'On success action'
  end
  object Label3: TLabel
    Left = 4
    Top = 308
    Width = 79
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'On failure action'
  end
  object cbOnSuccess: TComboBox
    Left = 104
    Top = 276
    Width = 225
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 1
    OnChange = ChangeControl
    OnDropDown = OnActionDropDown
    Items.Strings = (
      'Goto the next step'
      'Quit the job reporting success'
      'Quit the job reporting failure')
  end
  object cbOnFailure: TComboBox
    Left = 104
    Top = 304
    Width = 225
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    TabOrder = 2
    OnChange = ChangeControl
    OnDropDown = OnActionDropDown
    Items.Strings = (
      'Goto the next step'
      'Quit the job reporting success'
      'Quit the job reporting failure')
  end
  object gbAction: TGroupBox
    Left = 4
    Top = 0
    Width = 401
    Height = 269
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Action'
    TabOrder = 0
    DesignSize = (
      401
      269)
    object Label4: TLabel
      Left = 8
      Top = 20
      Width = 55
      Height = 13
      Caption = 'Step name:'
    end
    object Label11: TLabel
      Left = 8
      Top = 44
      Width = 28
      Height = 13
      Caption = 'Type:'
    end
    object cbActionKind: TComboBox
      Left = 88
      Top = 40
      Width = 305
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 1
      Text = 'SQL Script'
      OnChange = ChangeControl
      OnClick = cbActionKindClick
      Items.Strings = (
        'SQL Script'
        'Operating System Command'
        'Database Backup')
    end
    object nbActionKind: TNotebook
      Left = 8
      Top = 68
      Width = 385
      Height = 196
      Anchors = [akLeft, akTop, akBottom]
      TabOrder = 2
      object TPage
        Left = 0
        Top = 0
        Caption = 'Sql'
        object paSQL: TPanel
          Left = 0
          Top = 0
          Width = 385
          Height = 196
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            385
            196)
          object Label7: TLabel
            Left = 0
            Top = 4
            Width = 50
            Height = 13
            Caption = 'Database:'
          end
          object Label8: TLabel
            Left = 0
            Top = 32
            Width = 26
            Height = 13
            Caption = 'User:'
          end
          object Label10: TLabel
            Left = 0
            Top = 56
            Width = 51
            Height = 13
            Caption = 'Command:'
          end
          object Label9: TLabel
            Left = 192
            Top = 32
            Width = 50
            Height = 13
            Caption = 'Password:'
          end
          object Label17: TLabel
            Left = 0
            Top = 176
            Width = 104
            Height = 13
            Caption = 'Transaction Isolation:'
          end
          object meSqlCommand: TMemo
            Left = 0
            Top = 72
            Width = 385
            Height = 95
            Anchors = [akLeft, akTop, akBottom]
            ScrollBars = ssVertical
            TabOrder = 3
            WantReturns = False
          end
          object edSqlConnection: TEdit
            Left = 80
            Top = 0
            Width = 305
            Height = 21
            TabOrder = 0
          end
          object edUserName: TEdit
            Left = 80
            Top = 28
            Width = 105
            Height = 21
            MaxLength = 30
            TabOrder = 1
          end
          object edUserPassword: TEdit
            Left = 244
            Top = 28
            Width = 141
            Height = 21
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Marlett'
            Font.Style = [fsBold]
            MaxLength = 30
            ParentFont = False
            PasswordChar = 'h'
            TabOrder = 2
          end
          object cbSqlIsolation: TComboBox
            Left = 112
            Top = 172
            Width = 205
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            ItemIndex = 0
            TabOrder = 4
            Text = 'Default'
            Items.Strings = (
              'Default'
              'Snapshot'
              'Read Committed'
              'Read-Only Table Stability'
              'Read-Write Table Stability')
          end
          object btSqlIsolation: TButton
            Left = 324
            Top = 172
            Width = 27
            Height = 21
            Caption = '...'
            Enabled = False
            TabOrder = 5
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'CommandLine'
        object paCommand: TPanel
          Left = 0
          Top = 0
          Width = 385
          Height = 196
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          DesignSize = (
            385
            196)
          object Label5: TLabel
            Left = 0
            Top = 0
            Width = 51
            Height = 13
            Caption = 'Command:'
          end
          object Label6: TLabel
            Left = 0
            Top = 178
            Width = 101
            Height = 13
            Anchors = [akLeft, akBottom]
            Caption = 'Successful exit code:'
          end
          object meCommandLine: TMemo
            Left = 0
            Top = 16
            Width = 385
            Height = 151
            Anchors = [akLeft, akTop, akBottom]
            ScrollBars = ssVertical
            TabOrder = 0
            WantReturns = False
          end
          object edSuccessCode: TEdit
            Left = 108
            Top = 174
            Width = 57
            Height = 21
            Anchors = [akLeft, akBottom]
            TabOrder = 1
          end
        end
      end
      object TPage
        Left = 0
        Top = 0
        Caption = 'Backup'
        object paBackup: TPanel
          Left = 0
          Top = 0
          Width = 385
          Height = 196
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 0
          object Label15: TLabel
            Left = 192
            Top = 32
            Width = 50
            Height = 13
            Caption = 'Password:'
          end
          object Label13: TLabel
            Left = 0
            Top = 4
            Width = 50
            Height = 13
            Caption = 'Database:'
          end
          object Label14: TLabel
            Left = 0
            Top = 32
            Width = 26
            Height = 13
            Caption = 'User:'
          end
          object Label16: TLabel
            Left = 0
            Top = 60
            Width = 20
            Height = 13
            Caption = 'File:'
          end
          object Label1: TLabel
            Left = 0
            Top = 176
            Width = 38
            Height = 13
            Caption = 'Log file:'
          end
          object Label12: TLabel
            Left = 0
            Top = 88
            Width = 50
            Height = 13
            Caption = 'File count:'
          end
          object edBakSqlConnection: TEdit
            Left = 80
            Top = 0
            Width = 305
            Height = 21
            TabOrder = 0
            OnChange = ChangeControl
          end
          object edBakUserName: TEdit
            Left = 80
            Top = 28
            Width = 105
            Height = 21
            MaxLength = 30
            TabOrder = 1
            OnChange = ChangeControl
          end
          object edBakUserPassword: TEdit
            Left = 244
            Top = 28
            Width = 141
            Height = 21
            AutoSize = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Marlett'
            Font.Style = [fsBold]
            MaxLength = 30
            ParentFont = False
            PasswordChar = 'h'
            TabOrder = 2
            OnChange = ChangeControl
          end
          object edBakFile: TEdit
            Left = 80
            Top = 56
            Width = 273
            Height = 21
            TabOrder = 3
            OnChange = ChangeControl
          end
          object cbBakExternalFilesAsTables: TCheckBox
            Left = 80
            Top = 108
            Width = 137
            Height = 17
            Caption = 'External files as tables'
            TabOrder = 7
            OnClick = ChangeControl
          end
          object cbBakGarbageCollection: TCheckBox
            Left = 80
            Top = 128
            Width = 117
            Height = 17
            Caption = 'Garbage collection'
            Checked = True
            State = cbChecked
            TabOrder = 8
            OnClick = ChangeControl
          end
          object cbBakIgnoreBadChecksums: TCheckBox
            Left = 80
            Top = 148
            Width = 137
            Height = 17
            Caption = 'Ignore bad checksums'
            TabOrder = 9
            OnClick = ChangeControl
          end
          object edBakLog: TEdit
            Left = 80
            Top = 172
            Width = 273
            Height = 21
            TabOrder = 12
            OnChange = ChangeControl
          end
          object cbBakTransportable: TCheckBox
            Left = 232
            Top = 128
            Width = 133
            Height = 17
            Caption = 'Transportable backup'
            Checked = True
            State = cbChecked
            TabOrder = 11
            OnClick = ChangeControl
          end
          object cbBakIgnoreLimboTransactions: TCheckBox
            Left = 232
            Top = 108
            Width = 153
            Height = 17
            Caption = 'Ignore transactions in limbo'
            TabOrder = 10
            OnClick = ChangeControl
          end
          object btBakFile: TButton
            Left = 358
            Top = 56
            Width = 27
            Height = 21
            Caption = '...'
            TabOrder = 4
            OnClick = btBakFileClick
          end
          object btBakLog: TButton
            Left = 358
            Top = 172
            Width = 27
            Height = 21
            Caption = '...'
            TabOrder = 13
            OnClick = btBakLogClick
          end
          object edBakCount: TEdit
            Left = 80
            Top = 84
            Width = 45
            Height = 21
            TabOrder = 5
            Text = '1'
            OnChange = ChangeControl
          end
          object udBakCount: TUpDown
            Left = 125
            Top = 84
            Width = 15
            Height = 21
            Associate = edBakCount
            Min = 1
            Position = 1
            TabOrder = 6
          end
        end
      end
    end
    object edName: TEdit
      Left = 88
      Top = 12
      Width = 305
      Height = 21
      MaxLength = 64
      TabOrder = 0
      OnChange = ChangeControl
    end
  end
  object btAccept: TButton
    Left = 244
    Top = 331
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 4
    OnClick = btAcceptClick
  end
  object btCancel: TButton
    Left = 328
    Top = 331
    Width = 77
    Height = 25
    Anchors = [akLeft, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 5
    OnClick = btCancelClick
  end
  object cbEnabled: TCheckBox
    Left = 336
    Top = 275
    Width = 69
    Height = 17
    Anchors = [akLeft, akBottom]
    Caption = 'Enabled'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = ChangeControl
  end
  object OpenDialog: TOpenDialog
    Left = 248
    Top = 16
  end
end
