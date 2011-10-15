object foTask: TfoTask
  Left = 359
  Top = 305
  Width = 464
  Height = 262
  ActiveControl = edTaskName
  BorderIcons = [biSystemMenu]
  Caption = 'Edit Job'
  Color = clBtnFace
  Constraints.MinHeight = 262
  Constraints.MinWidth = 464
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  DesignSize = (
    456
    235)
  PixelsPerInch = 96
  TextHeight = 13
  object pcTasks: TPageControl
    Left = 4
    Top = 4
    Width = 446
    Height = 194
    ActivePage = tsTask
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsTask: TTabSheet
      Caption = 'Job'
      DesignSize = (
        438
        166)
      object Label1: TLabel
        Left = 4
        Top = 8
        Width = 50
        Height = 13
        Caption = 'Job name:'
      end
      object Label2: TLabel
        Left = 4
        Top = 64
        Width = 57
        Height = 13
        Caption = 'Description:'
      end
      object Label3: TLabel
        Left = 4
        Top = 36
        Width = 49
        Height = 13
        Caption = 'Category:'
      end
      object Label5: TLabel
        Left = 4
        Top = 145
        Width = 43
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Created:'
      end
      object laJobCreated: TLabel
        Left = 52
        Top = 145
        Width = 105
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'dd/mm/yyyy hh:nn:ss'
      end
      object Label6: TLabel
        Left = 204
        Top = 145
        Width = 44
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'Modified:'
      end
      object laJobModified: TLabel
        Left = 252
        Top = 145
        Width = 105
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'dd/mm/yyyy hh:nn:ss'
      end
      object edTaskName: TEdit
        Left = 68
        Top = 4
        Width = 293
        Height = 21
        MaxLength = 64
        TabOrder = 0
        OnChange = DoChange
      end
      object cbTaskEnabled: TCheckBox
        Left = 368
        Top = 8
        Width = 97
        Height = 17
        Caption = 'Enabled'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object meTaskDescription: TMemo
        Left = 4
        Top = 84
        Width = 430
        Height = 54
        Anchors = [akLeft, akTop, akRight, akBottom]
        ScrollBars = ssVertical
        TabOrder = 4
        OnChange = DoChange
      end
      object cbTaskCategory: TComboBox
        Left = 68
        Top = 32
        Width = 233
        Height = 21
        ItemHeight = 13
        MaxLength = 64
        TabOrder = 2
        OnChange = DoChange
      end
      object btDeleteCategory: TButton
        Left = 304
        Top = 32
        Width = 57
        Height = 21
        Caption = 'Delete'
        TabOrder = 3
        OnClick = btDeleteCategoryClick
      end
    end
    object tsSteps: TTabSheet
      Caption = 'Steps'
      ImageIndex = 1
      DesignSize = (
        438
        166)
      object Label4: TLabel
        Left = 4
        Top = 141
        Width = 54
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = '&Move step:'
      end
      object gtSteps: TDBGrid
        Left = 4
        Top = 4
        Width = 430
        Height = 126
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsStep
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        PopupMenu = poTask
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = ANSI_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = btStepChangeClick
      end
      object btStepAdd: TButton
        Left = 189
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&New...'
        TabOrder = 1
        OnClick = btStepAddClick
      end
      object btStepChange: TButton
        Left = 273
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Edit...'
        TabOrder = 2
        OnClick = btStepChangeClick
      end
      object btStepDelete: TButton
        Left = 357
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete...'
        TabOrder = 3
        OnClick = btStepDeleteClick
      end
      object btStepUp: TButton
        Left = 64
        Top = 137
        Width = 25
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '5'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Marlett'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 4
        OnClick = btStepUpClick
      end
      object btStepDown: TButton
        Left = 96
        Top = 137
        Width = 25
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = '6'
        Font.Charset = SYMBOL_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Marlett'
        Font.Style = [fsBold]
        ParentFont = False
        TabOrder = 5
        OnClick = btStepDownClick
      end
    end
    object tsSchedules: TTabSheet
      Caption = 'Schedules'
      ImageIndex = 2
      DesignSize = (
        438
        166)
      object grSchedules: TDBGrid
        Left = 4
        Top = 4
        Width = 430
        Height = 126
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsSchedule
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        PopupMenu = poTask
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = ANSI_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        OnDblClick = btScheduleChangeClick
      end
      object btScheduleAdd: TButton
        Left = 189
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&New...'
        TabOrder = 1
        OnClick = btScheduleAddClick
      end
      object btScheduleChange: TButton
        Left = 273
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Edit...'
        TabOrder = 2
        OnClick = btScheduleChangeClick
      end
      object btScheduleDelete: TButton
        Left = 357
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete...'
        TabOrder = 3
        OnClick = btScheduleDeleteClick
      end
    end
    object tsNotifications: TTabSheet
      Caption = 'Notifications'
      ImageIndex = 4
      DesignSize = (
        438
        166)
      object gbNotEMail: TGroupBox
        Left = 4
        Top = 0
        Width = 429
        Height = 77
        Anchors = [akLeft, akTop, akRight]
        Caption = 'E-Mail'
        TabOrder = 0
        object Label7: TLabel
          Left = 12
          Top = 20
          Width = 64
          Height = 13
          Caption = 'Recipient list:'
        end
        object Label11: TLabel
          Left = 12
          Top = 48
          Width = 32
          Height = 13
          Caption = 'Event:'
        end
        object chNotEMail: TCheckBox
          Left = 360
          Top = 20
          Width = 61
          Height = 17
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = DoChange
        end
        object edNotEMail: TEdit
          Left = 84
          Top = 16
          Width = 265
          Height = 21
          TabOrder = 0
          OnChange = DoChange
        end
        object cbNotEMail: TComboBox
          Left = 84
          Top = 44
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = 'When the job fails'
          OnChange = DoChange
          Items.Strings = (
            'When the job fails'
            'When the job succeeds'
            'When the completes')
        end
      end
      object gbNotNetSend: TGroupBox
        Left = 4
        Top = 84
        Width = 429
        Height = 77
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Net Send'
        TabOrder = 1
        object Label8: TLabel
          Left = 12
          Top = 20
          Width = 42
          Height = 13
          Caption = 'Host list:'
        end
        object Label9: TLabel
          Left = 12
          Top = 48
          Width = 32
          Height = 13
          Caption = 'Event:'
        end
        object chNotNetSend: TCheckBox
          Left = 360
          Top = 20
          Width = 61
          Height = 17
          Caption = 'Enabled'
          TabOrder = 2
          OnClick = DoChange
        end
        object edNotNetSend: TEdit
          Left = 84
          Top = 16
          Width = 265
          Height = 21
          TabOrder = 0
          OnChange = DoChange
        end
        object cbNotNetSend: TComboBox
          Left = 84
          Top = 44
          Width = 145
          Height = 21
          Style = csDropDownList
          ItemHeight = 13
          ItemIndex = 0
          TabOrder = 1
          Text = 'When the job fails'
          OnChange = DoChange
          Items.Strings = (
            'When the job fails'
            'When the job succeeds'
            'When the completes')
        end
      end
    end
    object pcHistory: TTabSheet
      Caption = 'History'
      ImageIndex = 3
      DesignSize = (
        438
        166)
      object grHistory: TDBGrid
        Left = 4
        Top = 4
        Width = 430
        Height = 126
        Anchors = [akLeft, akTop, akRight, akBottom]
        DataSource = dsHistory
        Options = [dgTitles, dgColumnResize, dgColLines, dgRowLines, dgRowSelect, dgAlwaysShowSelection, dgConfirmDelete, dgCancelOnExit]
        PopupMenu = poTask
        ReadOnly = True
        TabOrder = 0
        TitleFont.Charset = ANSI_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object btHisDelete: TButton
        Left = 357
        Top = 137
        Width = 77
        Height = 25
        Anchors = [akRight, akBottom]
        Caption = '&Delete...'
        TabOrder = 2
        OnClick = btHisDeleteClick
      end
      object cbDetail: TCheckBox
        Left = 4
        Top = 141
        Width = 113
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = '&Show step details'
        TabOrder = 1
        OnClick = cbDetailClick
      end
    end
  end
  object btCancel: TButton
    Left = 374
    Top = 205
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
    OnClick = btCancelClick
  end
  object btAccept: TButton
    Left = 290
    Top = 205
    Width = 77
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
    OnClick = btAcceptClick
  end
  object dsStep: TDataSource
    AutoEdit = False
    DataSet = daTask.quStep
    Left = 12
    Top = 192
  end
  object dsSchedule: TDataSource
    AutoEdit = False
    DataSet = daTask.quSchedule
    Left = 60
    Top = 192
  end
  object dsHistory: TDataSource
    AutoEdit = False
    DataSet = daTask.quHistory
    Left = 112
    Top = 192
  end
  object poTask: TPopupMenu
    OnPopup = poTaskPopup
    Left = 40
    Top = 64
    object miTaskNew: TMenuItem
      Caption = 'New...'
    end
    object miTaskEdit: TMenuItem
      Caption = 'Edit...'
    end
    object miTaskDelete: TMenuItem
      Caption = 'Delete...'
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object miTaskMoveUp: TMenuItem
      Caption = 'Move up'
      OnClick = btStepUpClick
    end
    object miTaskMoveDown: TMenuItem
      Caption = 'Move down'
      OnClick = btStepDownClick
    end
  end
end
