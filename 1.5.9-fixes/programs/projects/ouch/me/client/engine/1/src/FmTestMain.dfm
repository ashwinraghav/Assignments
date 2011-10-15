object Form1: TForm1
  Left = 234
  Top = 339
  Width = 870
  Height = 541
  Caption = 'desconectado'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 860
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 185
      Top = 11
      Width = 66
      Height = 13
      Alignment = taRightJustify
      Caption = 'Connect To:'
    end
    object edHost: TEdit
      Left = 254
      Top = 8
      Width = 109
      Height = 21
      TabOrder = 0
      Text = 'hcc5060'
    end
    object edPort: TEdit
      Left = 370
      Top = 8
      Width = 71
      Height = 21
      TabOrder = 1
      Text = '23360'
    end
    object btConnect: TButton
      Left = 10
      Top = 7
      Width = 83
      Height = 25
      Caption = 'Conectar'
      TabOrder = 2
      OnClick = btConnectClick
    end
    object btDisconnect: TButton
      Left = 94
      Top = 7
      Width = 83
      Height = 25
      Caption = 'Desconectar'
      TabOrder = 3
      OnClick = btDisconnectClick
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 41
    Width = 860
    Height = 72
    Align = alTop
    TabOrder = 1
    object Label5: TLabel
      Left = 403
      Top = 11
      Width = 57
      Height = 13
      Alignment = taRightJustify
      Caption = 'Password:'
    end
    object Label4: TLabel
      Left = 15
      Top = 11
      Width = 29
      Height = 13
      Alignment = taRightJustify
      Caption = 'User:'
    end
    object Label2: TLabel
      Left = 13
      Top = 45
      Width = 59
      Height = 13
      Alignment = taRightJustify
      Caption = 'SessionID:'
    end
    object Label3: TLabel
      Left = 342
      Top = 45
      Width = 42
      Height = 13
      Alignment = taRightJustify
      Caption = 'UserID:'
    end
    object edPassword: TEdit
      Left = 462
      Top = 8
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'password'
    end
    object edUser: TEdit
      Left = 46
      Top = 8
      Width = 339
      Height = 21
      TabOrder = 0
      Text = '{D7DD3D28-0B8A-432A-AFF7-4A0F8A00C43F} '
    end
    object edSessionID: TEdit
      Left = 76
      Top = 41
      Width = 259
      Height = 21
      Color = clWhite
      ReadOnly = True
      TabOrder = 4
    end
    object edUserID: TEdit
      Left = 388
      Top = 41
      Width = 275
      Height = 21
      Color = clWhite
      ReadOnly = True
      TabOrder = 5
    end
    object btLogin: TButton
      Left = 594
      Top = 7
      Width = 81
      Height = 25
      Caption = 'Logon'
      TabOrder = 2
      OnClick = btLoginClick
    end
    object btLogout: TButton
      Left = 677
      Top = 7
      Width = 83
      Height = 25
      Caption = 'Logout'
      TabOrder = 3
      OnClick = btLogoutClick
    end
    object btInfo: TButton
      Left = 761
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Info'
      TabOrder = 6
      OnClick = btInfoClick
    end
  end
  object Events: TMemo
    Left = 0
    Top = 448
    Width = 860
    Height = 64
    Align = alBottom
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object Objects: TTreeView
    Left = 7
    Top = 149
    Width = 146
    Height = 293
    Indent = 19
    TabOrder = 3
    OnChange = ObjectsChange
    OnDblClick = ObjectsDblClick
    OnDeletion = ObjectsDeletion
  end
  object btQueryGroups: TButton
    Left = 8
    Top = 120
    Width = 65
    Height = 25
    Caption = 'Grupos ...'
    TabOrder = 4
    OnClick = btQueryGroupsClick
  end
  object edGroupName: TEdit
    Left = 168
    Top = 120
    Width = 79
    Height = 21
    Color = clWhite
    TabOrder = 5
  end
  object edGroupID: TEdit
    Left = 255
    Top = 120
    Width = 297
    Height = 21
    Color = clWhite
    TabOrder = 6
  end
  object edGroupParent: TEdit
    Left = 559
    Top = 120
    Width = 297
    Height = 21
    Color = clWhite
    TabOrder = 7
  end
  object edOnlineUserName: TEdit
    Left = 168
    Top = 152
    Width = 79
    Height = 21
    Color = clWhite
    TabOrder = 8
  end
  object edOnlineUserID: TEdit
    Left = 255
    Top = 152
    Width = 297
    Height = 21
    Color = clWhite
    TabOrder = 9
  end
  object edOnlineAddress: TEdit
    Left = 559
    Top = 152
    Width = 130
    Height = 21
    Color = clWhite
    TabOrder = 10
  end
  object edOnlinePort: TEdit
    Left = 695
    Top = 152
    Width = 52
    Height = 21
    Color = clWhite
    TabOrder = 11
  end
  object btChangeStatus: TButton
    Left = 88
    Top = 120
    Width = 65
    Height = 25
    Caption = 'Status ...'
    TabOrder = 12
    OnClick = btChangeStatusClick
  end
  object btSendMessage: TButton
    Left = 753
    Top = 151
    Width = 65
    Height = 25
    Caption = 'Mensaje'
    TabOrder = 13
    OnClick = btSendMessageClick
  end
end
