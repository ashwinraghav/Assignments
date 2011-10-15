object FormTester: TFormTester
  Left = 6
  Top = 106
  Width = 694
  Height = 557
  ActiveControl = edCode
  Caption = 'FormTester'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 686
    Height = 507
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object edCode: TMemo
      Left = 0
      Top = 0
      Width = 686
      Height = 507
      Align = alClient
      Color = clWhite
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      HideSelection = False
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      WantTabs = True
      WordWrap = False
      OnChange = edCodeChange
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 507
    Width = 686
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      686
      23)
    object Matches: TButton
      Left = 0
      Top = 2
      Width = 75
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '&Matches'
      TabOrder = 0
      OnClick = MatchesClick
    end
    object Lexical: TButton
      Left = 80
      Top = 2
      Width = 75
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '&Lexical'
      TabOrder = 1
      OnClick = LexicalClick
    end
    object Parse: TButton
      Left = 160
      Top = 2
      Width = 75
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '&Request'
      TabOrder = 2
      OnClick = RequestClick
    end
    object Dump: TButton
      Left = 351
      Top = 2
      Width = 75
      Height = 21
      Caption = '&Dump'
      TabOrder = 3
      OnClick = DumpClick
    end
    object Modify: TButton
      Left = 429
      Top = 2
      Width = 75
      Height = 21
      Caption = 'Modi&fy'
      TabOrder = 4
      OnClick = ModifyClick
    end
    object Response: TButton
      Left = 240
      Top = 2
      Width = 75
      Height = 21
      Caption = 'Res&ponse'
      TabOrder = 5
      OnClick = ResponseClick
    end
  end
end
