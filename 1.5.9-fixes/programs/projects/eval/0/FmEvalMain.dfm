object FormTester: TFormTester
  Left = 88
  Top = 116
  Width = 836
  Height = 683
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
  object lbVariables: TListBox
    Left = 707
    Top = 0
    Width = 121
    Height = 633
    Align = alRight
    ItemHeight = 13
    TabOrder = 0
    OnKeyDown = lbVariablesKeyDown
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 633
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    TabOrder = 1
    object edCode: TMemo
      Left = 0
      Top = 0
      Width = 707
      Height = 489
      Align = alTop
      Font.Charset = ANSI_CHARSET
      Font.Color = clNavy
      Font.Height = -16
      Font.Name = 'Courier New'
      Font.Style = []
      HideSelection = False
      ParentFont = False
      ScrollBars = ssVertical
      TabOrder = 0
      OnChange = edCodeChange
    end
    object edMatches: TMemo
      Left = 0
      Top = 489
      Width = 707
      Height = 144
      Align = alClient
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Courier New'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 1
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 633
    Width = 828
    Height = 23
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      828
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
    object Parse: TButton
      Left = 160
      Top = 2
      Width = 75
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '&Parse'
      TabOrder = 1
      OnClick = ParseClick
    end
    object Lexical: TButton
      Left = 80
      Top = 2
      Width = 75
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '&Lexical'
      TabOrder = 2
      OnClick = LexicalClick
    end
    object Evaluate: TButton
      Left = 240
      Top = 2
      Width = 75
      Height = 21
      Anchors = [akLeft, akBottom]
      Caption = '&Evaluate'
      TabOrder = 3
      OnClick = EvaluateClick
    end
    object Script: TButton
      Left = 320
      Top = 2
      Width = 75
      Height = 21
      Caption = '&Script'
      TabOrder = 4
      OnClick = ParseScriptClick
    end
  end
end
