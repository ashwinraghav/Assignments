object foMain: TfoMain
  Left = 328
  Top = 257
  Width = 796
  Height = 512
  Caption = 'Text File Viewer'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = meMain
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 788
    Height = 466
    Align = alClient
    BevelOuter = bvLowered
    TabOrder = 0
    object meText: TMemo
      Left = 1
      Top = 1
      Width = 786
      Height = 464
      Align = alClient
      BorderStyle = bsNone
      Ctl3D = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Courier New'
      Font.Style = []
      ParentCtl3D = False
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssHorizontal
      TabOrder = 0
      WordWrap = False
    end
  end
  object meMain: TMainMenu
    Left = 236
    Top = 88
    object miItem: TMenuItem
      Caption = '&File'
      object miOpen: TMenuItem
        Caption = '&Open...'
        ShortCut = 16463
        OnClick = miOpenClick
      end
      object miExit: TMenuItem
        Caption = '&Exit'
        ShortCut = 32856
        OnClick = miExitClick
      end
    end
  end
  object odFile: TOpenDialog
    DefaultExt = '*'
    Filter = 'Text Files|*.txt *.log|All files|*.*'
    FilterIndex = 2
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 292
    Top = 88
  end
end
