object foMain: TfoMain
  Left = 279
  Top = 234
  Width = 716
  Height = 508
  Caption = 'foMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object spMain: TSplitter
    Left = 0
    Top = 229
    Width = 708
    Height = 8
    Cursor = crVSplit
    Align = alTop
    AutoSnap = False
    Beveled = True
    MinSize = 100
    ResizeStyle = rsUpdate
  end
  inline faPanel1: TfaPanel
    Left = 0
    Top = 0
    Width = 708
    Height = 229
    Align = alTop
    TabOrder = 0
    inherited Splitter1: TSplitter
      Left = 225
      Height = 185
    end
    inherited paBar: TPanel
      Width = 708
      inherited edPath: TEdit
        Width = 661
      end
      inherited btGo: TButton
        Left = 664
        Default = True
      end
    end
    inherited sbStatus: TStatusBar
      Top = 210
      Width = 708
    end
    inherited lvFile: TVirtualStringTree
      Left = 228
      Width = 480
      Height = 185
    end
    inherited tvFolder: TVirtualStringTree
      Width = 225
      Height = 185
    end
  end
  inline faPanel2: TfaPanel
    Left = 0
    Top = 237
    Width = 708
    Height = 225
    Align = alClient
    TabOrder = 1
    inherited Splitter1: TSplitter
      Left = 225
      Height = 181
    end
    inherited paBar: TPanel
      Width = 708
      inherited edPath: TEdit
        Width = 661
      end
      inherited btGo: TButton
        Left = 664
      end
    end
    inherited sbStatus: TStatusBar
      Top = 206
      Width = 708
    end
    inherited lvFile: TVirtualStringTree
      Left = 228
      Width = 480
      Height = 181
    end
    inherited tvFolder: TVirtualStringTree
      Width = 225
      Height = 181
    end
  end
  object MainMenu: TMainMenu
    Left = 404
    Top = 144
    object Archivo1: TMenuItem
      Caption = 'Archivo'
      object Salir1: TMenuItem
        Caption = 'Salir'
      end
    end
  end
  object Timer1: TTimer
    Interval = 250
    OnTimer = Timer1Timer
    Left = 272
    Top = 60
  end
end
