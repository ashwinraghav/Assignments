object foMain: TfoMain
  Left = 356
  Top = 365
  BorderStyle = bsDialog
  Caption = 'Fecha/Hora'
  ClientHeight = 129
  ClientWidth = 266
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 12
    Width = 39
    Height = 13
    Caption = 'Archivo:'
  end
  object laName: TLabel
    Left = 60
    Top = 12
    Width = 36
    Height = 13
    Caption = 'laName'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 68
    Width = 26
    Height = 13
    Caption = 'Hora:'
  end
  object Label3: TLabel
    Left = 8
    Top = 40
    Width = 33
    Height = 13
    Caption = 'Fecha:'
  end
  object edDate: TEdit
    Left = 60
    Top = 36
    Width = 69
    Height = 21
    TabOrder = 0
    Text = '00/00/0000'
  end
  object edTime: TEdit
    Left = 60
    Top = 64
    Width = 69
    Height = 21
    TabOrder = 1
    Text = '00:00:00'
  end
  object btOk: TButton
    Left = 96
    Top = 96
    Width = 75
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 2
    OnClick = btOkClick
  end
  object btCancel: TButton
    Left = 184
    Top = 96
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 3
    OnClick = btCancelClick
  end
end
