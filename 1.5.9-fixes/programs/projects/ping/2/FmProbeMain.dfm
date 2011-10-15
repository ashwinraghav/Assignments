object FormPingMain: TFormPingMain
  Left = 201
  Top = 269
  Width = 572
  Height = 640
  Caption = 'FormPingMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  DesignSize = (
    564
    613)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 10
    Top = 12
    Width = 22
    Height = 13
    Caption = 'Host'
  end
  object Label2: TLabel
    Left = 126
    Top = 12
    Width = 53
    Height = 13
    Caption = 'Frecuencia'
  end
  object btAgregar: TButton
    Left = 248
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Nuevo'
    TabOrder = 0
    OnClick = btAgregarClick
  end
  object edTarget: TEdit
    Left = 8
    Top = 26
    Width = 109
    Height = 21
    TabOrder = 1
    Text = 'hcc2619'
  end
  object btEliminar: TButton
    Left = 326
    Top = 24
    Width = 75
    Height = 25
    Caption = '&Eliminar'
    TabOrder = 2
    OnClick = btEliminarClick
  end
  object lvTargets: TListView
    Left = 1
    Top = 56
    Width = 562
    Height = 556
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        AutoSize = True
        Caption = 'Target'
      end
      item
        AutoSize = True
        Caption = 'Secuencia'
      end
      item
        AutoSize = True
        Caption = 'Nombre'
      end
      item
        AutoSize = True
        Caption = 'Direccion'
      end
      item
        AutoSize = True
        Caption = 'Delay'
      end>
    ReadOnly = True
    RowSelect = True
    TabOrder = 3
    ViewStyle = vsReport
    OnChange = lvTargetsChange
    OnDeletion = lvTargetsDeletion
  end
  object edInterval: TEdit
    Left = 124
    Top = 25
    Width = 121
    Height = 21
    TabOrder = 4
    Text = '1000'
  end
  object btControl: TButton
    Left = 401
    Top = 24
    Width = 75
    Height = 25
    Caption = 'Control'
    TabOrder = 5
    OnClick = btControlClick
  end
  object tkTaskbar: TWinTaskbar
    Icon.Data = {
      0000010001002020100000000000E80200001600000028000000200000004000
      0000010004000000000080020000000000000000000000000000000000000000
      000000008000008000000080800080000000800080008080000080808000C0C0
      C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000FFFFF
      FFFFFFFFF000000000FFFF00080FFFFFFFFFFFF00B9B9B9B9B000008880FFFFF
      FFFFF009B9B9B9B9B0008888880FFFFFFFFF0B9B9B9B9B9B0088888880FFFFFF
      FFF0B9B9B9B9B900088888880FFFFFFFFF0B9B9B9B9B000888888880FFFFFFFF
      FF09B9B9B900088888888800FFFFFFFFF09B9B9B000888888888809B0BFFFFFF
      F0B9B90008888888888809B90FFFFFFF0B9B00088888888888809B9B90FFFFFF
      09B00888888888888800B9B9B0FFFFFF0000888888888888800B9B9B90FFFF00
      008888888888888800B9B9B9B0FFF00888888888888888009B9B9B9B90FFFF00
      0888888888888009B9B9B9B9B0FFFFFF00888888888888009B9B9B9B90FFFFFF
      000008888888888009B9B9B9B0FFFFFFF090000888888888009B9B9B0FFFFFFF
      F0B9B900888888888009B9B90FFFFFFFFF0B9B900088888888009B90FFFFFFFF
      FF09B9B9B0008888888009B0FFFFFFFFFFF09B9B9B9000088888000FFFFFFFFF
      FFFF09B9B9B9B9000088880FFFFFFFFFFFFFF00B9B9B9B9B9B000880FFFFFFFF
      FFFFFFF009B9B9B9B00F0000FFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000}
    OnDblClick = itmShowClick
    PopUpMenu = mnPopup
    ShowIcon = False
    HideApp = False
    DblClickRestores = False
    Left = 488
    Top = 16
  end
  object mnPopup: TPopupMenu
    Left = 520
    Top = 16
    object itmShow: TMenuItem
      Caption = '&Mostrar'
      OnClick = itmShowClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object itTerminate: TMenuItem
      Caption = '&Terminar'
      OnClick = itTerminateClick
    end
  end
end
