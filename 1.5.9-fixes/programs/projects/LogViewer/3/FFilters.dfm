object FormFilters: TFormFilters
  Left = 391
  Top = 119
  Width = 355
  Height = 625
  ActiveControl = lvGrupos
  Caption = 'Filtros'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    347
    598)
  PixelsPerInch = 96
  TextHeight = 13
  object gbListaGrupos: TGroupBox
    Left = 8
    Top = 7
    Width = 330
    Height = 168
    Anchors = [akLeft, akTop, akRight]
    Caption = ' Grupos '
    TabOrder = 0
    object Panel1: TPanel
      Left = 2
      Top = 15
      Width = 326
      Height = 151
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 6
      TabOrder = 0
      object lvGrupos: TListView
        Left = 6
        Top = 37
        Width = 314
        Height = 108
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'Nombre'
            Width = 130
          end>
        ReadOnly = True
        SortType = stData
        TabOrder = 1
        ViewStyle = vsReport
        OnChange = lvGruposChange
        OnClick = lvGruposClick
        OnCompare = lvGruposCompare
      end
      object Panel5: TPanel
        Left = 6
        Top = 6
        Width = 314
        Height = 31
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          314
          31)
        object btDelete: TBitBtn
          Left = 30
          Top = 0
          Width = 30
          Height = 28
          TabOrder = 1
          OnClick = btDeleteClick
          Glyph.Data = {
            B6020000424DB602000000000000760000002800000030000000180000000100
            0400000000004002000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888FFFFFFFFF888
            88888888888000000000888888888888888777777777F888888888888880F777
            7770888888888888888788F8F8F78F8888888888880FF0807077088888888888
            887F87F7F7F87F8888888888880FF0808077088888888888887887F7F7F87FF8
            8888888880FFF080707770888888888887F88787F78F87F88888888880FF7780
            870770888888888887F87F87F87F87F88888888880FF08807807708888888888
            87F87F87F87F87F8F888888880FF0880870770808888888887F87F87F87F87F7
            8888888800FF0880780770088888888877F87F87F87F87788888888880FF7780
            870770888888888887F878F7F87887F88888888880FFF0807077708888888888
            878F87F7F7F8878888888888880FF0808077088888888888887F87F7F7F87F88
            88888888880FF0807077088888888888887F87F7F7F87F888888888888000000
            00000888888888888877777777777F8888888888880F88877777088888888888
            887FFFFFFFFF7F88888888888800000000000888888888888877777777777888
            88888888888880777088888888888888888887FFF78888888888888888888000
            0088888888888888888887777788888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888}
          Layout = blGlyphBottom
          NumGlyphs = 2
        end
        object btNew: TBitBtn
          Left = 0
          Top = 0
          Width = 30
          Height = 28
          TabOrder = 0
          OnClick = btNewClick
          Glyph.Data = {
            B6020000424DB602000000000000760000002800000030000000180000000100
            0400000000004002000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            888F888888888FFF8888888888E888888888BBB888888888887FF88888FF777F
            FFF8888888EE88888BBBB3BBBB8888FFFF778F8887777F7777F88EEEEEECE888
            8B3BB3BB3B888777777878F887F77F7787F88ECCCCCCCE888BB3B3B3BB8887F8
            8888878F877F787877FF8ECCCCCCCCE8BBBB333BBBB887F88888887F77778887
            777F8ECCCCCCCCE8B333333333B887F8888888787FFFF888FF7F8ECCCCCCCE88
            BBBB333BBBB887FFFFFF878877778F8777788EEEEEECE8888BB3B3B3BB888777
            777F788887787F7F77F8888888EE88888B3BB3BB3B8888888877888887F77F77
            F7F8888888E888888BBBB3BBBB8888888878888887777F777788888888888888
            8888BBB888888888888888888888777888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888}
          Layout = blGlyphBottom
          NumGlyphs = 2
        end
        object btUp: TBitBtn
          Left = 254
          Top = 0
          Width = 30
          Height = 28
          Anchors = [akTop, akRight]
          TabOrder = 2
          OnClick = btUpClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888FFFFF8888888888FFFFF8888888888877777F8888888888F999F88
            8888888887F887F8888888888F999F888888888887F887F8888888888F999F88
            8888888887F887F8888888888F999F888888888887F887F8888888888F999F88
            8888888887F887FFFF8888FFFF999FFFF8888877778887777F8888F999999999
            F888887FF88888887F8888FF9999999FF8888877FF8888877888888FF99999FF
            888888877FF8887788888888FF999FF88888888877FF8778888888888FF9FF88
            88888888877F77888888888888FFF88888888888887778888888888888888888
            8888888888888888888888888888888888888888888888888888}
          Layout = blGlyphBottom
          NumGlyphs = 2
        end
        object btDown: TBitBtn
          Left = 284
          Top = 0
          Width = 30
          Height = 28
          Anchors = [akTop, akRight]
          Cancel = True
          TabOrder = 3
          OnClick = btDownClick
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888FFF888888888888FFF888
            8888888888777FF8888888888FF9FF8888888888877877FF88888888FF999FF8
            888888887788877FF888888FF99999FF8888888778888877FF8888FF9999999F
            F8888877888888877F8888F999999999F888887FFFF888FF7F8888FFFF999FFF
            F888887777F88777788888888F999F888888888887F887F8888888888F999F88
            8888888887F887F8888888888F999F888888888887F887F8888888888F999F88
            8888888887F887F8888888888F999F888888888887FFF7F8888888888FFFFF88
            8888888887777788888888888888888888888888888888888888}
          Layout = blGlyphBottom
          NumGlyphs = 2
        end
      end
    end
  end
  object gbGrupo: TGroupBox
    Left = 8
    Top = 177
    Width = 330
    Height = 377
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 1
    object Panel4: TPanel
      Left = 2
      Top = 15
      Width = 326
      Height = 360
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 6
      Caption = 'Panel4'
      TabOrder = 0
      object gbAcciones: TGroupBox
        Left = 6
        Top = 36
        Width = 314
        Height = 118
        Align = alClient
        Caption = ' Acciones '
        TabOrder = 1
        object Panel2: TPanel
          Left = 2
          Top = 15
          Width = 310
          Height = 101
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 6
          TabOrder = 1
          object edActions: TMemo
            Left = 6
            Top = 6
            Width = 298
            Height = 89
            Align = alClient
            TabOrder = 0
            OnChange = EditGroupInfo
          end
        end
        object chkStopEvaluation: TCheckBox
          Left = 188
          Top = 1
          Width = 116
          Height = 17
          Caption = 'Detener evaluaci'#243'n'
          TabOrder = 0
          OnClick = ChageGropInfo
        end
      end
      object gbGroupExcluded: TGroupBox
        Left = 6
        Top = 254
        Width = 314
        Height = 100
        Align = alBottom
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 3
        object Panel3: TPanel
          Left = 2
          Top = 15
          Width = 310
          Height = 83
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 6
          TabOrder = 1
          object edGroupDontContains: TMemo
            Left = 6
            Top = 6
            Width = 298
            Height = 71
            Align = alClient
            TabOrder = 0
            OnChange = EditGroupInfo
          end
        end
        object chkMustExclude: TCheckBox
          Left = 8
          Top = 1
          Width = 110
          Height = 17
          Caption = 'No debe contener'
          TabOrder = 0
          OnClick = ChageGropInfo
        end
      end
      object pnGroupProperties: TPanel
        Left = 6
        Top = 6
        Width = 314
        Height = 30
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        Visible = False
        DesignSize = (
          314
          30)
        object Label1: TLabel
          Left = 1
          Top = 5
          Width = 40
          Height = 13
          Caption = 'Nombre:'
        end
        object edGroupName: TEdit
          Left = 45
          Top = 2
          Width = 121
          Height = 21
          TabOrder = 0
        end
        object btOk: TBitBtn
          Left = 254
          Top = 0
          Width = 30
          Height = 28
          Anchors = [akTop, akRight]
          Default = True
          TabOrder = 1
          OnClick = btOkClick
          Glyph.Data = {
            B6020000424DB602000000000000760000002800000030000000180000000100
            0400000000004002000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            88888888888888888888888888888888888888888888888888888FFFF8888888
            888888888888AAAA888888888888888888887777F8888888888888888888A22A
            888888888888888888887F87F888888888888888888AA22A8888888888888888
            88877887FF88888888888888888A222AA8888888888888888887F8877F888888
            8888888888AA2222A888888888888888887788887F888888888888888AA22222
            A888888888888888877888887FF8888888888888AA222222AA8888888888888F
            77888F8877F88888888888AAA222A2222A88888888888877788F7FF887FF8888
            88888AA222AAAA222AA88888888887788F7777F8877F888888888A22AAA88A22
            22A88888888887FF777887FF887FF88888888AAAA8888AA222AA888888888777
            7888877FF877FF8888888888888888AA222AA8888888888888888877FF877FF8
            888888888888888AA222AA8888888888888888877FF877FF8888888888888888
            AA222AA8888888888888888877FFF77FF8888888888888888AAA22AA88888888
            888888888777FF77FFF8888888888888888AA22AAA8888888888888888877FF7
            77F88888888888888888AAA22A888888888888888888777FF7F8888888888888
            888888AAAA888888888888888888887777888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888}
          Layout = blGlyphBottom
          NumGlyphs = 2
        end
        object btCancel: TBitBtn
          Left = 284
          Top = 0
          Width = 30
          Height = 28
          Anchors = [akTop, akRight]
          Cancel = True
          TabOrder = 2
          OnClick = btCancelClick
          Glyph.Data = {
            B6020000424DB602000000000000760000002800000030000000180000000100
            0400000000004002000000000000000000001000000000000000000000000000
            8000008000000080800080000000800080008080000080808000C0C0C0000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            88888888888888888888888888888888888888888888888888FFFF8888888FFF
            8888888889999888888899988888888887777FF88888777FF888888899119988
            8889919988888888778877F888877877F8888888911119888899111988888888
            7FF887FF88778887F888888899111998899111198888888877F8877FF7788887
            F888888889111199991111998888888887FF8877778888778888888889911119
            9111199888888888877FF8877888F77888888888889911111119998888888888
            8877FF88888777888888888888899111119988888888888888877FF888778888
            88888888888899111998888888888888888877F8877F88888888888888889911
            199888888888888888887788877FF88888888888888991111199888888888888
            88877888F877FF88888888888899111911199888888888888F778887FF877FF8
            88888888999111999111998888888888777888777FF877FF8888888891111198
            99111998888888887F88887F77F8877F88888888911119988911119888888888
            7FF8877887FF887F8888888899119988899111988888888877FF7788877FFF7F
            8888888889999888889999988888888887777888887777788888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888888888888888
            8888888888888888888888888888888888888888888888888888}
          Layout = blGlyphBottom
          NumGlyphs = 2
        end
      end
      object gbGroupIncluded: TGroupBox
        Left = 6
        Top = 154
        Width = 314
        Height = 100
        Align = alBottom
        TabOrder = 2
        object Panel6: TPanel
          Left = 2
          Top = 15
          Width = 310
          Height = 83
          Align = alClient
          BevelOuter = bvNone
          BorderWidth = 6
          TabOrder = 1
          object edGroupContains: TMemo
            Left = 6
            Top = 6
            Width = 298
            Height = 71
            Align = alClient
            TabOrder = 0
            OnChange = EditGroupInfo
          end
        end
        object chkMustInclude: TCheckBox
          Left = 8
          Top = 1
          Width = 97
          Height = 17
          Caption = 'Debe contener'
          TabOrder = 0
          OnClick = ChageGropInfo
        end
      end
    end
  end
  object Button1: TButton
    Left = 80
    Top = 566
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Caption = 'Aceptar'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 191
    Top = 566
    Width = 75
    Height = 25
    Anchors = [akBottom]
    Cancel = True
    Caption = 'Cancelar'
    ModalResult = 2
    TabOrder = 3
  end
  object PopupMenu1: TPopupMenu
    Left = 149
    Top = 23
    object Nuevo1: TMenuItem
      Caption = 'Nuevo'
      ShortCut = 16462
      OnClick = btNewClick
    end
  end
end
