object OuchDbHistory: TOuchDbHistory
  OldCreateOrder = False
  Left = 372
  Top = 106
  Height = 260
  Width = 215
  object sdData: TSilDataset
    Parameters = <>
    FieldDefs = <>
    Left = 32
    Top = 16
    object fdId: TIntegerField
      FieldName = 'id'
      Visible = False
    end
    object fdSndTime: TDateTimeField
      DisplayLabel = 'Enviado'
      FieldName = 'sndtime'
    end
    object fdRcvTime: TDateTimeField
      DisplayLabel = 'Recibido'
      FieldName = 'rcvtime'
    end
    object fdFrom: TGuidField
      DisplayLabel = 'De'
      FieldName = 'from'
      Visible = False
      Size = 38
    end
    object fdTo: TGuidField
      DisplayLabel = 'A'
      FieldName = 'to'
      Visible = False
      Size = 38
    end
    object fdKind: TStringField
      FieldName = 'kind'
      Visible = False
      Size = 2
    end
    object fdStatus: TStringField
      FieldName = 'status'
      Visible = False
      Size = 2
    end
    object fdText: TStringField
      DisplayLabel = 'Texto'
      FieldName = 'text'
    end
  end
end
