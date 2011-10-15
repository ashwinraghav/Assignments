(*)
type
  _Type = <any type>;
  _Value = <any interface>;
(*)

  IVariableTemplate = interface (_Value)
    procedure SetValue(const Value: _Type);
    procedure Write(const Value: _Type);
    property Value: _Type read GetValue write SetValue;
  end;
