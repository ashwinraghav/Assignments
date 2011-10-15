(*)
type
  _Type = <any type>;
  _ValueClass = <any class>;
  _Variable = <any interface>;
(*)

  TSilVariableTemplate = class(
    _ValueClass,
    _Variable )
  protected //_Variable
    procedure SetValue(const Value: _Type);
    procedure Write(const Value: _Type);
  protected
    function DoSet(const Value: _Type): TDataTypecastStatus; virtual; abstract;
  end;


