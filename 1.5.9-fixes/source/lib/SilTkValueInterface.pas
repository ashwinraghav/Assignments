(*)
type
  _Type = <any type>;
  _Value = <any interface>;

const
  _Default = _Type(<default>);
(*)

  TSilValueTemplate = class(
    TSilValueItem,
    _Value )
  protected //_Value
    function GetValue: _Type;
    function Read(const Default: _Type): _Type;
    function Get(out Value: _Type): Boolean;
  protected
    function DoGet(out Value: _Type): TDataTypecastStatus; virtual; abstract;     
  end;


