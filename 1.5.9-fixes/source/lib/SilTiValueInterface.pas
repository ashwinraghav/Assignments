(*)
type
  _Type = <any type>;

const
  _Default = _Type(<default>);
(*)

  IValueTemplate = interface
    function GetValue: _Type;
    function Read(const Default: _Type {$IFNDEF _NO_DEFAULT} = _Default {$ENDIF}): _Type;
    function Get(out Value: _Type): Boolean; 
    property Value: _Type read GetValue;
  end;
