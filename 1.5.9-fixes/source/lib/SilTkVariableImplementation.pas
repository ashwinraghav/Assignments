(*)
type
  _Type = <any type>;
  _Class = <any class>;
  _Variable = <any interface>;
(*)

{$O+,W-,R-,S-,V-,Q-}

procedure TSilVariableTemplate.SetValue(const Value: _Type);
var
  Status: TDataTypecastStatus;
begin
  Status := DoSet(Value);
  if Status in tcFailed then
    raise DoCreateError(Status);
end;

procedure TSilVariableTemplate.Write(const Value: _Type);
var
  Status: TDataTypecastStatus;
begin
  Status := DoSet(Value);
  if Status in tcFailed then
    raise DoCreateError(Status);
end;

