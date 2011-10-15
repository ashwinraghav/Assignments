(*)
type
  _Type = <any type>;
  _Value = <any interface>;

const
  _Default = _Type(<default>);
(*)

{$O+,W-,R-,S-,V-,Q-}

function TSilValueTemplate.GetValue: _Type;
var
  Status: TDataTypecastStatus;
begin
  Status := DoGet(Result);
  if Status in tcFailed then
    raise DoCreateError(Status);
end;

function TSilValueTemplate.Read(const Default: _Type): _Type;
var
  Status: TDataTypecastStatus;
begin
  Status := DoGet(Result);
  if Status in tcFailed then
    Result := Default;
end;

function TSilValueTemplate.Get(out Value: _Type): Boolean;
begin
  Result := DoGet(Value) in tcSucceeded;
end;


