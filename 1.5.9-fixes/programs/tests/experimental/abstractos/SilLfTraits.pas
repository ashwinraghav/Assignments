unit SilLfTraits;

interface

uses
  SilLiTraits;

function Adopt(Reference: PUnknown; const Value: IUnknown): Boolean;

implementation

uses
  SilLtReference;

function Adopt(Reference: PUnknown; const Value: IUnknown): Boolean;
begin
  if Assigned(Reference) and Ref.Supports(Reference^, IAdoptionTrait) then Reference._Release;
  Pointer(Reference^) := Pointer(Value); 
  if Assigned(Reference) and Ref.Supports(Reference^, IAdoptionTrait) then Reference._AddRef;
end;

end.
 