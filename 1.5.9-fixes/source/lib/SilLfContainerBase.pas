unit SilLfContainerBase;

{$INCLUDE Defines.inc}

interface

uses
  SilLiContainerTypes,
  SilLiContainer;

procedure DoQuickSort(const List: IContainerDynamic; Left: HItem = HNull; Right: HItem = HNull);
function DoSearch(const List: IContainerDynamic; Data: HData; var Index: HItem; Left: HItem = HNull; Right: HItem = HNull): Boolean;

implementation

procedure DoQuickSort(const List: IContainerDynamic; Left, Right: HItem);
var
  Items: IContainerItemsDynamic;
  Handler: ITypeHandler;
  I, J, Pivot: Integer;
begin
  Items := List.Items;
  Handler := List.Handler;

  if Items.Count = 0 then Exit;
  
  if Left = HNull then Left := Items.First;
  if Right = HNull then Right := Items.Last;
  
  repeat
    I := Left;
    J := Right;
    Pivot := (Left + Right) shr 1;
    repeat
      while Handler.Compare(List[I], List[Pivot]) < 0 do Inc(I);
      while Handler.Compare(List[J], List[Pivot]) > 0 do Dec(J);

      if I <= J then
      begin
        Items.Exchange(I, J);
        if Pivot = I then Pivot := J else
        if Pivot = J then Pivot := I;
        Inc(I);
        Dec(J);
      end;

    until I > J;
    if Left < J then DoQuickSort(List, Left, J);
    Left := I;
  until I >= Right;
end;

function DoSearch(const List: IContainerDynamic; Data: HData; var Index: HItem; Left: HItem = HNull; Right: HItem = HNull): Boolean;
var
  Items: IContainerItemsDynamic; 
  Handler: ITypeHandler;
  Item: HItem;
  C: Integer;
begin
  Result := False;
  Items := List.Items;

  if Items.Count > 0 then
  begin
    Handler := List.Handler;

    if Left = HNull then Left := Items.First;
    if Right = HNull then Right := Items.Last;

    while Left <= Right do
    begin
      Item := (Left + Right) shr 1;

      C := Handler.Compare(Data, List[Item]);

      if C > 0 then
        Left := Item + 1 else
      begin
        Right := Item - 1;
        if C = 0 then Result := True;
      end;

    end;
    Index := Left;
  end else
    Index := 0;
end;

end.
 