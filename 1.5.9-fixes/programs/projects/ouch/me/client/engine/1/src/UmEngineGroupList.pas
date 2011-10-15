unit UmEngineGroupList;

interface

uses
  Sil,
  SilClasses,
  UiOuchProtocol,
  UiOuchEngine;

type
  TEngineGroupList = class(
    TInterfaceList,
    IOuchGroups,
    IOuchGroupList )
  protected // IOuchGroups
    function GetItem(Index: Integer): IOuchGroupData;
    function Get(const ID: TGUID): IOuchGroupData;
    function Find(const GroupID: TGUID; const IID: TGUID; out Group): Boolean;
    function ToArray: TGuidArray;
  protected // IOuchGroupList
    function Add(const Item: IOuchGroupData): Integer; reintroduce;
    function Remove(const Item: IOuchGroupData): Integer; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
  public
    constructor Create(const Groups: IOuchGroups);
  end;

implementation

uses
  UdEngineGroupList;

{ TEngineGroupList }

constructor TEngineGroupList.Create(const Groups: IOuchGroups);
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  inherited Create;
  if Assigned(Groups) and (Groups.Count > 0) then
    while Groups.Enumerate(Enum, Item) do
      Add(Item as IOuchGroupData);
end;

function TEngineGroupList.Find(const GroupID: TGUID; const IID: TGUID; out Group): Boolean;
var
  Enum: IEnumerator;
  Item: IOuchGroupData;
begin
  Result := False;
  while Enumerate(Enum, Item) do
  begin
    Result := Sil.GUID.IsEqual(Item.ID, GroupID) and (Item.QueryInterface(IID, Group) = 0);
    if Result then Break;
  end;
end;

function TEngineGroupList.ToArray: TGuidArray;
var
  Enum: IEnumerator;
  Item: IOuchGroupData;
begin
  if Count > 0 then
  begin
    SetLength(Result, Count);
    while Enumerate(Enum, Item) do
      Result[Enum.Iteration] := Item.ID;
  end;
end;

function TEngineGroupList.Get(const ID: TGUID): IOuchGroupData;
begin
  if not Find(ID, IOuchGroupData, Result) then
    raise Sil.Error.Create(SErrorItemNotFound)
end;

function TEngineGroupList.GetItem(Index: Integer): IOuchGroupData;
begin
  Result := inherited GetItem(Index) as IOuchGroupData;
end;

function TEngineGroupList.Add(const Item: IOuchGroupData): Integer;
begin
  Result := inherited Add(Item);
end;

function TEngineGroupList.Remove(const Item: IOuchGroupData): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TEngineGroupList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

end.
