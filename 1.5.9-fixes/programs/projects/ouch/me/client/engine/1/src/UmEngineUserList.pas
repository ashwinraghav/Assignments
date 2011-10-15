unit UmEngineUserList;

interface

uses
  Sil,
  SilClasses,
  UiOuchProtocol,
  UiOuchEngine;

type
  TEngineUserList = class(
    TInterfaceList,
    IOuchUsers,
    IOuchUserList )
  protected // IOuchUsers
    function GetItem(Index: Integer): IOuchUserData;
    function Get(const ID: TGUID): IOuchUserData;
    function Find(const GroupID: TGUID; const IID: TGUID; out Group): Boolean;
    function ToArray: TGuidArray;
  protected // IOuchUserList
    function Add(const Item: IOuchUserData): Integer; reintroduce;
    function Remove(const Item: IOuchUserData): Integer; reintroduce;
    procedure Delete(Index: Integer); reintroduce;
  public
    constructor Create(const Users: IOuchUsers); 
  end;

implementation

uses
  UdEngineGroupList, SilLkList;

{ TEngineUserList }

constructor TEngineUserList.Create(const Users: IOuchUsers);
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  inherited Create;
  if Assigned(Users) and (Users.Count > 0) then
    while Users.Enumerate(Enum, Item) do
      Add(Item as IOuchUserData);
end;

function TEngineUserList.Find(const GroupID: TGUID; const IID: TGUID; out Group): Boolean;
var
  Enum: IEnumerator;
  Item: IOuchUserData;
begin
  Result := False;
  while Enumerate(Enum, Item) do
  begin
    Result := Sil.GUID.IsEqual(Item.ID, GroupID) and (Item.QueryInterface(IID, Group) = 0);
    if Result then Break;
  end;
end;

function TEngineUserList.ToArray: TGuidArray;
var
  Enum: IEnumerator;
  Item: IOuchUserData;
begin
  if Count > 0 then
  begin
    SetLength(Result, Count);
    while Enumerate(Enum, Item) do
      Result[Enum.Iteration] := Item.ID;
  end;
end;

function TEngineUserList.Get(const ID: TGUID): IOuchUserData;
begin
  if not Find(ID, IOuchUserData, Result) then
    raise Sil.Error.Create(SErrorItemNotFound)
end;

function TEngineUserList.GetItem(Index: Integer): IOuchUserData;
begin
  Result := inherited GetItem(Index) as IOuchUserData;
end;

function TEngineUserList.Add(const Item: IOuchUserData): Integer;
begin
  Result := inherited Add(Item);
end;

function TEngineUserList.Remove(const Item: IOuchUserData): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TEngineUserList.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

end.
 