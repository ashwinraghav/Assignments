unit UmOuchContacts;

interface

uses
  Sil, SilClasses,
  UiOuch;

type
  TOuchAccounts = class(
    TSilInterfaceList,
    IOuchAccounts,
    IOuchAccountList)
  private
    FEngine: Pointer;
  private
    function DoGetEngine: IOuchEngine;
    procedure DoFireAdd(const Item: IUnknown; Data: Pointer);
    procedure DoFireRemove(const Item: IUnknown; Data: Pointer);
    procedure DoFireChanged(const Item: IUnknown; Data: Pointer);
  protected
    procedure FireAdd(Index: Integer); override;
    procedure FireDelete(Index: Integer); override;
  protected // IOuchAccounts
    function Find(const ID: TGUID; out Item: IOuchAccount): Boolean; 
    function Enumerate(var Enum: IEnumerator; out Item: IOuchAccount): Boolean; reintroduce; overload;
    function ToGuidList: TGuidArray;
  protected // IOuchAccountList
    procedure Clear; override;
    function Add(const Item: IOuchAccount): Integer; reintroduce;
    function Remove(const Item: IOuchAccount): Integer; reintroduce;
    procedure Changed(const Item: IOuchAccount);
  public
    constructor Create(const Engine: IOuchEngine);
    destructor Destroy; override;
  public
    property Engine: IOuchEngine read DoGetEngine;
  end;

implementation

{ TOuchAccounts }

constructor TOuchAccounts.Create(const Engine: IOuchEngine);
begin
  inherited Create(True);
  FEngine := Pointer(Engine);
end;

destructor TOuchAccounts.Destroy;
begin
  FEngine := nil;
  inherited;
end;

function TOuchAccounts.Enumerate(var Enum: IEnumerator; out Item: IOuchAccount): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TOuchAccounts.Find(const ID: TGUID; out Item: IOuchAccount): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Item) do
    Result := Sil.GUID.IsEqual(ID, Item.ID);
end;

function TOuchAccounts.ToGuidList: TGuidArray;
var
  Enum: IEnumerator;
  Item: IOuchAccount;
begin
  SetLength(Result, Self.Count);
  if Self.Count > 0 then
    while Enumerate(Enum, Item) do
      Result[Enum.Iteration] := Item.ID;    
end;

function TOuchAccounts.Add(const Item: IOuchAccount): Integer;
begin
  Result := inherited Add(Item);
end;

function TOuchAccounts.Remove(const Item: IOuchAccount): Integer;
begin
  Result := inherited Remove(Item);
end;

procedure TOuchAccounts.Changed(const Item: IOuchAccount);
begin
  Sil.Os.Thread.SyncCall(DoFireChanged, Item);
end;

procedure TOuchAccounts.FireAdd(Index: Integer);
begin
  if ValidIndex(Index) then Sil.Os.Thread.SyncCall(DoFireAdd, GetItem(Index));
end;

procedure TOuchAccounts.FireDelete(Index: Integer);
begin
  if ValidIndex(Index) then Sil.Os.Thread.SyncCall(DoFireRemove, GetItem(Index));
end;

procedure TOuchAccounts.DoFireAdd(const Item: IUnknown; Data: Pointer);
var
  Enum: IEnumerator;
  Sink: IOuchContactsEvents;
begin
  if HasConnections then
    with Events do
      while Enumerate(Enum, Sink, IOuchContactsEvents) do
        Sink.OnUserAdd(Item as IOuchAccount);
end;

procedure TOuchAccounts.DoFireChanged(const Item: IUnknown; Data: Pointer);
var
  Enum: IEnumerator;
  Sink: IOuchContactsEvents;
begin
  if HasConnections then
    with Events do
      while Enumerate(Enum, Sink, IOuchContactsEvents) do
        Sink.OnUserChanged(Item as IOuchAccount);
end;

procedure TOuchAccounts.DoFireRemove(const Item: IUnknown; Data: Pointer);
var
  Enum: IEnumerator;
  Sink: IOuchContactsEvents;
begin
  if HasConnections then
    with Events do
      while Enumerate(Enum, Sink, IOuchContactsEvents) do
        Sink.OnUserRemove(Item as IOuchAccount);
end;

procedure TOuchAccounts.Clear;
begin
  inherited;
end;

function TOuchAccounts.DoGetEngine: IOuchEngine;
begin
  Result := IOuchEngine(FEngine);
end;

end.
