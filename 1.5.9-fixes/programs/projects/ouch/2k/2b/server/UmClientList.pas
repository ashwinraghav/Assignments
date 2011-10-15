unit UmClientList;

interface

uses
  Sil,
  SilClasses,

  UiClient;

type
  TClientList = class (TSilInterfaceList, IClients, IClientList)
  protected // IClients
    function GetItem(Index: Integer): IClient;
    function IndexOf(const Value: IUnknown): Integer; reintroduce; 
    function First: IClient;
    function Last: IClient;
    function Enumerate(var Enum: IEnumerator; out Item: IClient): Boolean; reintroduce; 
    function Find(const Id: TGuid; out Value: IClient): Boolean;
  protected // IClientList
    procedure SetItem(Index: Integer; const Value: IClient);
    function Add(const Value: IClient): Integer; reintroduce;
    function Remove(const Value: IClient): Integer; reintroduce;
    property Items[Index: Integer]: IClient read GetItem write SetItem; default;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TClientList }

constructor TClientList.Create;
begin
  inherited Create(true);
end;

destructor TClientList.Destroy;
begin
  inherited;
end;

function TClientList.Add(const Value: IClient): Integer;
begin
  Result := inherited Add(Value);
end;

function TClientList.Enumerate(var Enum: IEnumerator; out Item: IClient): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TClientList.First: IClient;
begin
  Result := IClient(inherited First);
end;

function TClientList.GetItem(Index: Integer): IClient;
begin
  Result := IClient(inherited GetItem(Index));
end;

function TClientList.IndexOf(const Value: IInterface): Integer;
begin
  Result := inherited IndexOf(Value);
end;

function TClientList.Last: IClient;
begin
  Result := IClient(inherited Last);
end;

function TClientList.Remove(const Value: IClient): Integer;
begin
  Result := inherited Remove(Value);
end;

procedure TClientList.SetItem(Index: Integer; const Value: IClient);
begin
  inherited SetItem(Index, Value);
end;

function TClientList.Find(const Id: TGuid; out Value: IClient): Boolean;
var
  Enum: IEnumerator;
begin
  while Enumerate(Enum, Value) do
    if Guid.Compare(Id, Value.Id) = 0 then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;

end.
 