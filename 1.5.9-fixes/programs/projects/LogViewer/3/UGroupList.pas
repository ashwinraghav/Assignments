unit UGroupList;

interface

uses
  Sil,
  UDef;

type
  TGroupList = class ( TSilInterfacedObject, IGroupList )
  private
    FFilters: IFilters;
    FList: IInterfaceList;
  protected // IGroupList
    function GetGroup(Index: integer): IGroup;
    function GetGroupByName(const Name: string): IGroup;
    function New(System: boolean = false): IGroup;
    procedure Remove(const Item: IGroup);
    function GetCount: integer;
    function GetOrderOf(const Name: string): integer;
    procedure SetOrderOf(const Name: string; Value: integer);
    function Enumerate(var Enum: IEnumerator; out Item: IGroup): boolean;
    // properties
    property Count: integer read GetCount;
    property Group[Index: integer]: IGroup read GetGroup;
    property GroupByName[const Name: string]: IGroup read GetGroupByName;
    property OrderOf[const Name: string]: integer read GetOrderOf write SetOrderOf;
  public
    constructor Create( const Owner: IFilters );
  end;

implementation

uses
  UGroup;

{ TGroupList }

constructor TGroupList.Create(const Owner: IFilters);
begin
  inherited Create;
  FFilters := Owner;
  FList := Sil.List.InterfaceList;
end;

function TGroupList.Enumerate(var Enum: IEnumerator;
  out Item: IGroup): boolean;
begin
  result := FList.Enumerate( Enum, Item );
end;

function TGroupList.GetCount: integer;
begin
  result := FList.Count;
end;

function TGroupList.GetGroup(Index: integer): IGroup;
begin
  if ( 0 <= Index ) and ( Index < FList.Count ) then
    result := IGroup( FList.Items[ Index ] ) else
    result := nil;
end;

function TGroupList.GetGroupByName(const Name: string): IGroup;
begin
  result := Group[ OrderOf[ Name ] ];
end;

function TGroupList.GetOrderOf(const Name: string): integer;
begin
  result := 0;
  while ( result < FList.Count ) do
  begin
    if ( Group[ result ].Name = Name ) then
      break;
    Inc( result );
  end;

  if ( result = FList.Count ) then
    result := -1;
end;

procedure TGroupList.SetOrderOf(const Name: string; Value: integer);
var
  i1: integer;
  grp: IGroup;
begin
  i1 := OrderOf[ Name ];
  if ( i1 <> Value ) then
  begin
    grp := Group[ i1 ];
    if ( Value < i1 ) then
    begin
      FList.Insert( Value, grp );
      FList.Delete( i1 + 1 );
    end
    else
    begin
      if ( Value + 1 >= FList.Count ) then
        FList.Add( grp ) else
        FList.Insert( Value + 1, grp );
      FList.Delete( i1 );
    end;
  end;
end;

function TGroupList.New(System: boolean): IGroup;
begin
  result := TGroup.Create( self, System );
  FList.Add( result );
end;

procedure TGroupList.Remove(const Item: IGroup);
begin
  FList.Remove( Item );
end;

end.

