unit UFilters;

interface

uses
  Sil,
  UDef;

type
  TFilters = class ( TSilInterfacedObject, IFilters )
  private
    FGroups: IGroupList;
  protected // IFilters
    function GetActions(const AText: string; out AActions: string): boolean;
    function GetGroups: IGroupList;
    // properties
    property Groups: IGroupList read GetGroups;
  public
    constructor Create; reintroduce;
  end;

implementation

uses
  UGroupList;

{ TFilters }

constructor TFilters.Create;
begin
  inherited Create;
  FGroups := TGroupList.Create( self );
  with FGroups.New( true ) do
  begin
    Name := '(default)';
    Actions := 'Show';
  end;
end;

function TFilters.GetGroups: IGroupList;
begin
  result := FGroups;
end;

function TFilters.GetActions(const AText: string; out AActions: string): boolean;
var
  enum: IEnumerator;
  grp: IGroup;
begin
  result := false;
  AActions := '';
  while Groups.Enumerate( enum, grp ) do
    if grp.Enabled and grp.Match( AText ) then
    begin
      result := true;

      if ( Length( AActions ) = 0 ) then
        AActions := grp.Actions else
        AActions := AActions + ';' + grp.Actions;

      if grp.StopEvaluation then
        break;
    end;
end;

end.

