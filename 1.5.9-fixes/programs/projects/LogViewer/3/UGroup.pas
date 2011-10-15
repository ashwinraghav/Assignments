unit UGroup;

interface

uses
  Sil,
  UDef;

type
  TGroup = class ( TSilInterfacedObject, IGroup )
  private
    FOwner: IGroupList;
    FSystem: boolean;
    FEnabled: boolean;
    FName: string;
    FActions: string;
    FIncludes: IStringList;
    FStopEvaluation: boolean;
    FKeysIncluded: string;
    FKeysExcluded: string;
    FKeysIncludedEnabled: boolean;
    FKeysExcludedEnabled: boolean;
  protected // IGroup
    function GetEnabled: boolean;
    function GetName: string;
    function GetSystem: boolean;
    function GetIncludes: IStringList;
    procedure SetEnabled(Value: boolean);
    procedure SetName(const Value: string);
    function GetKeysExcluded: string;
    function GetKeysIncluded: string;
    procedure SetKeysExcluded(const Value: string);
    procedure SetKeysIncluded(const Value: string);
    function GetOrder: integer;
    procedure SetOrder(const Value: integer);
    function GetKeysExcludedEnabled: boolean;
    function GetKeysIncludedEnabled: boolean;
    procedure SetKeysExcludedEnabled(const Value: boolean);
    procedure SetKeysIncludedEnabled(const Value: boolean);
    function GetActions: string;
    procedure SetActions(const Value: string);
    function GetStopEvaluation: boolean;
    procedure SetStopEvaluation(const Value: boolean);
    function Match(const AText: string): boolean;
    // properties
    property Enabled: boolean read GetEnabled write SetEnabled;
    property Name: string read GetName write SetName;
    property System: boolean read GetSystem;
    property Actions: string read GetActions write SetActions;
    property Includes: IStringList read GetIncludes;
    property Order: integer read GetOrder write SetOrder;
    property StopEvaluation: boolean read GetStopEvaluation write SetStopEvaluation;
    property KeysIncluded: string read GetKeysIncluded write SetKeysIncluded;
    property KeysExcluded: string read GetKeysExcluded write SetKeysExcluded;
    property KeysIncludedEnabled: boolean read GetKeysIncludedEnabled write SetKeysIncludedEnabled;
    property KeysExcludedEnabled: boolean read GetKeysExcludedEnabled write SetKeysExcludedEnabled;
  public
    constructor Create( const Owner: IGroupList; System: boolean );
  end;

implementation

{ TGroup }

constructor TGroup.Create(const Owner: IGroupList; System: boolean);
begin
  inherited Create;
  FOwner := Owner;
  FSystem := System;
  FEnabled := true;
  FIncludes := Sil.List.StringList;
end;

function TGroup.GetActions: string;
begin
  result := FActions;
end;

function TGroup.GetIncludes: IStringList;
begin
  result := FIncludes;
end;

function TGroup.GetKeysExcluded: string;
begin
  result := FKeysExcluded;
end;

function TGroup.GetKeysExcludedEnabled: boolean;
begin
  result := FKeysExcludedEnabled;
end;

function TGroup.GetKeysIncluded: string;
begin
  result := FKeysIncluded;
end;

function TGroup.GetKeysIncludedEnabled: boolean;
begin
  result := FKeysIncludedEnabled;
end;

function TGroup.GetName: string;
begin
  result := FName;
end;

function TGroup.GetOrder: integer;
begin
  result := FOwner.OrderOf[ FName ];
end;

function TGroup.GetStopEvaluation: boolean;
begin
  result := FStopEvaluation;
end;

function TGroup.GetSystem: boolean;
begin
  result := FSystem;
end;

function TGroup.GetEnabled: boolean;
begin
  result := FEnabled;
end;

function TGroup.Match(const AText: string): boolean;
var
  s1: string;
  i1: integer;
begin
  result := true;
  if FKeysIncludedEnabled then
  begin
    i1 := 0;
    while result and Sil.Str.Enumerate( FKeysIncluded, ';', s1, i1 ) do
      result := Sil.Str.Pos( s1, AText ) > 0;
  end;

  if FKeysExcludedEnabled then
  begin
    i1 := 0;
    while result and Sil.Str.Enumerate( FKeysExcluded, ';', s1, i1 ) do
      result := Sil.Str.Pos( s1, AText ) <= 0;
  end;
end;

procedure TGroup.SetActions(const Value: string);
begin
  FActions := Value;
end;

procedure TGroup.SetKeysExcluded(const Value: string);
begin
  FKeysExcluded := Value;
end;

procedure TGroup.SetKeysExcludedEnabled(const Value: boolean);
begin
  FKeysExcludedEnabled := Value;
end;

procedure TGroup.SetKeysIncluded(const Value: string);
begin
  FKeysIncluded := Value;
end;

procedure TGroup.SetKeysIncludedEnabled(const Value: boolean);
begin
  FKeysIncludedEnabled := Value;
end;

procedure TGroup.SetName(const Value: string);
begin
  if ( Length( Value ) = 0 ) then
    raise Exception.Create( 'Nombre de grupo inválido.' );

  if ( FName <> Value ) then
  begin
    if Assigned( FOwner.GroupByName[ Value ] ) then
      raise Exception.Create( 'Nombre de grupo ya existente.' );

    FName := Value;
  end;
end;

procedure TGroup.SetOrder(const Value: integer);
begin
  FOwner.OrderOf[ FName ] := Value;
end;

procedure TGroup.SetStopEvaluation(const Value: boolean);
begin
  FStopEvaluation := Value;
end;

procedure TGroup.SetEnabled(Value: boolean);
begin
  FEnabled := Value;
end;

end.

