unit SilSmEntities;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilClasses, SilSiLists, SilSiModel;

type
  TSilEntityList = class(
    TSilInterfaceList,
    ICollection,
    IEntities,
    IEntityList )
  private
    FModel: IModel;
    FOwner: TObject;
  protected //- IEntities
    function GetFirst: PIEntity;
    function GetLast: PIEntity;
    function Lookup(const Name: string): IEntity; overload;
    function Enumerate(var Enum; out Item: IEntity): Boolean; reintroduce; overload;
    function Enumerate(var Enum; out Name: String): Boolean; reintroduce; overload;
  protected //- IEntityList
    function Add(const Name: string; Kind: TEntityKind = ekObject; const Parent: IEntity = nil; Owner: TObject = nil): IEntity; reintroduce; overload; // base
    function Add(const Name: string; Kind: TClass; const Parent: IEntity = nil; Owner: TObject = nil): IEntity; reintroduce; overload; // base
    function Add(const Item: IEntity): Integer; reintroduce; overload; 
    function Remove(const Item: IEntity): Integer; reintroduce;
  public
    constructor Create(const Model: IModel; Owner: TObject = nil);
    destructor Destroy; override;
  public
    property Owner: TObject read FOwner;
  end;

  TSilEntityClass = class of TSilEntity;
  TSilEntity = class(
    TSilObject,
    IEntity )
  private
    FList: TSilEntityList;
    FOwner: TObject;
    FName: String;
    FParent: TSilEntity;
    FChildren: IEntityList;
    FRules: IRuleList;
    FTags: ITagList;
    FData: Pointer;
  protected //- IEntity
    function GetModel: IModel;
    function GetOwner: TObject;
    function GetParent: IEntity;
    procedure SetParent(const Value: IEntity);
    function GetKind: TEntityKind; virtual;
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetRules: IRuleList;
    function GetTags: ITagList;
    function GetObjects: IInterfaces;
    function GetChildren: IEntities;
    function GetInstanceName: string; override;
  public
    constructor Create(List: TSilEntityList; const Name: string; const Parent: IEntity = nil; Owner: TObject = nil); virtual;
    destructor Destroy; override;
  public
    property Name: string read GetInstanceName;
    property Parent: IEntity read GetParent write SetParent;
    property Data: Pointer read GetData write SetData; // generico
    property Rules: IRuleList read GetRules;
    property Tags: ITagList read GetTags;
    property Objects: IInterfaces read GetObjects;
    property Children: IEntities read GetChildren;
  end;

  TSilEntityObject = class(TSilEntity)
  protected 
    function GetKind: TEntityKind; override; 
  end;
  
  TSilEntityGroup = class(TSilEntity)
  protected 
    function GetKind: TEntityKind; override; 
  end;

  TSilEntityCoordinates = class(TSilEntity)
  protected 
    function GetKind: TEntityKind; override; 
  end;

  TSilEntityLayer = class(TSilEntity)
  protected 
    function GetKind: TEntityKind; override; 
  end;

  TSilEntityCustom = class(TSilEntity)
  protected 
    function GetKind: TEntityKind; override; 
  end;

implementation

uses
  SilSmTags, SilSmRules;

const
  MClassRef: array[TEntityKind] of TSilEntityClass =
    (
      nil,            // ekUndefined,
      TSilEntity,     // ekObject,
      TSilEntity,     // ekGroup,
      TSilEntity,     // ekCoordinates,
      TSilEntity,     // ekLayer,
      TSilEntity      // ekCustom
    );

{ TSilEntityList }

constructor TSilEntityList.Create(const Model: IModel; Owner: TObject);
begin
  inherited Create;
  FModel := Model;  
  FOwner := Owner;
end;

destructor TSilEntityList.Destroy;
begin
  FOwner := nil;
  FModel := nil;  
  inherited;
end;

function TSilEntityList.Add(const Name: string; Kind: TEntityKind; const Parent: IEntity; Owner: TObject): IEntity;
begin
  if Kind <> ekUndefined then
    Result := MClassRef[Kind].Create(Self, Name, Parent, Owner) else
    raise Sil.Error.Create('SilSmEntities.TSilEntityList.Add: Kind = ekUndefined');
end;

function TSilEntityList.Add(const Name: string; Kind: TClass; const Parent: IEntity; Owner: TObject): IEntity;
begin
  if Kind.InheritsFrom(TSilEntity) then
    Result := TSilEntityClass(Kind).Create(Self, Name, Parent, Owner) else
    raise Sil.Error.Create('SilSmEntities.TSilEntityList.Add: Kind no deriva de TSilEntity');
end;

function TSilEntityList.Lookup(const Name: string): IEntity;
var
  First, Last: PIEntity;
begin
  Result := nil;
  First := Pointer(Self.List);
  Last := Pointer(Integer(Self.List) + Count * SizeOf(Pointer));
  while First <> Last do
  begin
    if Str.CompareText(Name, First.Name, True) = 0 then
    begin
      Result := First^;
      Break;
    end;
    Inc(First);
  end;
end;

function TSilEntityList.Enumerate(var Enum; out Name: String): Boolean;
var
  Item: IEntity;
begin
  Result := inherited Enumerate(IEnumerator(Enum), Item);
  if Result then Name := Item.Name;
end;

function TSilEntityList.Enumerate(var Enum; out Item: IEntity): Boolean;
begin
  Result := inherited Enumerate(IEnumerator(Enum), Item);
end;

function TSilEntityList.GetFirst: PIEntity;
begin
  Result := Pointer(List);
end;

function TSilEntityList.GetLast: PIEntity;
begin
  Result := Pointer(Integer(List) + Count * SizeOf(Pointer));
end;

function TSilEntityList.Add(const Item: IEntity): Integer;
begin
  Result := inherited Add(Item);
end;

function TSilEntityList.Remove(const Item: IEntity): Integer;
begin
  Result := inherited Remove(Item);
end;

{ TSilEntity }

constructor TSilEntity.Create(List: TSilEntityList; const Name: string; const Parent: IEntity; Owner: TObject);
var
  Tags: ITags;
begin
  inherited Create;
  FList := List;
  FName := Name;
  if Assigned(Owner) then
    FOwner := Owner else
    FOwner := List.Owner;
  FChildren := TSilEntityList.Create(FList.FModel);
  SetParent(Parent);
  if Assigned(FParent) then
  begin
    Tags := FParent.Tags;
    FParent.FChildren.Add(Self);
  end else
    Tags := nil;
  FTags := TSilTagList.Create(Name, Self, Tags);
  FRules := TSilRuleList.Create(FList.FModel, Self, Self);
  FList.Add(IEntity(Self));
  if Assigned(FOwner) then SetReference(FOwner, FName, Self);
end;

destructor TSilEntity.Destroy;
begin
  if Assigned(FOwner) then SetReference(FOwner, FName, nil);  
  //!FList.Remove(IEntity(Self));
  FRules := nil;
  FTags := nil;
  FList := nil;
  FChildren := nil;
  inherited;
end;

function TSilEntity.GetChildren: IEntities;
begin
  Result := FChildren;
end;

function TSilEntity.GetObjects: IInterfaces;
begin

end;

function TSilEntity.GetParent: IEntity;
begin
  Result := FParent;
end;

procedure TSilEntity.SetParent(const Value: IEntity);
begin
  if Assigned(Value) then
    FParent := Sil.Ref.GetInstance(Value) else
    FParent := nil;
end;

function TSilEntity.GetKind: TEntityKind;
begin
  Result := ekCustom;
end;

function TSilEntity.GetRules: IRuleList;
begin
  Result := FRules;
end;

function TSilEntity.GetTags: ITagList;
begin
  Result := FTags;
end;

function TSilEntity.GetData: Pointer;
begin
  Result := FData;
end;

procedure TSilEntity.SetData(const Value: Pointer);
begin
  FData := Value;
end;

function TSilEntity.GetInstanceName: string;
begin
  Result := FName;
end;

function TSilEntity.GetModel: IModel;
begin
  Result := FList.FModel;
end;

function TSilEntity.GetOwner: TObject;
begin
  Result := FOwner;
end;

{ TSilEntityObject }

function TSilEntityObject.GetKind: TEntityKind;
begin
  Result := ekObject;
end;

{ TSilEntityGroup }

function TSilEntityGroup.GetKind: TEntityKind;
begin
  Result := ekGroup;
end;

{ TSilEntityCoordinates }

function TSilEntityCoordinates.GetKind: TEntityKind;
begin
  Result := ekCoordinates;
end;

{ TSilEntityLayer }

function TSilEntityLayer.GetKind: TEntityKind;
begin
  Result := ekLayer;
end;

{ TSilEntityCustom }

function TSilEntityCustom.GetKind: TEntityKind;
begin
  Result := ekCustom;
end;

end.
