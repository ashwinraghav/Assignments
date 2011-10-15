unit SilSmRules;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilClasses, SilSiLists, SilSiModel;

type
  TSilRuleList = class(
    TSilInterfaceList,
    IRules,
    IRuleList )
  private
    FModel: IModel; 
    FOwner: TObject;
    FEntity: IEntity;
  protected // IRules
    function GetFirst: PIRule;
    function GetLast: PIRule;
    function GetItem(Index: Integer): IRule;
    function GetRule(const Name: string): IRule;
  protected // IRuleList
    function Add(const Name: string; Kind: TRuleKind = rkMethod; const Defines: ITag = nil; const Depends: ITags = nil; Owner: TObject = nil): IRule; reintroduce; overload;
    function Add(const Name: string; const Defines: ITag = nil; const Depends: ITags = nil; Owner: TObject = nil): IRule; reintroduce; overload;
  public
    constructor Create(const Model: IModel; const Entity: IEntity; Owner: TObject = nil);
    destructor Destroy; override;
  public
    property Owner: TObject read FOwner;
    property Item[Index: Integer]: IRule read GetItem;
    property Rule[const Name: string]: IRule read GetRule; default;
  end;

  TSilRuleClass = class of TSilRule;

  TSilRule = class(TSilObject)
  private
    FName: string;
    FList: TSilRuleList;
    FOwner: TObject;
    FIndex: Integer;
    FData: Pointer;
    FDepends: ITags;
    FDefines: Pointer;
  protected // IRule
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetDefines: ITag;
    procedure SetDefines(const Value: ITag);
    function GetDepends: ITags;
    procedure SetDepends(const Value: ITags);
    function GetIsValid: Boolean;
    function GetIsInvalid: Boolean;
    function GetInstanceName: string; override;
  public
    constructor Create(List: TSilRuleList; const Name: string; const Depends: ITags; const Defines: ITag; Owner: TObject); overload; virtual;
    destructor Destroy; override;
  public
    class function CreateNew(List: TSilRuleList; const Name: string; Kind: TRuleKind; const Depends: ITags; const Defines: ITag; Owner: TObject): TSilRule; reintroduce;
  public
    property Name: string read GetInstanceName;
    property Data: Pointer read FData write FData;
  end;

  TEvRuleCallback = procedure(const Model: TObject; const Rule: IRule; const Entity: TObject) of object;

  TSilRuleMethod = class(
    TSilRule,
    IRule )
  private
    FModel: TObject;
    FEntity: TObject;
    FAddress: Pointer;
  protected // IRule
    procedure Update;
  public
    constructor Create(List: TSilRuleList; const Name: string; const Depends: ITags; const Defines: ITag; Owner: TObject); override; 
    destructor Destroy; override;
  public
    property Address: Pointer read FAddress; 
  end;
  
implementation

const
  MClassRef: array[TRuleKind] of TSilRuleClass =
    (
      TSilRule,       // rkExternal,
      TSilRuleMethod, // rkMethod,
      TSilRule,       // rkCalculation,
      TSilRule        // rkTrigger
    );

{ TSilRuleList }

constructor TSilRuleList.Create(const Model: IModel; const Entity: IEntity; Owner: TObject);
begin
  inherited Create;
  FModel := Model;
  FOwner := Owner;
  FEntity := Entity;
end;

destructor TSilRuleList.Destroy;
begin
  FEntity := nil;
  FOwner := nil;
  FModel := nil;
  inherited;
end;

function TSilRuleList.Add(const Name: string; Kind: TRuleKind; const Defines: ITag; const Depends: ITags; Owner: TObject): IRule;
begin
  Result := TSilRule.CreateNew(Self, Name, Kind, Depends, Defines, Owner) as IRule;
end;

function TSilRuleList.Add(const Name: string; const Defines: ITag; const Depends: ITags; Owner: TObject): IRule;
begin
  Result := Add(Name, rkMethod, Defines, Depends, Owner);
end;

function TSilRuleList.GetItem(Index: Integer): IRule;
begin
  Result := IRule(Pointer(Integer(List) + Index * SizeOf(Pointer))^);
end;

function TSilRuleList.GetRule(const Name: string): IRule;
var
  First, Last: PIRule;
begin
  Result := nil;
  First := Pointer(List);
  Last := Pointer(Integer(List) + Count * SizeOf(Pointer));

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

function TSilRuleList.GetFirst: PIRule;
begin
  Result := Pointer(List);
end;

function TSilRuleList.GetLast: PIRule;
begin
  Result := Pointer(Integer(List) + Count * SizeOf(Pointer));
end;

{ TSilRule }

constructor TSilRule.Create(List: TSilRuleList; const Name: string; const Depends: ITags; const Defines: ITag; Owner: TObject);
begin
  inherited Create;
  FList := List;
  if Assigned(Owner) then
    FOwner := Owner
  else if Assigned(FList) then
    FOwner := List.Owner
  else if Assigned(Defines) then
    FOwner := Defines.Owner; 
  FName := Name;
  FDepends := Depends;
  SetDefines(Defines);
  if Assigned(FList) then FIndex := FList.Add(Self as IRule);
  if Assigned(FOwner) then SetReference(FOwner, FName, Self);
end;

destructor TSilRule.Destroy;
begin
  if Assigned(FOwner) then SetReference(FOwner, FName, nil);
//!if Assigned(FList) then FList.Remove(IRule(Self));
  FDefines := nil;
  FDepends := nil;
  FList := nil;
  inherited;
end;

class function TSilRule.CreateNew(List: TSilRuleList; const Name: string; Kind: TRuleKind; const Depends: ITags; const Defines: ITag; Owner: TObject): TSilRule;
begin
  Result := MClassRef[Kind].Create(List, Name, Depends, Defines, Owner);  
end;

function TSilRule.GetData: Pointer;
begin
  Result := FData;
end;

function TSilRule.GetInstanceName: string;
begin
  Result := FName;
end;

procedure TSilRule.SetData(const Value: Pointer);
begin
  FData := Value;
end;

function TSilRule.GetDefines: ITag;
begin
  Result := ITag(FDefines);
end;

procedure TSilRule.SetDefines(const Value: ITag);
begin
  if Pointer(Value) <> FDefines then
  begin
    if Assigned(FDefines) and     ITag(FDefines).IsCalculated then ITag(FDefines).SetRule(nil);
    FDefines := Pointer(Value);
    if Assigned(FDefines) and not ITag(FDefines).IsCalculated then ITag(FDefines).SetRule(Self as IRule);
  end;
end;

function TSilRule.GetDepends: ITags;
begin
  Result := FDepends;
end;

procedure TSilRule.SetDepends(const Value: ITags);
begin
  FDepends := Value;
end;

function TSilRule.GetIsInvalid: Boolean;
begin
  Result := False;
  {checkeo}
end;

function TSilRule.GetIsValid: Boolean;
begin
  Result := True;
  {checkeo}
end;

{ TSilRuleMethod }

constructor TSilRuleMethod.Create(List: TSilRuleList; const Name: string; const Depends: ITags; const Defines: ITag; Owner: TObject);
begin
  inherited;
  if Assigned(FList) then
  begin
    FModel := FList.FModel.Owner;
    if Assigned(FList.FEntity) then
      FEntity := FList.FEntity.GetInstance else
      FEntity := nil;
  end;
  if Assigned(FOwner) then FAddress := FOwner.MethodAddress(Name);
end;

destructor TSilRuleMethod.Destroy;
begin
  FAddress := nil;
  FEntity := nil;
  FModel := nil;
  inherited;
end;

procedure TSilRuleMethod.Update;
var
  Method: TEvRuleCallback;
begin
  if Assigned(FAddress) and Assigned(FOwner) then
  begin
    TMethod(Method).Data := FOwner;
    TMethod(Method).Code := FAddress;
    Method(FModel, Self, FEntity);
  end;
end;

end.
 