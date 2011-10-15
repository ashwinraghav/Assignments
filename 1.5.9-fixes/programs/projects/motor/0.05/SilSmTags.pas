unit SilSmTags;

{$INCLUDE Defines.inc}

interface

uses
  TypInfo,
  Sil, SilClasses, SilSiLists, SilSiHolors, SilSiModel, SilSeValues;

type
  TSilTagList = class(
    TSilInterfaceList,
    ICollection,
    ITags,
    ITagList )
  private
    FOwner: TObject;
    FParent: ITags;
    FName: String;
  protected //- ITags
    function GetInstanceName: string; override;
    function GetItem(Index: Integer): ITag;
    function GetTag(const Name: string): ITag;
    function Lookup(const Name: string; out Tag: ITag): Boolean;
    function Enumerate(var Enum; out Tag: ITag): Boolean; reintroduce; overload;
    function Enumerate(var Enum; out Name: string): Boolean; reintroduce; overload;
    function NewList(const Name: string; Owner: TObject): ITagList;
    function Slice(const Tags: array of ITag; const Name: string = ''; Owner: TObject = nil): ITags;
    function GetFirst: PITag;
    function GetLast: PITag;
  protected // ITagList
    function Add(const Name: string; Kind: TTagKind = tkUndefined; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: Boolean; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: Integer; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: Double; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: string; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: Variant; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: Pointer; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: IObject; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: IInterface; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Add(const Name: string; const Value: IHolor; const Rule: IRule = nil; Owner: TObject = nil): ITag; reintroduce; overload;
    function Remove(const Name: string): Integer; reintroduce; overload;
    function Remove(const Tag: ITag): Integer; reintroduce; overload;
    procedure Remove(Index: Integer); reintroduce; overload;
  public
    constructor Create(const Name: string; Owner: TObject = nil; const Parent: ITags = nil);
    destructor Destroy; override;
    property Owner: TObject read FOwner;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: ITag read GetItem;
    property Tag[const Name: string]: ITag read GetTag; default;
  end;

  TSilTagClass = class of TSilTag;
  TSilTag = packed class(
    TSilObject,
    ITag )
  private
    FOwner: TObject;
    FList: TSilTagList;
    FName: string;
    FIndex: Integer;
    FData: Pointer;
    FRule: IRule;
  private
    procedure DoCheck;
  protected //- ITag
    function GetName: String;
    function GetOwner: TObject;
    function GetInstanceName: string; override;
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetRule: IRule;
    function GetIsCalculated: Boolean;
    function GetKind: TTagKind; virtual; abstract;
    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;
    function GetText: String; virtual; abstract;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Rule: IRule = nil; Owner: TObject = nil); virtual;
    destructor Destroy; override;
  public 
    procedure SetRule(const Value: IRule); overload; 
    function SetRule(const Name: string; Kind: TRuleKind; const Depends: ITags = nil; Owner: TObject = nil): IRule; overload; 
    function SetRule(const Name: string; const Depends: ITags = nil; Owner: TObject = nil): IRule; overload;
  public
    property Name: string read GetInstanceName;
    property Kind: TTagKind read GetKind;
    property Data: Pointer read GetData write SetData; // generico
    property Value: Variant read GetValue write SetValue;
    property Rule: IRule read GetRule;
  end;

  TSilTagBoolean = packed class(TSilTag)
  protected
    FValue: Boolean;
  protected
    function GetBoolean: PBoolean;
  protected //- ITagBoolean
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; Value: Boolean; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PBoolean read GetBoolean;
  end;

  TSilTagInteger = packed class(TSilTag)
  protected
    FValue: Integer;
  protected
    function GetInteger: PInteger;
  protected //-
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; Value: Integer; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PInteger read GetInteger;
  end;

  TSilTagFloat = packed class(TSilTag)
  protected
    FValue: Double;
  protected
    function GetFloat: PDouble;
  protected //- ITagFloat
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Value: Double; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PDouble read GetFloat;
  end;

  TSilTagString = packed class(TSilTag)
  protected
    FValue: string;
  protected
    function GetString: PString;
  protected //- ITagString
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Value: String; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PString read GetString;
  end;

  TSilTagVariant = packed class(TSilTag)
  protected
    FValue: Variant;
  protected
    function GetVariant: PVariant;
  protected //- ITagVariant
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Value: Variant; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PVariant read GetVariant;
  end;

  TSilTagPointer = packed class(TSilTag)
  protected
    FValue: Pointer;
  protected
    function GetPointer: PPointer;
  protected //- ITagPointer
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; Value: Pointer; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PPointer read GetPointer;
  end;

  TSilTagInterface = packed class(TSilTag)
  protected
    FValue: IUnknown;
  protected
    function GetInterface: PIInterface;
  protected //- ITagInterface
    function GetKind: TTagKind; override;
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Value: IInterface; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PIInterface read GetInterface;
  end;

  TSilTagObject = packed class(TSilTagInterface)
  protected
    function GetObject: PIObject;
  protected //-
    function GetKind: TTagKind; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Value: IObject; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PIObject read GetObject;
  end;

  TSilTagHolor = packed class(TSilTagInterface)
  protected
    function GetHolor: PIHolor;
  protected //-
    function GetKind: TTagKind; override;
    procedure SetValue(const Value: Variant); override;
    function GetText: String; override;
  public
    constructor Create(const List: TSilTagList; const Name: string; const Value: IHolor; const Rule: IRule = nil; Owner: TObject = nil); reintroduce;
  public
    property Value: PIHolor read GetHolor;
  end;

implementation

uses
  SilSmRules, SilSfValues;

const
  MClassRef: array[TTagKind] of TSilTagClass =
    (
      nil,              // tkUndefined,
      TSilTagBoolean,   // tkBoolean,
      TSilTagInteger,   // tkInteger,
      TSilTagFloat,     // tkFloat,
      TSilTagString,    // tkString,
      TSilTagVariant,   // tkVariant,
      TSilTagPointer,   // tkPointer,
      TSilTagObject,    // tkObject,
      TSilTagInterface, // tkInterface,
      TSilTagHolor      // tkHolor
    );

{ TSilTagList }

constructor TSilTagList.Create(const Name: string; Owner: TObject; const Parent: ITags); 
begin
  inherited Create;
  FOwner := Owner;
  FParent := Parent;
  FName := Name;
end;

destructor TSilTagList.Destroy;
begin
  FOwner := nil;
  FParent := nil;
  inherited;
end;

function TSilTagList.Add(const Name: string; Kind: TTagKind; const Rule: IRule; Owner: TObject): ITag;
begin
  if Kind <> tkUndefined then
    Result := MClassRef[Kind].Create(Self, Name, Rule, Owner) as ITag else
    raise Sil.Error.Create('SilSmTags.TSilTagList.Add: Kind = tkUndefined');
end;

function TSilTagList.Add(const Name: string; const Value: Double; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagFloat.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name, Value: string; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagString.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: Variant; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagVariant.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: Boolean; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagBoolean.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: Integer; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagInteger.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: IInterface; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagInterface.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: IHolor; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagHolor.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: Pointer; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagPointer.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Add(const Name: string; const Value: IObject; const Rule: IRule; Owner: TObject): ITag;
begin
  Result := TSilTagObject.Create(Self, Name, Value, Rule, Owner);
end;

function TSilTagList.Enumerate(var Enum; out Name: string): Boolean;
var
  Tag: ITag;
begin
  Result := inherited Enumerate(IEnumerator(Enum), Tag);
  if Result then Name := Tag.Name; 
end;

function TSilTagList.Enumerate(var Enum; out Tag: ITag): Boolean;
begin
  Result := inherited Enumerate(IEnumerator(Enum), Tag);
end;

function TSilTagList.GetInstanceName: string;
begin
  Result := FName;
end;

function TSilTagList.GetItem(Index: Integer): ITag;
begin
  Result := ITag(Pointer(Integer(List) + Index * SizeOf(Pointer))^);
end;

function TSilTagList.GetTag(const Name: string): ITag;
begin
  if not Lookup(Name, Result) then
    raise Sil.Error.Create('No existe el tag: %s', [Name]);
end;

function TSilTagList.Lookup(const Name: string; out Tag: ITag): Boolean;
var
  First, Last: PITag;
begin
  Result := False;
  First := Pointer(Self.List);
  Last := Pointer(Integer(Self.List) + Count * SizeOf(Pointer));

  while First <> Last do
  begin
    if Str.CompareText(Name, First.Name, True) = 0 then
    begin
      Tag := First^;
      Result := True;
      Break;
    end;
    Inc(First);
  end;
end;

function TSilTagList.Remove(const Name: string): Integer;
var
  Tag: ITag;
begin
  if Lookup(Name, Tag) then
    Result := inherited Remove(Tag) else
    Result := -1;
end;

function TSilTagList.Remove(const Tag: ITag): Integer;
begin
  Result := inherited Remove(Tag);
end;

procedure TSilTagList.Remove(Index: Integer);
begin
  inherited Delete(Index);
end;

function TSilTagList.NewList(const Name: string; Owner: TObject): ITagList;
begin
  Result := TSilTagList.Create(Name, Owner, Self);
end;

function TSilTagList.GetFirst: PITag;
begin
  Result := Pointer(List);
end;

function TSilTagList.GetLast: PITag;
begin
  Result := Pointer(Integer(List) + Count * SizeOf(Pointer));
end;

function TSilTagList.Slice(const Tags: array of ITag; const Name: string; Owner: TObject): ITags;
var
  Instance: TSilTagList;
  I: Integer;
begin
  if Owner = nil then Owner := FOwner;
  Instance := TSilTagList.Create(Name, Owner);
  for I := 0 to High(Tags) do Instance.Add(Tags[I]);
  Result := Instance;
end;

{ TSilTag }

constructor TSilTag.Create(const List: TSilTagList; const Name: string; const Rule: IRule; Owner: TObject);
begin
  inherited Create;
  if Assigned(Owner) then
    FOwner := Owner else
    FOwner := List.Owner;
  FRule := Rule;
  FList := List;
  FName := Name;
  FIndex := FList.Add(Self as ITag);
  if Assigned(FOwner) then SetReference(FOwner, FName, Self); 
end;

destructor TSilTag.Destroy;
begin
  if Assigned(FOwner) then SetReference(FOwner, FName, nil); 
  //! FList.Remove(ITag(Self));
  FList := nil;
  FRule := nil;
  inherited;
end;

function TSilTag.GetData: Pointer;
begin
  Result := FData;
end;

procedure TSilTag.SetData(const Value: Pointer);
begin
  FData := Value;
end;

function TSilTag.GetRule: IRule;
begin
  Result := FRule;
end;

procedure TSilTag.SetRule(const Value: IRule);
begin
  if Pointer(Value) <> Pointer(FRule) then
  begin
    if Assigned(FRule) then FRule.Defines := nil;
    FRule := Value;
    if Assigned(FRule) then FRule.Defines := Self as ITag;
  end;
end;

function TSilTag.SetRule(const Name: string; Kind: TRuleKind; const Depends: ITags; Owner: TObject): IRule;
begin
  SetRule(TSilRule.CreateNew(nil, Name, Kind, Depends, Self as ITag, Owner) as IRule);
end;

function TSilTag.SetRule(const Name: string; const Depends: ITags; Owner: TObject): IRule;
begin
  SetRule(TSilRule.CreateNew(nil, Name, rkMethod, Depends, Self as ITag, Owner) as IRule);
end;

function TSilTag.GetIsCalculated: Boolean;
begin
  Result := Assigned(FRule);
end;

function TSilTag.GetInstanceName: string;
begin
  Result := FName;
end;

function TSilTag.GetName: String;
begin
  Result := FName;
end;

function TSilTag.GetOwner: TObject;
begin
  Result := FOwner;
end;

procedure TSilTag.DoCheck;
begin
  if Assigned(FRule) then
    FRule.Update;
end;

{ TSilTagBoolean }

constructor TSilTagBoolean.Create(const List: TSilTagList; const Name: string; Value: Boolean; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagBoolean.GetBoolean: PBoolean;
begin
  Result := @FValue;
end;

function TSilTagBoolean.GetKind: TTagKind;
begin
  Result := tkBoolean;
end;

function TSilTagBoolean.GetText: String;
begin
  Result := BooleanIdents[FValue];
end;

function TSilTagBoolean.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilTagBoolean.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TSilTagInteger }

constructor TSilTagInteger.Create(const List: TSilTagList; const Name: string; Value: Integer; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagInteger.GetInteger: PInteger;
begin
  Result := @FValue;
end;

function TSilTagInteger.GetKind: TTagKind;
begin
  Result := tkInteger;
end;

function TSilTagInteger.GetText: String;
begin
  Result := Int.ToStr(FValue);
end;

function TSilTagInteger.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilTagInteger.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TSilTagFloat }

constructor TSilTagFloat.Create(const List: TSilTagList; const Name: string; const Value: Double; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagFloat.GetFloat: PDouble;
begin
  Result := @FValue;
end;

function TSilTagFloat.GetKind: TTagKind;
begin
  Result := tkFloat;
end;

function TSilTagFloat.GetText: String;
begin
  Result := Float.ToStr(FValue);
end;

function TSilTagFloat.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilTagFloat.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TSilTagString }

constructor TSilTagString.Create(const List: TSilTagList; const Name, Value: String; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagString.GetKind: TTagKind;
begin
  Result := tkString;
end;

function TSilTagString.GetString: PString;
begin
  Result := @FValue;
end;

function TSilTagString.GetText: String;
begin
  Result := FValue;
end;

function TSilTagString.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilTagString.SetValue(const Value: Variant);
begin
  FValue := Value; //@!@
end;

{ TSilTagVariant }

constructor TSilTagVariant.Create(const List: TSilTagList; const Name: string; const Value: Variant; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagVariant.GetKind: TTagKind;
begin
  Result := tkVariant;
end;

function TSilTagVariant.GetText: String;
begin
  Result := Vart.ToStr(FValue);
end;

function TSilTagVariant.GetValue: Variant;
begin
  Result := FValue;
end;

function TSilTagVariant.GetVariant: PVariant;
begin
  Result := @FValue;
end;

procedure TSilTagVariant.SetValue(const Value: Variant);
begin
  FValue := Value;
end;

{ TSilTagPointer }

constructor TSilTagPointer.Create(const List: TSilTagList; const Name: string; Value: Pointer; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagPointer.GetKind: TTagKind;
begin
  Result := tkPointer;
end;

function TSilTagPointer.GetPointer: PPointer;
begin
  Result := @FValue;
end;

function TSilTagPointer.GetText: String;
begin
  Result := '0x' + Int.ToHex(Integer(FValue), 8);
end;

function TSilTagPointer.GetValue: Variant;
begin
  Result := Integer(FValue);
end;

procedure TSilTagPointer.SetValue(const Value: Variant);
begin
  FValue := Pointer(Integer(Value)); 
end;

{ TSilTagInterface }

constructor TSilTagInterface.Create(const List: TSilTagList; const Name: string; const Value: IInterface; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Rule, Owner);
  FValue := Value;
end;

function TSilTagInterface.GetInterface: PIInterface;
begin
  Result := @FValue;
end;

function TSilTagInterface.GetKind: TTagKind;
begin
  Result := tkInterface;
end;

function TSilTagInterface.GetText: String;
begin
  if Assigned(FValue) then
    Result := 'IUnknown(0x' + Int.ToHex(Integer(FValue), 8) + ')' else
    Result := 'nil';
end;

function TSilTagInterface.GetValue: Variant;
begin
  Result := FValue;
end;

procedure TSilTagInterface.SetValue(const Value: Variant);
begin
  FValue := Sil.Vart.ToUnknown(Value);
end;

{ TSilTagObject }

constructor TSilTagObject.Create(const List: TSilTagList; const Name: string; const Value: IObject; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Value, Rule, Owner);
end;

function TSilTagObject.GetKind: TTagKind;
begin
  Result := tkObject;
end;

function TSilTagObject.GetObject: PIObject;
begin
  Result := @FValue;
end;

function TSilTagObject.GetText: String;
begin
  if Assigned(FValue) then
    Result := 'Object(' + IObject(FValue).ClassType.ClassName + '(' + IObject(FValue).InstanceName + '))' else
    Result := 'nil';
end;

procedure TSilTagObject.SetValue(const Value: Variant);
begin
  Sil.Vart.ToInterface(Value, IObject, IObject(FValue));
end;

{ TSilTagHolor }

constructor TSilTagHolor.Create(const List: TSilTagList; const Name: string; const Value: IHolor; const Rule: IRule; Owner: TObject);
begin
  inherited Create(List, Name, Value, Rule, Owner);
end;

function TSilTagHolor.GetHolor: PIHolor;
begin
  Result := @FValue;
end;

function TSilTagHolor.GetKind: TTagKind;
begin
  Result := tkHolor;
end;

function TSilTagHolor.GetText: String;
begin
  if Assigned(FValue) then
    Result := 'Holor(' + Shw(IHolor(FValue).Limits) + ')' else
    Result := 'nil';
end;

procedure TSilTagHolor.SetValue(const Value: Variant);
begin
  Sil.Vart.ToInterface(Value, IHolor, IHolor(FValue));
end;                                   

{ globales }

end.
 