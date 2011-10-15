unit SilSiModel;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilSiLists, SilSiHolors, SilSiBlocks;

(*
  dudas:
    1. Como definir una Rule que sea una simple operacion matematica entre Tags?
    2. Como asociar Tags con diferentes Layers? Es necesario?
    3. Es correcta la representacion de vectores y matrices?
    4. Algoritmos: deben estar en el Motor o en la logica externa?
         Quizá deba haber un repositorio de algoritmos tipicos, tales como
         - diferentes productos de matrices y vectores.
         - inversion de matrices.
         - solucion de ecuaciones.
         - etc.
    5. Sería util definir Tags sobre los mismos Tags? creo que no ...
    
  falta:
    1. Entidades: definir las interfaces para los Layers, Grupos y Coordenadas.
    2. Reglas: resolver como hacer para que la evaluacion de una regla vea los Tags
        de los objetos que necesite.
 *)

type
  IModel = interface;
  IEntity = interface;
  IEntities = interface;
  IEntityList = interface;
  IRule = interface;
  IRules = interface;
  IRuleList = interface;
  ITags = interface;
  ITagList = interface;
  ITag = interface;

  PIEntity = ^IEntity;
  PIRule = ^IRule;
  PITag = ^ITag;

  TagList = array of ITag;
  RuleList = array of IRule;
  Entities = array of IEntity; 

  IModel = interface
    ['{73A15B42-8792-43CC-BC66-502F5A4BB193}']
    function GetOwner: TObject;
    function GetEntities: IEntityList;
    function GetRules: IRuleList;
    function GetTags: ITagList;
    property Owner: TObject read GetOwner;
    property Entities: IEntityList read GetEntities;
    property Rules: IRuleList read GetRules;
    property Tags: ITagList read GetTags;
  end;
  
  TEntityKind = (
    ekUndefined,
    ekObject,
    ekGroup,
    ekCoordinates,
    ekLayer,
    ekCustom );

  IEntities = interface (ICollection)
    ['{8A878B95-9E85-4141-B652-4A975313273D}']
    function GetFirst: PIEntity;    
    function GetLast: PIEntity;    
    function Lookup(const Name: string): IEntity; overload;
    function Enumerate(var Enum; out Item: IEntity): Boolean; overload;
    function Enumerate(var Enum; out Name: String): Boolean; overload;
    property First: PIEntity read GetFirst;
    property Last: PIEntity read GetLast;
  end;

  IEntityList = interface (IEntities)
    ['{EA2EC901-D9B6-43FC-9CEB-82B0252752EF}']
    function Add(const Name: string; Kind: TEntityKind = ekObject; const Parent: IEntity = nil; Owner: TObject = nil): IEntity; overload; // base
    function Add(const Name: string; Kind: TClass; const Parent: IEntity = nil; Owner: TObject = nil): IEntity; overload; // base
    function Add(const Item: IEntity): Integer; overload;
    function Remove(const Item: IEntity): Integer;
  end;

  IEntity = interface (IObject)
    ['{FBFE7B93-6D2F-4C3B-AEA1-2942AF009C2B}']
    function GetModel: IModel;
    function GetOwner: TObject;
    function GetKind: TEntityKind;
    function GetParent: IEntity;
    procedure SetParent(const Value: IEntity);
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetRules: IRuleList;
    function GetTags: ITagList;
    function GetObjects: IInterfaces;
    function GetChildren: IEntities;
    property Owner: TObject read GetOwner;
    property Name: string read GetInstanceName;
    property Kind: TEntityKind read GetKind;
    property Model: IModel read GetModel;
    property Parent: IEntity read GetParent write SetParent;
    property Data: Pointer read GetData write SetData; // generico
    property Rules: IRuleList read GetRules;
    property Tags: ITagList read GetTags;
    property Objects: IInterfaces read GetObjects;
    property Children: IEntities read GetChildren;
  end;

  TRuleKind = (
    rkExternal,
    rkMethod,
    rkCalculation,
    rkTrigger );

  IRules = interface (ICollection)
    ['{5713FCFA-0DEF-4276-8920-1CB6CEB9E970}']
    function GetFirst: PIRule;    
    function GetLast: PIRule;    
    function GetItem(Index: Integer): IRule;
    function GetRule(const Name: string): IRule;
    property First: PIRule read GetFirst;
    property Last: PIRule read GetLast;
    property Item[Index: Integer]: IRule read GetItem;
    property Rule[const Name: string]: IRule read GetRule; default;
  end;

  IRuleList = interface (IRules)
    ['{9FC1C33F-6532-49A0-81B6-BC6C9EECD87C}']
    function Add(const Name: string; Kind: TRuleKind; const Defines: ITag = nil; const Depends: ITags = nil; Owner: TObject = nil): IRule; overload;
    function Add(const Name: string; const Defines: ITag = nil; const Depends: ITags = nil; Owner: TObject = nil): IRule; overload;
  end;

  IRule = interface (IObject)
    ['{1FD37C58-4247-48F1-BD2D-FD0FD12F5D4B}']
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetDefines: ITag;
    procedure SetDefines(const Value: ITag);
    function GetDepends: ITags;
    procedure SetDepends(const Value: ITags);
    procedure Update;
    property Name: string read GetInstanceName;
    property Data: Pointer read GetData write SetData; // generico
    property Defines: ITag read GetDefines write SetDefines;
    property Depends: ITags read GetDepends write SetDepends;
  end;

  IEvRuleEvaluate = interface
    ['{20134FE2-EF41-482D-857D-B91B36D00B14}']
    procedure OnRuleEvaluate(const Sender: IRule);
  end;

  TTagKind = (
    tkUndefined,
    tkBoolean,
    tkInteger,
    tkFloat,
    tkString,
    tkVariant,
    tkPointer,
    tkObject,
    tkInterface,
    tkHolor );

  ITags = interface (ICollection)
    ['{6944BCEA-AAB4-4BFF-B949-AD4003219A81}']
    function GetFirst: PITag;    
    function GetLast: PITag;    
    function GetItem(Index: Integer): ITag;
    function GetTag(const Name: string): ITag;
    function Lookup(const Name: string; out Tag: ITag): Boolean;
    function Enumerate(var Enum; out Tag: ITag): Boolean; overload;
    function Enumerate(var Enum; out Name: string): Boolean; overload;
    function NewList(const Name: string; Owner: TObject = nil): ITagList;
    function Slice(const Tags: array of ITag; const Name: string = ''; Owner: TObject = nil): ITags;
    property Name: string read GetInstanceName;
    property First: PITag read GetFirst;
    property Last: PITag read GetLast;
    property Count: Integer read GetCount;
    property Item[Index: Integer]: ITag read GetItem;
    property Tag[const Name: string]: ITag read GetTag; default;
  end;

  ITagList = interface (ITags)
    ['{108EF180-8879-46AC-86B5-DCFB2E1CCFE9}']
    function Add(const Name: string; Kind: TTagKind = tkUndefined; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Boolean; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Integer; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Double; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: string; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Pointer; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: IObject; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: IInterface; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: IHolor; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Remove(const Name: string): Integer; overload;
    function Remove(const Tag: ITag): Integer; overload;
    procedure Remove(Index: Integer); overload;
  end;

  ITag = interface
    ['{1CB25AB2-222F-49D4-96C0-84B2BEB54AD5}']
    function GetName: String;
    function GetOwner: TObject;
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetKind: TTagKind;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    function GetRule: IRule;
    function GetText: String;
    function GetIsCalculated: Boolean;
    function SetRule(const Name: string; Kind: TRuleKind; const Depends: ITags = nil; Owner: TObject = nil): IRule; overload;
    function SetRule(const Name: string; const Depends: ITags = nil; Owner: TObject = nil): IRule; overload;
    procedure SetRule(const Value: IRule); overload;
    property Name: string read GetName;
    property Owner: TObject read GetOwner;
    property Kind: TTagKind read GetKind;
    property Data: Pointer read GetData write SetData; // generico
    property Rule: IRule read GetRule;
    property Value: Variant read GetValue write SetValue;
    property IsCalculated: Boolean read GetIsCalculated;
    property Text: String read GetText;
  end;

  ITagBoolean = interface(ITag)
    ['{6428590C-4FC5-4D2A-9404-E64B9B88017A}']
    function GetBoolean: PBoolean;
    property VBoolean: PBoolean read GetBoolean;
  end;

  ITagInteger = interface(ITag)
    ['{35BC4CAC-6F30-4193-AE26-3EE190B7F439}']
    function GetInteger: PInteger;
    property VInteger: PInteger read GetInteger;
  end;

  ITagFloat = interface(ITag)
    ['{1F6827B3-50CD-4838-A6EB-A32AC290FB22}']
    function GetFloat: PDouble;
    property VFloat: PDouble read GetFloat;
  end;

  ITagString = interface(ITag)
    ['{22C703CD-7265-443A-9B5A-56030ABD7851}']
    function GetString: PString;
    property VString: PString read GetString;
  end;

  ITagVariant = interface(ITag)
    ['{114848FA-F771-4C3A-B1FF-701A94688303}']
    function GetVariant: PVariant;
    property VVariant: PVariant read GetVariant;
  end;

  ITagPointer = interface(ITag)
    ['{49115592-75B1-48EC-8772-83A8A7CFDE04}']
    function GetPointer: PPointer;
    property VPointer: PPointer read GetPointer;
  end;

  ITagInterface = interface(ITag)
    ['{CB2B9DB9-B1C6-4695-B40E-4852BE6D021A}']
    function GetInterface: PIInterface;
    property VInterface: PIInterface read GetInterface;
  end;

  ITagObject = interface(ITag)
    ['{89698B67-5641-43D7-8DDA-F9DD7A7CE735}']
    function GetObject: PIObject;
    property VObject: PIObject read GetObject;
  end;

  ITagHolor = interface(ITag)
    ['{B193778B-C595-4547-A304-66B7ABA07085}']
    function GetHolor: PIHolor;
    property VHolor: PIHolor read GetHolor;
  end;

  IEvTagChanged = interface
    ['{A29E5CD6-03B7-4E8E-9BE5-41A91BAEF82D}']
    procedure OnTagChanged(const Sender: ITag);
  end;

  Motor = class
    class function Tags(const Data: array of ITag): TagList;
    class function Rules(const Data: array of IRule): RuleList;
  end;

procedure SetReference(Instance: TObject; const Name: string; Value: Pointer);
  
implementation

procedure SetReference(Instance: TObject; const Name: string; Value: Pointer);
var
  Field: PPointer;
begin
  if Assigned(Instance) then
  begin
    Field := Instance.FieldAddress(Name);
    if Assigned(Field) then
      Field^ := Value
    else if Typ.Prop.Exists(Instance, Name) then
      Typ.Prop.Value(Instance, Name, Integer(Value));
  end;
end;

class function Motor.Rules(const Data: array of IRule): RuleList;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := Low(Data) to High(Data) do
    Result[I] := Data[I];
end;

class function Motor.Tags(const Data: array of ITag): TagList;
var
  I: Integer;
begin
  SetLength(Result, Length(Data));
  for I := Low(Data) to High(Data) do
    Result[I] := Data[I];
end;

end.
