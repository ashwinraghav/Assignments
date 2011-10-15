unit SilSiMotor;

interface

uses
  Sil, SilSiLists, SilSiHolors, SilSiCalculus;

(*
  dudas:
    1. Como definir una Rule que sea una simple operacion matematica entre Tags?
    2. Como asociar Tags con diferentes Layers? Es necesario?
    3. Es correcta la representacion de vectores y matrices?
    4. Algoritmos: deben estar en el Motor o en la logica externa?
         Quizá deba haber un repositorio de algoritmos tipicos, tale como
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
  ITag = interface;


  TagList = array of ITag;
  RuleList = array of IRule;
  Entities = array of IEntity; 

  IModel = interface
    ['{73A15B42-8792-43CC-BC66-502F5A4BB193}']
    function GetEntities: IEntityList;
    function GetRules: IRuleList;
    function GetTags: ITags;
    property Entities: IEntityList read GetEntities;
    property Rules: IRuleList read GetRules;
    property Tags: ITags read GetTags;
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
    function Lookup(const Name: string): IEntity; overload;
    function Enumerate(var Enum; out Item: IEntity): Boolean; overload;
    function Enumerate(var Enum; out Name: String): Boolean; overload;
  end;

  IEntityList = interface (IEntities)
    ['{EA2EC901-D9B6-43FC-9CEB-82B0252752EF}']
    function Add(const Name: string; Kind: TEntityKind = ekObject; const Parent: IEntity = nil; Owner: TObject = nil): IEntity; overload; // base
    function Add(const Name: string; Kind: TClass; const Parent: IEntity = nil; Owner: TObject = nil): IEntity; overload; // base
  end;

  PIEntity = ^IEntity;
  IEntity = interface (IObject)
    ['{FBFE7B93-6D2F-4C3B-AEA1-2942AF009C2B}']
    function GetParent: IEntity;
    procedure SetParent(const Value: IEntity);
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetRules: IRuleList;
    function GetTags: ITags;
    function GetObjects: IInterfaces;
    function GetChildren: IEntities;
    property Name: string read GetInstanceName;
    property Parent: IEntity read GetParent write SetParent;
    property Data: Pointer read GetData write SetData; // generico
    property Rules: IRuleList read GetRules;
    property Tags: ITags read GetTags;
    property Objects: IInterfaces read GetObjects;
    property Children: IEntities read GetChildren;
  end;

  TRuleKind = (
    rkExternal,
    rkMethod,
    rkCalculation,
    rkTrigger );

  IRules = interface (IObjects)
    ['{5713FCFA-0DEF-4276-8920-1CB6CEB9E970}']
    function GetItem(Index: Integer): IRule;
    function GetRule(const Name: string): IRule;
    function GetDepends: ITags;
    property Item[Index: Integer]: IRule read GetItem;
    property Rule[const Name: string]: IRule read GetRule; default;
    property Depends: ITags read GetDepends;
  end;

  IRuleList = interface (IRules)
    ['{9FC1C33F-6532-49A0-81B6-BC6C9EECD87C}']
    function Add(const Name: string; Kind: TRuleKind = rkExternal): IRule; overload;
    function Add(const Name: string; const Depends: TagList; Kind: TRuleKind = rkExternal): IRule; overload;
  end;

  IRule = interface (IObject)
    ['{1FD37C58-4247-48F1-BD2D-FD0FD12F5D4B}']
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    property Name: string read GetInstanceName;
    property Data: Pointer read GetData write SetData; // generico
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
    function GetItem(Index: Integer): ITag;
    function GetTag(const Name: string): ITag;
    function Add(const Name: string; Kind: TTagKind = tkUndefined; const Rule: IRule = nil; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Boolean; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Integer; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Double; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: string; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: Pointer; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: IObject; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: IInterface; Owner: TObject = nil): ITag; overload;
    function Add(const Name: string; const Value: IHolor; Owner: TObject = nil): ITag; overload;
    function Remove(const Name: string): Integer; overload;
    function Remove(const Tag: ITag): Integer; overload;
    procedure Remove(Index: Integer); overload;
    function Lookup(const Name: string; out Tag: ITag): Boolean;
    function Enumerate(var Enum; out Tag: ITag): Boolean; overload;
    function Enumerate(var Enum; out Name: string): Boolean; overload;
    function NewList(const Name: string; Owner: TObject = nil): ITags;  
    property Count: Integer read GetCount;
    property Item[Index: Integer]: ITag read GetItem;
    property Tag[const Name: string]: ITag read GetTag; default;
  end;

  PITag = ^ITag;
  ITag = interface
    ['{1CB25AB2-222F-49D4-96C0-84B2BEB54AD5}']
    function GetName: String;
    function GetData: Pointer;
    procedure SetData(const Value: Pointer);
    function GetKind: TTagKind;
    function GetIsNull: Boolean;
    procedure SetIsNull(const Value: Boolean);
    function GetIsModified: Boolean;
    procedure SetIsModified(const Value: Boolean);
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    property Name: string read GetName;
    property Kind: TTagKind read GetKind;
    property Data: Pointer read GetData write SetData; // generico
    property IsNull: Boolean read GetIsNull write SetIsNull;
    property IsModified: Boolean read GetIsModified write SetIsModified;
    property Value: Variant read GetValue write SetValue;
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
