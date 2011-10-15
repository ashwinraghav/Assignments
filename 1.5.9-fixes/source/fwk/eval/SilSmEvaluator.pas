{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    antiriad@gmail.com                 *
 *     Copyright (C) 2000 Leandro Conde      lconde@str.com.ar                  *
 *     Copyright (C) 2000 Lisandro Podestá   lisandrop@movi.com.ar              *
 *                                                                              *
 *     See License.txt for details.                                             *
 *                                                                              *
 *   This library is free software; you can redistribute it and/or              *
 *   modify it under the terms of the GNU Lesser General Public                 *
 *   License as published by the Free Software Foundation; either               *
 *   version 2.1 of the License, or (at your option) any later version.         *
 *                                                                              *
 *   This library is distributed in the hope that it will be useful,            *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 *   Lesser General Public License for more details.                            *
 *                                                                              *
 *   You should have received a copy of the GNU Lesser General Public           *
 *   License along with this library; if not, write to the Free Software        *
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                              *
 ********************************************************************************}

unit SilSmEvaluator;

{$I Defines.inc}

interface

uses
  Sil,

  SilLmInterfaceList,
  SilLiTypeInfo,
  SilTokens,
  SilSiEvaluator;

type
  TSilInstruction = class;
  TSilLabelInstruction = class;

  TSilEvaluator = class(
    TSilObject,
    IEvaluator )
  private
    FHook: Pointer;
    FInstructions: IEvalInstructionList;
  private
    function DoGetHook: ITokenHandler;
    function DoGetValue(const Tok: IToken): Variant;
    procedure DoSetValue(const Tok: IToken; const Value: Variant);
  protected
    function GetInstructions: IEvalInstructionList;
    function Execute: Variant;
  public
    constructor Create(const Hook: ITokenHandler);
    destructor Destroy; override;
    property Hook: ITokenHandler read DoGetHook;
  end;

  TSilEvaluationStack = class(
    TSilInterfaceList,
    IEvaluationStack )
  private
    FOwner: TSilEvaluator;
  protected //- IEvaluationStack
    function GetTop: IEvaluationItem;
    procedure PushValue(const Value: Variant);
    procedure PushToken(const Tok: IToken);
    procedure PushObject(const Instance: IUnknown);
    procedure PushItem(const Item: IEvaluationItem);
    function Pop: IEvaluationItem;
  public
    constructor Create(Owner: TSilEvaluator); reintroduce;
    destructor Destroy; override;
  public
    property Top: IEvaluationItem read GetTop;
    property Owner: TSilEvaluator read FOwner;
  end;

  TSilEvaluationItem = class(TSilObject)
  private
    FStack: TSilEvaluationStack;
  public
    constructor Create(Stack: TSilEvaluationStack);
    destructor Destroy; override;
    property Stack: TSilEvaluationStack read FStack; 
  end;
  
  TSilEvaluationValue = class(
    TSilEvaluationItem,
    IEvaluationItem )
  private
    FValue: Variant;
  protected //-
    function GetKind: TItemKind;
    function GetAsValue: Variant;
    procedure SetAsValue(const Value: Variant);
    function GetAsToken: IToken;
    function GetAsObject: IUnknown;
  public
    constructor Create(Stack: TSilEvaluationStack; const Value: Variant);
    property AsValue: Variant read GetAsValue write SetAsValue;
  end;

  TSilEvaluationToken = class(
    TSilEvaluationItem,
    IEvaluationItem )
  private
    FToken: IToken;
  protected //-
    function GetKind: TItemKind;
    function GetAsValue: Variant;
    procedure SetAsValue(const Value: Variant);
    function GetAsToken: IToken;
    function GetAsObject: IUnknown;
  public
    constructor Create(Stack: TSilEvaluationStack; const Tok: IToken);
  end;

  TSilEvaluationObject = class(
    TSilEvaluationItem,
    IEvaluationItem )
  private
    FInstance: IUnknown;
  protected //-
    function GetKind: TItemKind;
    function GetAsValue: Variant;
    procedure SetAsValue(const Value: Variant);
    function GetAsToken: IToken;
    function GetAsObject: IUnknown;
  public
    constructor Create(Stack: TSilEvaluationStack; const Instance: IUnknown);
  end;

  TSilInstructionList = class(
    TSilInterfaceList,
    IEvalInstructions,
    IEvalInstructionList )
  private
    FLabels: IStringList;
  private
    function DoAdd(Instance: TSilInstruction): TSilInstruction;
    function DoNewLabel(const Name: string): string;
    function DoAddLabel(Instance: TSilLabelInstruction): TSilLabelInstruction;
    function DoDefineLabel(const Name: string): TSilLabelInstruction;
    function DoPlaceLabel(Instance: TSilLabelInstruction): TSilLabelInstruction;
    function DoGetEnumerator(out Enum: IEvalMachine; Locked: Boolean): Boolean;
  protected // IEvalInstructions
    function IndexOf(const Item: IEvalInstruction): Integer; reintroduce;
    function Enumerate(var Ptr: IEvalMachine; out Item: IEvalInstruction): Boolean; reintroduce;
  protected // IEvalInstructionList
    function DefineLabel(const Name: string): IEvalLabel;
    function AddNop(const Text: string): IEvalInstruction;
    function AddPush(const Data: IToken): IEvalInstruction;
    function AddPop: IEvalInstruction;
    function AddCopy: IEvalInstruction;
    function AddOperator(Kind: TSilOperatorKind; const Name: string; const LType: ITypeInfo = nil; const RType: ITypeInfo = nil): IEvalInstruction; 
    function AddJump(const Target: IEvalLabel; const Condition: TGotoCondition = goInconditional): IEvalInstruction;
    function AddLabel(const Name: string): IEvalLabel;
    function PlaceLabel(const Item: IEvalLabel): IEvalLabel;
    function Remove(const Item: IEvalInstruction): Integer; reintroduce;
  public
    constructor Create(const Evaluator: TSilEvaluator);
    destructor Destroy; override;
  end;

  TSilInstruction = class(
    TSilObject,
    IReferenceable,
    IEvalInstruction )
  private
    FKind: TInstructionKind;
    FData: IUnknown;
    FName: string;
  protected // IEvalInstruction
    function GetKind: TInstructionKind;
    function GetName: string; virtual;
    function GetAsOperator: IEvalOperator;
    function GetAsToken: IToken;
    function GetAsTarget: IEvalLabel;
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); virtual; abstract;
    function ToStr: string; virtual;
  public
    constructor Create(Kind: TInstructionKind; const Data: IUnknown = nil; const Name: string = '');
    destructor Destroy; override;
    property Name: string read GetName;
    property AsToken: IToken read GetAsToken;
    property AsOperator: IEvalOperator read GetAsOperator;
    property AsTarget: IEvalLabel read GetAsTarget;
  end;

  TSilPushInstruction = class(TSilInstruction)
  protected 
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
    function ToStr: string; override; 
  public
    constructor Create(const Tok: IToken);
  end;

  TSilNopInstruction = class(TSilInstruction)
  private
    FText: string;
  protected 
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
    function ToStr: string; override; 
  public
    constructor Create(const Text: string);
  end;

  TSilPopInstruction = class(TSilInstruction)
  protected 
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
  public
    constructor Create;
  end;

  TSilCopyInstruction = class(TSilInstruction)
  protected
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
  public
    constructor Create;
  end;

  TSilOperatorInstruction = class(TSilInstruction)
  protected
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
    function ToStr: string; override; 
  public
    constructor Create(const Operator: IEvalOperator);
  end;

  TSilLabelInstruction = class(
    TSilInstruction,
    IEvalLabel )
  protected 
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
    function ToStr: string; override;
  public
    constructor Create(const Name: string);
  end;

  TSilGotoInstruction = class(TSilInstruction)
  private
    FCondition: TGotoCondition;
  protected
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack); override;
    function GetName: string; override;
    function ToStr: string; override; 
  public
    constructor Create(Condition: TGotoCondition; const Target: IEvalLabel = nil; const Name: string = '');
  end;
    
  TSilOperator = class(
    TSilOperatorType,
    IEvalOperator )
  private
    FName: string;
  protected // IEvalOperator
    function GetName: string;
    function GetKind: TSilOperatorKind;
    procedure Execute(const Stack: IEvaluationStack); override;
  public
    constructor Create(const Name: string; const LType, RType: ITypeInfo); override;
    destructor Destroy; override;
  end;

  TSilUnaryOp = class(TSilOperator)
  protected //-
    procedure Execute(const Stack: IEvaluationStack); overload; override;
  protected
    function Execute(const Item: IEvaluationItem): Variant; reintroduce; overload; virtual; abstract;
  end;
  
  TSilBinaryOp = class(TSilOperator)
  protected //-
    procedure Execute(const Stack: IEvaluationStack); overload; override;
  protected
    function Execute(const Left, Right: IEvaluationItem): Variant; reintroduce; overload; virtual; abstract;
  end;

  TSilFunctionFrame = class(
    TSilObject,
    IEvalFunctionFrame )
  private
    FFunction: IToken;
    FScope: ITokenTable;
    FParams: IParameterList;
    FObject: IDispatchable;
    FMessage: Integer;
  protected // IEvalFunctionFrame
    function GetParams: IParameterList;
    function GetScope: ITokenTable;
    procedure Add(const Value: Variant);
    function Invoke: Variant;
  public
    constructor Create(const Tok: IToken);
    destructor Destroy; override; 
  end;
  
implementation

uses
  SilLmListEnumerator;

type
  TSilEvalMachine = class(
    TListEnumerator,
    IEvalMachine )
  private
    function DoGetContainer: IEvalInstructions;
  protected //- IEvalMachine
    procedure JumpTo(const Target: IEvalLabel);
  public
    property Container: IEvalInstructions read DoGetContainer;
  end;

{ TSilEvaluator }

constructor TSilEvaluator.Create(const Hook: ITokenHandler);
begin
  inherited Create;
  FInstructions := TSilInstructionList.Create(Self); 
  FHook := Pointer(Hook);
end;

destructor TSilEvaluator.Destroy;
begin
  FInstructions := nil;
  FHook := nil;
  inherited;
end;

function TSilEvaluator.Execute: Variant;
var
  Enum: IEvalMachine;
  Item: IEvalInstruction;
  Stack: IEvaluationStack;
begin
  Stack := TSilEvaluationStack.Create(Self); 
  with FInstructions do
    while Enumerate(Enum, Item) do
      Item.Execute(Enum, Stack);
  if Stack.Count > 0 then
    Result := Stack.Top.AsValue else
    Result := Sil.Vart.Unassigned;
end;

function TSilEvaluator.DoGetHook: ITokenHandler;
begin
  Result := ITokenHandler(FHook);
end;

function TSilEvaluator.GetInstructions: IEvalInstructionList;
begin
  Result := FInstructions;
end;

function TSilEvaluator.DoGetValue(const Tok: IToken): Variant;
var
  Data: Variant;
  Kind: TIdentifierKind;
begin
  if Token.Has(Tok.Symbol, ttIdentifier) then
  begin
    Data := Tok.Data['kind'];
    if Vart.IsOK(Data) then
      Kind := Data else
      Kind := ikVariable;      
    case Kind of
      ikConstant:   Result := Tok.Value;
      else          Result := Hook.GetTokenValue(Tok);
    end;
  end else
    Result := Tok.Value; 
end;

procedure TSilEvaluator.DoSetValue(const Tok: IToken; const Value: Variant);
begin
  if Token.Has(Tok.Symbol, ttIdentifier) then
    Hook.SetTokenValue(Tok, Value) else
    Tok.Value := Value; 
end;                   

{ TSilEvaluationStack }

constructor TSilEvaluationStack.Create(Owner: TSilEvaluator);
begin
  inherited Create(False, VariantHandler);
  FOwner := Owner;
end;

destructor TSilEvaluationStack.Destroy;
begin
  FOwner := nil;
  inherited;
end;

procedure TSilEvaluationStack.PushToken(const Tok: IToken);
begin
  inherited Add(TSilEvaluationToken.Create(Self, Tok));
end;

procedure TSilEvaluationStack.PushObject(const Instance: IInterface);
begin
  inherited Add(TSilEvaluationObject.Create(Self, Instance));
end;

procedure TSilEvaluationStack.PushItem(const Item: IEvaluationItem);
begin
  inherited Add(Item);
end;

procedure TSilEvaluationStack.PushValue(const Value: Variant);
begin
  inherited Add(TSilEvaluationValue.Create(Self, Value));
end;

function TSilEvaluationStack.Pop: IEvaluationItem;
begin
  if Count > 0 then
  begin
    Result := GetTop();
    inherited Delete(Count - 1);
  end;
end;

function TSilEvaluationStack.GetTop: IEvaluationItem;
begin
  Result := Last as IEvaluationItem;
end;

{ TSilEvaluationItem }

constructor TSilEvaluationItem.Create(Stack: TSilEvaluationStack);
begin
  inherited Create;
  FStack := Stack;
end;

destructor TSilEvaluationItem.Destroy;
begin
  FStack := nil;
  inherited;
end;

{ TSilEvaluationValue }

constructor TSilEvaluationValue.Create(Stack: TSilEvaluationStack; const Value: Variant);
begin
  inherited Create(Stack);
  FValue := Value;
end;

function TSilEvaluationValue.GetAsObject: IUnknown;
begin
  raise Sil.Error.Create('TSilEvaluationValue: invalid conversion -> Value to Object');
end;

function TSilEvaluationValue.GetAsToken: IToken;
begin
  raise Sil.Error.Create('TSilEvaluationValue: invalid conversion -> Value to Token');
end;

function TSilEvaluationValue.GetAsValue: Variant;
begin
  Result := FValue;
end;

function TSilEvaluationValue.GetKind: TItemKind;
begin
  Result := ikValue;
end;

procedure TSilEvaluationValue.SetAsValue(const Value: Variant);
begin
  raise Sil.Error.Create('TSilEvaluationValue: read only item');
end;

{ TSilEvaluationToken }

constructor TSilEvaluationToken.Create(Stack: TSilEvaluationStack; const Tok: IToken);
begin
  inherited Create(Stack);
  FToken := Tok;
end;

function TSilEvaluationToken.GetAsToken: IToken;
begin
  Result := FToken;
end;

function TSilEvaluationToken.GetAsObject: IUnknown;
begin
  Result := FToken;
end;

function TSilEvaluationToken.GetAsValue: Variant;
begin
  Result := FStack.Owner.DoGetValue(FToken);
end;

procedure TSilEvaluationToken.SetAsValue(const Value: Variant);
begin
  FStack.Owner.DoSetValue(FToken, Value);
end;

function TSilEvaluationToken.GetKind: TItemKind;
begin
  Result := ikToken;
end;

{ TSilEvaluationObject }

constructor TSilEvaluationObject.Create(Stack: TSilEvaluationStack; const Instance: IInterface);
begin
  inherited Create(Stack);
  FInstance := Instance;
end;

function TSilEvaluationObject.GetAsObject: IUnknown;
begin
  Result := FInstance;
end;

function TSilEvaluationObject.GetAsToken: IToken;
begin
  raise Sil.Error.Create('TSilEvaluationValue: invalid conversion -> Object to Token');
end;

function TSilEvaluationObject.GetAsValue: Variant;
begin
  raise Sil.Error.Create('TSilEvaluationValue: invalid conversion -> Object to Value');
end;

function TSilEvaluationObject.GetKind: TItemKind;
begin
  Result := ikObject;
end;

procedure TSilEvaluationObject.SetAsValue(const Value: Variant);
begin
end;

{ TSilInstructionList }

constructor TSilInstructionList.Create(const Evaluator: TSilEvaluator);
begin
  inherited Create;
  FLabels := Sil.List.StringList();
end;

destructor TSilInstructionList.Destroy;
begin
  FLabels := nil;
  inherited;
end;

function TSilInstructionList.IndexOf(const Item: IEvalInstruction): Integer;
begin
  Result := inherited IndexOf(Item);
end;

function TSilInstructionList.Enumerate(var Ptr: IEvalMachine; out Item: IEvalInstruction): Boolean;
begin
  if Ptr <> nil then
    Result := Ptr.Next
  else if DoGetEnumerator(Ptr, Lockable <> nil) then
    Result := Ptr.HasMore
  else
    Result := false;

  if Result then
  begin
    if Ptr.Get(Item) then
      Item := Item as IEvalInstruction;
  end else
    Ptr := nil;
end;

function TSilInstructionList.DefineLabel(const Name: string): IEvalLabel;
begin
  Result := DoDefineLabel(Name);
end;

function TSilInstructionList.AddNop(const Text: string): IEvalInstruction;
begin
  Result := DoAdd(TSilNopInstruction.Create(Text));
end;

function TSilInstructionList.AddPush(const Data: IToken): IEvalInstruction;
begin
  Result := DoAdd(TSilPushInstruction.Create(Data));
end;

function TSilInstructionList.AddPop: IEvalInstruction;
begin
  Result := DoAdd(TSilPopInstruction.Create());
end;

function TSilInstructionList.AddCopy: IEvalInstruction;
begin
  Result := DoAdd(TSilCopyInstruction.Create());
end;

function TSilInstructionList.AddOperator(Kind: TSilOperatorKind; const Name: string; const LType, RType: ITypeInfo): IEvalInstruction;
begin
  Result := DoAdd(TSilOperatorInstruction.Create(Kind.Create(Name, LType, RType) as IEvalOperator));
end;

function TSilInstructionList.AddJump(const Target: IEvalLabel; const Condition: TGotoCondition): IEvalInstruction;
begin
  Result := DoAdd(TSilGotoInstruction.Create(Condition, Target, Target.Name));
end;

function TSilInstructionList.AddLabel(const Name: string): IEvalLabel;
begin
  Result := DoPlaceLabel(DoDefineLabel(Name));
end;

function TSilInstructionList.PlaceLabel(const Item: IEvalLabel): IEvalLabel;
begin
  Result := DoPlaceLabel(Sil.Ref.GetInstance(Item))
end;

function TSilInstructionList.Remove(const Item: IEvalInstruction): Integer;
begin
  if Item.Kind = ikLabel then FLabels.Delete(FLabels.IndexOf(Item.Name));
  Result := inherited Remove(Item);
end;

function TSilInstructionList.DoAdd(Instance: TSilInstruction): TSilInstruction;
begin
  Result := Instance;
  inherited Add(Result);
end;

function TSilInstructionList.DoNewLabel(const Name: string): string;
begin
  Result := Name + Sil.Int.ToStr(FLabels.Count + 1);
end;

function TSilInstructionList.DoAddLabel(Instance: TSilLabelInstruction): TSilLabelInstruction;
begin
  FLabels.Add(Instance.Name, Instance);
  Result := Instance;
end;

function TSilInstructionList.DoDefineLabel(const Name: string): TSilLabelInstruction;
begin
  Result := DoAddLabel(TSilLabelInstruction.Create(DoNewLabel(Name)));
end;

function TSilInstructionList.DoPlaceLabel(Instance: TSilLabelInstruction): TSilLabelInstruction;
begin
  Result := DoAdd(Instance) as TSilLabelInstruction;
end;

function TSilInstructionList.DoGetEnumerator(out Enum: IEvalMachine; Locked: Boolean): Boolean;
begin
  Result := Count > 0;
  if Result then Enum := TSilEvalMachine.Create(Self, Locked, FTypeHandler, FTypeData);
end;

{ TSilInstruction }

constructor TSilInstruction.Create(Kind: TInstructionKind; const Data: IInterface; const Name: string);
begin
  inherited Create;
  FKind := Kind;
  FData := Data;
  if Sil.Str.IsEmpty(Name) then
    FName := Sil.Typ.Enum.Name(TypeInfo(TInstructionKind), Ord(Kind), 'ik') else
    FName := Name;
end;

destructor TSilInstruction.Destroy;
begin
  FData := nil;
  inherited;
end;

function TSilInstruction.GetAsOperator: IEvalOperator;
begin
  Result := FData as IEvalOperator;
end;

function TSilInstruction.GetAsToken: IToken;
begin
  Result := FData as IToken;
end;

function TSilInstruction.GetAsTarget: IEvalLabel;
begin
  Result := FData as IEvalLabel;
end;

function TSilInstruction.GetName: string;
begin
  Result := FName;
end;

function TSilInstruction.GetKind: TInstructionKind;
begin
  Result := FKind;
end;

function TSilInstruction.ToStr: string;
begin
  Result := Sil.Str.ReplicateChar(' ', 16);
  Sil.Str.Add(Result, Sil.Str.ToLower(Name));
  Sil.Str.Add(Result, Sil.Str.ReplicateChar(' ', 32 - Length(Result)))
end;

{ TSilNopInstruction }

constructor TSilNopInstruction.Create(const Text: string);
begin
  inherited Create(ikNop);
  FText := Text;
end;

procedure TSilNopInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
begin
end;

function TSilNopInstruction.ToStr: string;
begin
  Result := FText;
end;

{ TSilPushInstruction }

constructor TSilPushInstruction.Create(const Tok: IToken);
begin
  inherited Create(ikPush, Tok);
end;

procedure TSilPushInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
begin
  Stack.PushToken(Self.AsToken);
end;

function TSilPushInstruction.ToStr: string;
begin
  Result := inherited ToStr;
  Sil.Str.Add(Result, AsToken.Lexema);
end;

{ TSilPopInstruction }

constructor TSilPopInstruction.Create;
begin
  inherited Create(ikPop);
end;

procedure TSilPopInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
begin
  Stack.Pop;
end;

{ TSilCopyInstruction }

constructor TSilCopyInstruction.Create;
begin
  inherited Create(ikCopy);
end;

procedure TSilCopyInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
begin
  Stack.PushItem(Stack.Top);  
end;

{ TSilOperatorInstruction }

constructor TSilOperatorInstruction.Create(const Operator: IEvalOperator);
begin
  inherited Create(ikOperator, Operator);
end;

procedure TSilOperatorInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
begin
  Self.AsOperator.Execute(Stack);
end;

function TSilOperatorInstruction.ToStr: string;
begin
  Result := inherited ToStr;
  Sil.Str.Add(Result, AsOperator.Kind.ClassName + '::' + AsOperator.Name);
end;

{ TSilLabelInstruction }

constructor TSilLabelInstruction.Create(const Name: string);
begin
  inherited Create(ikLabel, nil, Name);
end;

procedure TSilLabelInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
begin
end;

function TSilLabelInstruction.ToStr: string;
begin
  Result := Name + ':';
end;

{ TSilGotoInstruction }

constructor TSilGotoInstruction.Create(Condition: TGotoCondition; const Target: IEvalLabel; const Name: string);
begin
  inherited Create(ikJump, Target, 'jump');
  FCondition := Condition;  
end;

procedure TSilGotoInstruction.Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
var
  DoJump: Boolean;
begin
  case FCondition of
    goIfTrue:         DoJump := Integer(Stack.Pop.AsValue) <> 0;
    goIfFalse:        DoJump := Integer(Stack.Pop.AsValue) =  0;
    else              DoJump := True;
  end;
  if DoJump then Machine.JumpTo(Self.AsTarget);
end;

function TSilGotoInstruction.GetName: string;
begin
  Result := inherited GetName;
  case FCondition of
    goIfTrue:  Sil.Str.Add(Result, ' if true');
    goIfFalse: Sil.Str.Add(Result, ' if false');
  end;
end;

function TSilGotoInstruction.ToStr: string;
begin
  Result := inherited ToStr;
  Sil.Str.Add(Result, AsTarget.Name);
end;

{ TSilOperator }

constructor TSilOperator.Create(const Name: string; const LType, RType: ITypeInfo);
begin
  inherited Create;
  FName := Name;
end;

destructor TSilOperator.Destroy;
begin
  inherited;
end;

procedure TSilOperator.Execute(const Stack: IEvaluationStack);
begin
end;

function TSilOperator.GetKind: TSilOperatorKind;
begin
  Result := TSilOperatorKind(ClassType);
end;

function TSilOperator.GetName: string;
begin
  Result := FName;
end;

{ TSilBinaryOp }

procedure TSilBinaryOp.Execute(const Stack: IEvaluationStack);
var
  Left, Right: IEvaluationItem;
begin
  Right := Stack.Pop;
  Left := Stack.Pop;
  Stack.PushValue(Execute(Left, Right));
end;

{ TSilUnaryOp }

procedure TSilUnaryOp.Execute(const Stack: IEvaluationStack);
begin
  Stack.PushValue(Execute(Stack.Pop()));
end;

{ TSilFunctionFrame }

constructor TSilFunctionFrame.Create(const Tok: IToken);
begin
  inherited Create;
  FFunction := Tok;
  if FFunction.Data['function'] then
  begin
    Sil.Vart.ToInterface(FFunction.Data['scope'], ITokenTable, FScope);
    if not Sil.Vart.ToInterface(FFunction.Data['instance'], IDispatchable, FObject) then
      raise Sil.Error.Create('Function incorrecly defined: dispatchable object is missing');
    FMessage := Sil.Vart.ToInt(FFunction.Data['message'], -1);
    if FMessage = -1 then
      raise Sil.Error.Create('Function incorrecly defined: message id is missing');
  end else
    raise Sil.Error.Create('Attempt to call an undefined function: %s', [FFunction.Lexema]);
  FParams := Sil.List.Parameters();
end;

destructor TSilFunctionFrame.Destroy;
begin
  FParams := nil;
  FScope := nil;
  FFunction := nil;
  inherited;
end;

procedure TSilFunctionFrame.Add(const Value: Variant);
var
  Index: Integer;
  Name: string;
begin
  Index := FParams.Count;
  if Assigned(FScope) and FScope.ValidIndex(Index) then
    Name := FScope[Index].Lexema else
    Name := '$' + Int.ToStr(Index + 1);
  FParams[Name] := Value;
end;

function TSilFunctionFrame.Invoke: Variant;
var
  Msg: RFunctionMsg;
begin
  Msg.ID := FMessage;
  Msg.Frame := Self;
  Msg.Return := @Result;
  Msg.Result := 0;
  FObject.Dispatch(Msg);  
end;

function TSilFunctionFrame.GetParams: IParameterList;
begin
  Result := FParams;
end;

function TSilFunctionFrame.GetScope: ITokenTable;
begin
  Result := FScope;
end;

{ TSilEvalMachine }

procedure TSilEvalMachine.JumpTo(const Target: IEvalLabel);
begin
  FIteration := Container.IndexOf(Target);
end;

function TSilEvalMachine.DoGetContainer: IEvalInstructions;
begin
  Result := FContainer as IEvalInstructions;
end;

end.
