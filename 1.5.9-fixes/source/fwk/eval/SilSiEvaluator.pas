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

unit SilSiEvaluator;

interface

uses
  Sil,
  SilLiTypeInfo,
  SilTokens;

type
  IEvaluator = interface;
  IEvalMachine = interface;
  ITokenHandler = interface;
  IEvaluationStack = interface;
  IEvaluationItem = interface;
  IEvalOperator = interface;
  IEvalInstructions = interface;
  IEvalInstructionList = interface;
  IEvalInstruction = interface;
  IEvalLabel = interface;
  IEvalFunctionFrame = interface;

  TSilOperatorKind  = class of TSilOperatorType;

  TInstructionKind  = (ikNop, ikPush, ikPop, ikCopy, ikOperator, ikJump, ikLabel);

  TGotoCondition    = (goInconditional, goIfTrue, goIfFalse);

  IEvaluator = interface
    ['{0CAC5084-B1D7-44B6-976A-776BFB14053B}']
    function GetInstructions: IEvalInstructionList;
    function Execute: Variant;
    property Instructions: IEvalInstructionList read GetInstructions;
  end;

  IEvalMachine = interface (IEnumerator)
    ['{CA443275-FD71-4A51-B833-D1CB8C3DCBFB}']
    procedure JumpTo(const Target: IEvalLabel);
  end;

  ITokenHandler = interface
    ['{261CF1BF-C114-4C25-9DFA-99EF1D7B1806}']
    function GetTokenValue(const Token: IToken): Variant;
    procedure SetTokenValue(const Token: IToken; const Value: Variant);
  end;

  IEvaluationStack = interface
    ['{6C817D00-E6BF-4A58-9185-B995D7F7C082}']
    function GetCount: Integer;
    function GetTop: IEvaluationItem;
    procedure PushValue(const Value: Variant);
    procedure PushToken(const Token: IToken);
    procedure PushObject(const Instance: IUnknown);
    procedure PushItem(const Item: IEvaluationItem);
    function Pop: IEvaluationItem;
    property Count: Integer read GetCount;
    property Top: IEvaluationItem read GetTop;
  end;

  TItemKind = (ikValue, ikToken, ikObject);

  IEvaluationItem = interface
    ['{CD85C3C8-7C51-4916-BE24-ABCCC4822374}']
    function GetKind: TItemKind;
    function GetAsValue: Variant;
    procedure SetAsValue(const Value: Variant);
    function GetAsToken: IToken;
    function GetAsObject: IUnknown;
    property Kind: TItemKind read GetKind;
    property AsValue: Variant read GetAsValue write SetAsValue;
    property AsToken: IToken read GetAsToken;
    property AsObject: IUnknown read GetAsObject;
  end;

  IEvalOperator = interface
    ['{B2D622EA-62AF-44AA-AD8B-4AC94C7618EB}']
    function GetName: string;
    function GetKind: TSilOperatorKind;
    procedure Execute(const Stack: IEvaluationStack);
    property Name: string read GetName;
    property Kind: TSilOperatorKind read GetKind;
  end;
  
  IEvalInstructions = interface
    ['{AA51D04E-948E-450E-BC7C-483EA3A91C26}']
    function GetCount: Integer;
    function IndexOf(const Item: IEvalInstruction): Integer;
    function Enumerate(var Ptr: IEvalMachine; out Item: IEvalInstruction): Boolean;
    property Count: Integer read GetCount;
  end;

  IEvalInstructionList = interface (IEvalInstructions)
    ['{DDF35C96-9C01-4CF9-969B-AA01AA9320A6}']
    procedure Clear;
    function DefineLabel(const Name: string): IEvalLabel;
    function AddNop(const Text: string): IEvalInstruction;
    function AddPush(const Data: IToken): IEvalInstruction;
    function AddPop: IEvalInstruction;
    function AddCopy: IEvalInstruction;
    function AddOperator(Kind: TSilOperatorKind; const Name: string; const LType: ITypeInfo = nil; const RType: ITypeInfo = nil): IEvalInstruction; 
    function AddJump(const Target: IEvalLabel; const Condition: TGotoCondition = goInconditional): IEvalInstruction;
    function AddLabel(const Name: string): IEvalLabel;
    function PlaceLabel(const Item: IEvalLabel): IEvalLabel;
    function Remove(const Item: IEvalInstruction): Integer;
  end;

  IEvalInstruction = interface
    ['{8FFD27FF-6643-4E65-A78A-F598EEC9316E}']
    function GetKind: TInstructionKind;
    function GetName: string;
    function GetAsOperator: IEvalOperator;
    function GetAsToken: IToken;
    function GetAsTarget: IEvalLabel;
    procedure Execute(const Machine: IEvalMachine; const Stack: IEvaluationStack);
    function ToStr: string;
    property Kind: TInstructionKind read GetKind;
    property Name: string read GetName;
    property AsToken: IToken read GetAsToken;
    property AsOperator: IEvalOperator read GetAsOperator;
    property AsTarget: IEvalLabel read GetAsTarget;
  end;

  IEvalLabel = interface (IEvalInstruction)
    ['{B8C2FD69-25E7-4793-A168-71778E8B9769}']
  end;

  TSilOperatorType = class(TSilObject)
  protected
    procedure Execute(const Stack: IEvaluationStack); virtual; abstract; 
  public
    constructor Create(const Name: string; const LType: ITypeInfo = nil; const RType: ITypeInfo = nil); overload; virtual; abstract; 
  end;

  RFunctionMsg = record
    ID: Integer;
    Frame: IEvalFunctionFrame;
    Return: PVariant;
    Result: Integer;
  end;
    
  IEvalFunctionFrame = interface
    ['{524D68F9-5AC4-46E9-A1D6-E71C17E07CBC}']
    function GetParams: IParameterList;
    function GetScope: ITokenTable;
    procedure Add(const Value: Variant);
    function Invoke: Variant;
    property Params: IParameterList read GetParams;
    property Scope: ITokenTable read GetScope;
  end;

  TIdentifierKind = (ikConstant, ikVariable, ikFunction);

implementation
end.
