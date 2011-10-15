unit SilSiBlocks;

{$INCLUDE Defines.inc}

interface

uses
  Sil, SilSiLists, SilSiHolors;

type
  IBlockNode = interface;
  IBlockOperator = interface;
  IBlockOperand = interface;
  IBlockOperands = interface;

  IBlockNode = interface
    ['{9F13B155-038B-4540-8791-0C5C3CDA50ED}']
    function GetParent: IBlockNode;
    function GetOperands: IBlockOperands;
    function GetOperator: IBlockOperator;
    property Parent: IBlockNode read GetParent;
    property Operands: IBlockOperands read GetOperands;
    property Operator: IBlockOperator read GetOperator;
  end;

  IBlockOperator = interface (IObject)
    ['{8C8FC6E8-A9A2-4E7F-A888-69842BB903E9}']
    property Name: string read GetInstanceName;
  end;

  IBlockOperands = interface (ICollection)
    ['{2D74E7A7-11B7-4DA4-852E-BBBCEDDDBB13}']
    function GetItem(Index: Integer): IBlockOperand;
    function GetObject(const Name: string): IBlockOperand;
    function Enumerate(var Enum; out Item: IBlockOperand): Boolean;
    function Lookup(const Name: string; const IID: TGUID; out Obj): Boolean; overload;
    property Item[Index: Integer]: IBlockOperand read GetItem;
    property Value[const Name: string]: IBlockOperand read GetObject; default;
  end;

  IOperandList = interface (IBlockOperands)
    ['{8CEFCF1A-D8E5-4D16-8B3F-1D34D6B03FF8}']
    function Add(const Node: IBlockNode): IBlockOperand; overload;
    function Add(const Value: IUnknown): IBlockOperand; overload;
  end;

  TOperandKind = (opNode, opValue);
  IBlockOperand = interface
    ['{35AD11CD-B864-418B-B344-7899F2380029}']
    function GetKind: TOperandKind;
    function GetNode: IBlockNode;
    function GetValue: IUnknown;
    property Kind: TOperandKind read GetKind;
    property Node: IBlockNode read GetNode;
    property Value: IUnknown read GetValue;
  end;

(*)
type
  function Add: IBlockOperator;
  function Sub: IBlockOperator;
  function Mul: IBlockOperator;
  function Dvd: IBlockOperator;
  function Pow: IBlockOperator;

  function Node(const Op: IBlockOperator; const Node1, Node2: IBlockNode): IBlockNode; overload; 
  function Node(const Op: IBlockOperator; const Tag1, Tag2: ITag): IBlockNode; overload;
  function Node(const Op: IBlockOperator; const Node1: IBlockNode; const Tag2: ITag): IBlockNode; overload; 
  function Node(const Op: IBlockOperator; const Tag1: ITag; const Node2: IBlockNode): IBlockNode; overload; 

  function Calc(const Root: IBlockNode): ICalculation;
  
(*)

(*)

  X * 5 + A * m * 1/2

  Node(
    Add(),
    [
      Node(
        Mul(),
        [
          X,
          5
        ]),
      Node(
        Mul(),
        [
          A,
          m,
          1/2
        ])
    ]


postfijo:
    push   A
    push   m
    push   1/2
    mul
    mul
    push   X
    push   5
    mul
    add

tres vias:
    R = mul    A, m
    R = mul    R, 1/2
    S = mul    X, 5
    R = add    S, R

(*)

implementation
end.
