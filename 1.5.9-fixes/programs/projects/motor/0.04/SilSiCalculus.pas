unit SilSiCalculus;

interface

uses
  Sil, SilSiHolors;

type
  ICommands = interface
    ['{20F3037B-9FCC-4560-ADA9-17A7667D4660}']

  end;

  IOperator = interface
    ['{8C8FC6E8-A9A2-4E7F-A888-69842BB903E9}']

  end;

  IOperands = interface
    ['{2D74E7A7-11B7-4DA4-852E-BBBCEDDDBB13}']
  end;

  INode = interface
    ['{9F13B155-038B-4540-8791-0C5C3CDA50ED}']
    property Operands: IOperands;
    property Operator: IOperator;
    function Compile(var Cmd: ICommands);
  end;

  ICalculation = interface
    ['{DC6B24B5-E1AB-417B-96D2-F73753B3D38D}']
    property Commands: ICommands;
    function Execute(const Arguments: ITags): ITag;
  end;

  function Add: IOperator;
  function Sub: IOperator;
  function Mul: IOperator;
  function Dvd: IOperator;
  function Pow: IOperator;

  function Node(const Op: IOperator; const Node1, Node2: INode): INode; overload; 
  function Node(const Op: IOperator; const Tag1, Tag2: ITag): INode; overload;
  function Node(const Op: IOperator; const Node1: INode; const Tag2: ITag): INode; overload; 
  function Node(const Op: IOperator; const Tag1: ITag; const Node2: INode): INode; overload; 

  function Calc(const Root: INode): ICalculation;


///////////////////////////////////////////////////////////////////////////////////////


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

    push   A
    push   m
    push   1/2
    mul
    mul
    push   X
    push   5
    mul
    add


    R = mul    A, m
    R = mul    R, 1/2
    S = mul    X, 5
    R = add    S, R



implementation
end.
 