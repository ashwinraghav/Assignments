unit SilSiTokenAnalyzerLalr;

interface

uses
  SilSeTokens;

type
  TLalrState = type Word;
  TSymbolCount = Smallint;

  TActionKind = (akAccept, akReduce, akMove, akError);

  TActionAccept = packed record
  end;

  TActionReduce = packed record
    NewSymbol: TToken;
    SymbolCount: TSymbolCount;
  end;

  TActionMove = packed record
    NewState: TLalrState;
  end;

  TActionError = packed record
  end;

  TLalrAction = packed record
    Kind: TActionKind;
    case TActionKind of
      akAccept: (Accept: TActionAccept; );
      akReduce: (Reduce: TActionReduce; );
      akMove  : (Move: TActionMove; );
      akError : (Error: TActionError; );
  end;

const
  ssInitial = TLalrState(0);

type
  ILalrTable = interface;
  ILalrStack = interface;

  TTokenArray = array of TToken;

  ILalrAnalyzer = interface(ISyntaxAnalyzer)
    ['{DA1917D3-87CF-489F-A284-8404B9532CBE}']
  end;

  ILalrTable = interface
    ['{7828E5B8-C19B-4EC2-A361-6F32E3B58D6A}']
    function Action(Token: TToken; AState: TLalrState): TLalrAction;
    function Next(Token: TToken; AState: TLalrState): TLalrState;
  end;

  ILalrStack = interface
    ['{D0CA72B7-56F4-4C6A-9D28-E9B3D0814C5C}']
    function GetState: TLalrState;
    procedure Push(Token: TToken; State: TLalrState);
    procedure Pop(Count: TSymbolCount; out Tokens: TTokenArray);
    property State: TLalrState read GetState;
  end;

implementation
end.
