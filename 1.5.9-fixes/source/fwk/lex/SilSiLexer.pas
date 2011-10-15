unit SilSiLexer;

interface

uses
  Sil,
  SilSeTokens, SilSiToken;

type
  ILexer = interface
    ['{BE71A069-081D-46C0-B256-C02B15768674}']
    function GetTable: ITokenTable;
    function GetCurrent: TToken;
    procedure Next;
    property Table: ITokenTable read GetTable;
    property Current: TToken read GetCurrent;
  end;

implementation
end.
