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

unit SilSmTokenLexer;

interface

uses
  Sil,
  SilTokens,
  SilSiTokenLexer,
  SilSiLexDictionary;

type
  TSilTokenLexer = class(
    TSilInterfacedObject,
    ITokenLexer )
  private
    FStream: IStream;
    FReader: ITextStream;
    FTable: ITokenTable;
    FDictionary: ILexDictionary;
    FCurrent: IToken;
    FLine: string;
    FIndex: Integer;
    procedure DoNext;
    procedure DoClearTable;
  protected // ITokenLexer
    function GetTable: ITokenTable;
    function GetStream: IStream;
    function GetCurrent: IToken;
    function Next(Previous: Boolean = True): IToken;
    procedure Check(Symbol: TToken; Advance: Boolean = True);
    procedure Reset;
  public
    constructor Create(const Stream: IStream; const Table: ITokenTable; const Dictionary: ILexDictionary);
    destructor Destroy; override;
  end;

implementation                            
          
uses
  SilSgTokens, SilStTokens, SilSmToken, SilSmTokenTable, SilStLexDictionary,
  SilLtStream;

{ TSilTokenLexer }

constructor TSilTokenLexer.Create(const Stream: IStream; const Table: ITokenTable; const Dictionary: ILexDictionary);
begin
  inherited Create;
  FCurrent := nil;
  FIndex := -1;
  if Assigned(Stream) then
    FStream := Stream else
    FStream := Sil.MemoryStream.Create();
  if Assigned(Table) then
    FTable := Table else
    FTable := TSilTokenTable.Create;
  FDictionary := Dictionary;
  FReader := Sil.Stream.Text(FStream);
  Reset;
end;

destructor TSilTokenLexer.Destroy;
begin
  FTable := nil;
  FDictionary := nil;
  FReader := nil;
  FStream := nil;
  inherited;
end;

function TSilTokenLexer.GetCurrent: IToken;
begin
  Result := FCurrent;
end;

function TSilTokenLexer.GetStream: IStream;
begin
  Result := FStream;
end;

function TSilTokenLexer.GetTable: ITokenTable;
begin
  Result := FTable;
end;

function TSilTokenLexer.Next(Previous: Boolean = True): IToken;
begin
  if Previous     and Assigned(FCurrent) then Result := FCurrent;
  DoNext;
  if not Previous and Assigned(FCurrent) then Result := FCurrent; 
end;

procedure TSilTokenLexer.Check(Symbol: TToken; Advance: Boolean);
begin
  if not Token.IsEqual(FCurrent.Symbol, Symbol) then
    raise Sil.Error.Create('Syntax error: %s was espected but %s found', [Token.ToStr(Symbol), Token.ToStr(FCurrent.Symbol)]);
  if Advance then DoNext;    
end;

procedure TSilTokenLexer.Reset;
var
  Rand: IRandomStream;
begin
  if Sil.Ref.GetInterface(FStream, IRandomStream, Rand) then
    Rand.Position := 0;

  FIndex := -1;

  DoClearTable;
  DoNext;
end;


{/
  reglas:
    - se eliminan de consideración todos los que sean de tipo Private.
    - si entre los matches hay un grupo, matchea este.
    - si no hay un grupo, se elige el terminal que aparezca primero en la lista.
    - si matcheo un grupo, para determinar el linked:
      - si no hay otros matches, se genera un error (error "las reglas estan mal construidas"); 
      - si hay mas de un terminal, el linked es el que aparece antes y mas cerca del grupo.
      - si hay solo un terminal, el linked es este.
      - si no hay otro terminal, se elige un lexema.
 /}


procedure TSilTokenLexer.DoNext;
var
  Text: string;
  Enum: IEnumerator;
  Matches: ILexMatches;
  Item, Match, Group, Linked, Terminal, Any: ILexMatch;
  FoundMatches: Boolean;
  Len: Integer;
  Symbol: RToken;
begin
  FCurrent := nil;
  repeat

    if (FIndex <= 0) then
      if not FReader.ReadLn(FLine) then
      begin
        FCurrent := nil;
        Break;
      end else
      begin
        Str.Add(FLine, sLineBreak);      
        FIndex := 1;
      end;

    if Sil.Str.IsEmpty(FLine) or (FIndex > Length(FLine)) then
    begin
      FIndex := -1;
      Continue;
    end;

    Text := Sil.Str.Copy(FLine, FIndex);

    FoundMatches := (FIndex > 0) and FDictionary.Definitions.GetMatches(Text, Matches, True);

    if FoundMatches then
    begin
      Match := nil;

      Len := 0;
      Any := nil;
      while Matches.Enumerate(Enum, Item) do
      begin
        if Len < Item.Len then
          Len := Item.Len;
        if Token.KindOf(Item.Definition.Symbol) = tkPrivate then
          Matches.Delete(Enum.Iteration);
      end;

      while Matches.Enumerate(Enum, Item) do
      begin
        Symbol := Token.Data(Item.Definition.Symbol);

        Matches.Delete(Enum.Iteration);

        if Symbol.TokenClass = tcGroup then
          begin
            Group := Item;
            if not Assigned(Linked) and Assigned(Terminal) then
              Linked := Terminal;
          end
        else if Symbol.TokenClass = tcTerminal then
          begin
            if not Assigned(Terminal) then
              Terminal := Item;
          end
        else
          begin
            if not Assigned(Any) then
              Any := Item;
          end;
      end;

      if Assigned(Group) then
        begin
          Match := Group;
          if not Assigned(Linked) and Assigned(Terminal) then
            Linked := Terminal;
          if not Assigned(Linked) and Assigned(Any) then
            Linked := Any;
        end
      else if Assigned(Terminal) then
        Match := Terminal
      else
        Match := Any;
      
      if Assigned(Match) then
      begin
        Len := Match.Len;
        Symbol := Token.Data(Match.Definition.Symbol);
        if Symbol.TokenClass <> tcLexema then
        begin
          if not FTable.Find(Match.Lexema, FCurrent) then
            FCurrent := FTable.Add(Match.Definition.Symbol, Match.Definition.Name, Match.Lexema);

          if (Symbol.TokenClass = tcGroup) and Assigned(Linked) then
            FCurrent.Linked := FTable.Add(Linked.Definition.Symbol, Linked.Definition.Name, Linked.Lexema) else
            FCurrent.Linked := nil;
        end;
      end;
      
      Inc(FIndex, Len);

    end else
      FIndex := -1;

  until FCurrent <> nil;
end;

procedure TSilTokenLexer.DoClearTable;
var
  Enum: IEnumerator;
  Tok: IToken;
begin
  if Assigned(FTable) then
    with FTable do
      while Enumerate(Enum, Tok) do
        if Token.KindOf(Tok.Symbol) = tkSystem then
          Delete(Enum.Iteration)        
end;

end.
