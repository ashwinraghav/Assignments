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

unit SilSmTokenTable;

interface

uses
  Sil,

  SilLmInterfaceList, 
  SilSeTokens,
  SilSiToken;

type
  TSilTokenTable = class(
    TSilInterfaceList,
    ITokens,
    ITokenTable )
  protected // ITokens
    function GetItem(Index: Integer): IToken;
    function Find(Symbol: TToken; out Token: IToken): Boolean; overload; 
    function Find(const Lexema: string; out Token: IToken): Boolean; overload; 
    function Lookup(Symbol: TToken): IToken; overload;
    function Lookup(const Lexema: string): IToken; overload;
    function Enumerate(var Enum: IEnumerator; out Token: IToken): Boolean; reintroduce;
  protected // ITokenTable
  //procedure Clear; {inherited}
    function Add(Symbol: TToken; const Name: string; const Lexema: string): IToken; reintroduce;
    function Remove(Symbol: TToken): Integer; reintroduce; overload;  
    function Remove(const Item: IToken): Integer; reintroduce; overload;  
  //procedure Delete(Index: Integer); {inherited}   
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SilSgTokens, SilStTokens, SilSmToken;

{ TSilTokenTable }

constructor TSilTokenTable.Create;
begin
  inherited Create;
end;

destructor TSilTokenTable.Destroy;
begin
  inherited;
end;

function TSilTokenTable.Enumerate(var Enum: IEnumerator; out Token: IToken): Boolean;
begin
  Result := inherited Enumerate(Enum, Token);
end;

function TSilTokenTable.GetItem(Index: Integer): IToken;
begin
  Result := IToken(inherited GetItem(Index));
end;

function TSilTokenTable.Find(Symbol: TToken; out Token: IToken): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Token) do
    Result := Token.Symbol = Symbol;
end;

function TSilTokenTable.Find(const Lexema: string; out Token: IToken): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Token) do
    Result := Sil.Text.Compare(Token.Lexema, Lexema) = 0;
end;

function TSilTokenTable.Lookup(Symbol: TToken): IToken;
begin
  if not Find(Symbol, Result) then
    raise Sil.Error.Create('Invalid token ID: (0x%x) %s', [Symbol, Token.ToStr(Symbol)]);
end;

function TSilTokenTable.Lookup(const Lexema: string): IToken;
begin
  if not Find(Lexema, Result) then
    Result := nil;
end;

function TSilTokenTable.Add(Symbol: TToken; const Name: string; const Lexema: string): IToken;
var
  Instance: TSilToken;
begin
  if Token.ID(Symbol) = 0 then
    Symbol := TToken(TOKEN_KIND_SYSTEM or (Symbol and not TOKEN_KIND_MASK) or TTokenID(Self.Count + 1));
    
  Instance := TSilToken.Create(Symbol, Name, Lexema, Sil.Vart.Unassigned);
  Instance.Index := inherited Add(IToken(Instance));

  Result := Instance;
end;

function TSilTokenTable.Remove(Symbol: TToken): Integer;
var
  Instance: IToken;
begin
  if Find(Symbol, Instance) then
  begin
    Result := Instance.Index;
    SetItem(Result, nil);
  end else
    Result := -1;
end;

function TSilTokenTable.Remove(const Item: IToken): Integer;
var
  Index: Integer;
begin
  Index := IndexOf(Item);
  if ValidIndex(Index) then
    SetItem(Index, nil);
  Result := Index;
end;

end.

