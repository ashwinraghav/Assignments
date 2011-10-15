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

unit SilSfTokens;

interface

uses
  Sil,
  SilBeTypeInfo,
  SilSeTokens;

function Token(const AType: TTokenType; const ID: TTokenID; const Value: TTokenValue = tvVoid; const AClass: TTokenClass = tcTerminal; const Kind: TTokenKind = tkSystem): TToken; overload;
function Token(const Data: RToken): TToken; overload;


function TokenData(const Token: TToken): RToken;
function TokenID(const Token: TToken): TTokenID;
function TokenClass(const Token: TToken): TTokenClass;
function TokenType(const Token: TToken): TTokenType;
function TokenKind(const Token: TToken): TTokenKind;
function TokenValue(const Token: TToken): TTokenValue;

function TokenToStr(const Value: array of TToken): string; overload;
function TokenToStr(const Value: TToken): string; overload;
function TokenToStr(const Value: TTokenID): string; overload;
function TokenToStr(const Value: TTokenType): string; overload;
function TokenToStr(const Value: TTokenClass): string; overload;
function TokenToStr(const Value: TTokenKind): string; overload;
function TokenToStr(const Value: TTokenValue): string; overload;

function TokenIs(const Token: TToken; const Value: TTokenID): Boolean; overload;
function TokenIn(const Token: TToken; const Value: array of TTokenID): Boolean; overload;

function TokenIs(const Token: TToken; const Value: TTokenType): Boolean; overload;
function TokenIn(const Token: TToken; const Value: TTokenTypes): Boolean; overload;

function TokenIs(const Token: TToken; const Value: TTokenClass): Boolean; overload;
function TokenIn(const Token: TToken; const Value: TTokenClasses): Boolean; overload;

function TokenIs(const Token: TToken; const Value: TTokenKind): Boolean; overload;
function TokenIn(const Token: TToken; const Value: TTokenKinds): Boolean; overload;

function TokenIs(const Token: TToken; const Value: TTokenValue): Boolean; overload;
function TokenIn(const Token: TToken; const Value: TTokenValues): Boolean; overload;

function TokenIn(const Token: TToken; const Tokens: array of TToken): Boolean; overload;

function TokenCompare(const Token1, Token2: TToken): Integer;

function TypeKindToTokenValue(Kind: TTypeKind): TTokenValue;

implementation

uses
  SilSgTokens;

function Token(const AType: TTokenType; const ID: TTokenID; const Value: TTokenValue; const AClass: TTokenClass; const Kind: TTokenKind): TToken;
begin
  Result := TOKEN_VALUE[Value] + TOKEN_CLASS[AClass] + TOKEN_KIND[Kind] + TOKEN_TYPE[AType] + TToken(ID);
end;

function Token(const Data: RToken): TToken;
begin
  Result := TOKEN_VALUE[Data.TokenValue] + TOKEN_CLASS[Data.TokenClass] + TOKEN_KIND[Data.TokenKind] + TOKEN_TYPE[Data.TokenType] + TToken(Data.TokenID);
end;

function TokenData(const Token: TToken): RToken;
begin
  Result.TokenID    := TokenID(Token);
  Result.TokenType  := TokenType(Token);
  Result.TokenKind  := TokenKind(Token);
  Result.TokenClass := TokenClass(Token);
  Result.TokenValue := TokenValue(Token);
end;

function TokenID(const Token: TToken): TTokenID;
begin
  Result := TTokenID ((Token and TOKEN_ID_MASK) div TOKEN_ID_WEIGHT);
end;

function TokenClass(const Token: TToken): TTokenClass;
begin
  Result := TTokenClass ((Token and TOKEN_CLASS_MASK) div TOKEN_CLASS_WEIGHT);
end;

function TokenType(const Token: TToken): TTokenType;
begin
  Result := TTokenType ((Token and TOKEN_TYPE_MASK) div TOKEN_TYPE_WEIGHT);
end;

function TokenKind(const Token: TToken): TTokenKind;
begin
  Result := TTokenKind ((Token and TOKEN_KIND_MASK) div TOKEN_KIND_WEIGHT);
end;

function TokenValue(const Token: TToken): TTokenValue;
begin
  Result := TTokenValue ((Token and TOKEN_VALUE_MASK) div TOKEN_VALUE_WEIGHT);
end;

function TokenToStr(const Value: array of TToken): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Value) to High(Value) do
    Sil.Str.Add(Result, TokenToStr(Value[I]), ', ');
end;

function TokenToStr(const Value: TToken): string;
begin
  Result := 'K=' + Str.PadR(TokenToStr(TokenKind(Value)),   8)  + ', ' +
            'C=' + Str.PadR(TokenToStr(TokenClass(Value)), 11) + ', ' +
            'T=' + Str.PadR(TokenToStr(TokenType(Value)),  12)  + ', ' +
            'V=' + Str.PadR(TokenToStr(TokenValue(Value)),  9) + ', ' +
            'I=' + Str.PadL(TokenToStr(TokenID(Value)),     7);
end;

function TokenToStr(const Value: TTokenID): string;
begin
  case Value of
    idUnknown:  Result := 'Unknown';
    idAny:      Result := 'Any';
    else        Result := Sil.Int.ToStr(Value);
  end;
end;

function TokenToStr(const Value: TTokenType): string; 
begin
  Result := Sil.Enum.Name(TypeInfo(TTokenType), Ord(Value), 'tt');
end;

function TokenToStr(const Value: TTokenClass): string;
begin
  Result := Sil.Enum.Name(TypeInfo(TTokenClass), Ord(Value), 'tc');
end;

function TokenToStr(const Value: TTokenKind): string;
begin
  Result := Sil.Enum.Name(TypeInfo(TTokenKind), Ord(Value), 'tk');
end;

function TokenToStr(const Value: TTokenValue): string;
begin
  Result := Sil.Enum.Name(TypeInfo(TTokenValue), Ord(Value), 'tv');
end;

function TokenIs(const Token: TToken; const Value: TTokenID): Boolean;
begin
  Result := (Value = idAny) or (TokenID(Token) = idAny) or (TokenID(Token) = Value);
end;

function TokenIn(const Token: TToken; const Value: array of TTokenID): Boolean;
var
  I: Integer;
begin
  for I := Low(Value) to High(Value) do
  begin
    Result := TokenIs(Token, Value[I]);
    if Result then Exit;
  end;
  Result := False;
end;
  
function TokenIs(const Token: TToken; const Value: TTokenType): Boolean;
begin
  Result := (Value = ttAny) or (TokenType(Token) in [Value, ttAny]);
end;

function TokenIn(const Token: TToken; const Value: TTokenTypes): Boolean;
begin
  Result := TokenType(Token) in Value + [ttAny];
end;

function TokenIs(const Token: TToken; const Value: TTokenClass): Boolean;
begin
  Result := (Value = tcAny) or (TokenClass(Token) in [Value, tcAny]);
end;

function TokenIn(const Token: TToken; const Value: TTokenClasses): Boolean; 
begin
  Result := TokenClass(Token) in Value + [tcAny];
end;

function TokenIs(const Token: TToken; const Value: TTokenKind): Boolean;
begin
  Result := (Value = tkAny) or (TokenKind(Token) in [Value, tkAny]);
end;

function TokenIn(const Token: TToken; const Value: TTokenKinds): Boolean;
begin
  Result := TokenKind(Token) in Value + [tkAny];
end;

function TokenIs(const Token: TToken; const Value: TTokenValue): Boolean;
begin
  Result := (Value = tvAny) or (TokenValue(Token) in [Value, tvAny]);
end;

function TokenIn(const Token: TToken; const Value: TTokenValues): Boolean;
begin
  Result := TokenValue(Token) in Value + [tvAny];
end;

function TokenIn(const Token: TToken; const Tokens: array of TToken): Boolean;
var
  I: Integer;
begin
  for I := Low(Tokens) to High(Tokens) do
    if TokenCompare(Token, Tokens[I]) = 0 then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

function TokenCompare(const Token1, Token2: TToken): Integer;
begin
  Result := Ord(
    TokenIs(Token1, TokenType(Token2)) and
    TokenIs(Token1, TokenKind(Token2)) and 
    TokenIs(Token1, TokenClass(Token2)) and
    TokenIs(Token1, TokenValue(Token2)) and
    TokenIs(Token1, TokenID(Token2)) ) - 1;
end;


function TypeKindToTokenValue(Kind: TTypeKind): TTokenValue;
begin
  case Kind of
    tkInteger:        Result := tvInteger;
    tkChar:           Result := tvChar;       
    tkEnumeration:    Result := tvInteger; 
    tkFloat:          Result := tvFloat; 
    tkString:         Result := tvString; 
    tkSet:            Result := tvInteger; 
    tkClass:          Result := tvInteger;
    tkMethod:         Result := tvInteger;
    tkWChar:          Result := tvChar;
    tkLString:        Result := tvString;
    tkWString:        Result := tvString;
    tkVariant:        Result := tvCustom;
    tkArray:          Result := tvCustom;
    tkRecord:         Result := tvCustom;
    tkInterface:      Result := tvCustom;
    tkInt64:          Result := tvCustom;
    tkDynArray:       Result := tvCustom; 
    else              Result := tvUnknown; 
  end;
end;


end.
