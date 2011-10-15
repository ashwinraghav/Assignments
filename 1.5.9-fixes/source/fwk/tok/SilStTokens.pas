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

unit SilStTokens;

interface

uses
  SilBkTool,
  SilLiTypeInfo,
  SilSeTokens;

type
  Token = class(Tool)
    class function Make(const AType: TTokenType; const ID: TTokenID; const Value: TTokenValue = tvString; const AClass: TTokenClass = tcTerminal; const Kind: TTokenKind = tkPublic): TToken; overload;
    class function Make(const Data: RToken): TToken; overload;

    class function Data(const Token: TToken): RToken;
    class function ID(const Token: TToken): TTokenID;
    class function ClassOf(const Token: TToken): TTokenClass;
    class function TypeOf(const Token: TToken): TTokenType;
    class function KindOf(const Token: TToken): TTokenKind;
    class function ValueOf(const Token: TToken): TTokenValue; overload;
    class function ValueOf(const TypeInfo: ITypeInfo): TTokenValue; overload;

    class function ToStr(const Value: TToken): string; overload;
    class function ToStr(const Value: array of TToken): string; overload;
    class function ToStr(const Value: TTokenID): string; overload;
    class function ToStr(const Value: TTokenType): string; overload;
    class function ToStr(const Value: TTokenClass): string; overload;
    class function ToStr(const Value: TTokenKind): string; overload;
    class function ToStr(const Value: TTokenValue): string; overload;

    class function InValue(const Token: TToken; const Tokens: array of TToken): Boolean; overload;

    class function Has(const Token: TToken; const Value: TTokenID): Boolean; overload;
    class function Has(const Token: TToken; const Value: array of TTokenID): Boolean; overload;

    class function Has(const Token: TToken; const Value: TTokenType): Boolean; overload;
    class function Has(const Token: TToken; const Value: TTokenTypes): Boolean; overload;

    class function Has(const Token: TToken; const Value: TTokenClass): Boolean; overload;
    class function Has(const Token: TToken; const Value: TTokenClasses): Boolean; overload;

    class function Has(const Token: TToken; const Value: TTokenKind): Boolean; overload;
    class function Has(const Token: TToken; const Value: TTokenKinds): Boolean; overload;

    class function Has(const Token: TToken; const Value: TTokenValue): Boolean; overload;
    class function Has(const Token: TToken; const Value: TTokenValues): Boolean; overload;

    class function Compare(const Token1, Token2: TToken): Integer;
    class function IsEqual(const Token1, Token2: TToken): Boolean;
  end;

implementation

uses
  SilSfTokens;

{ Token }

class function Token.Make(
  const AType: TTokenType;
  const ID: TTokenID;
  const Value: TTokenValue;
  const AClass: TTokenClass;
  const Kind: TTokenKind): TToken;
begin
  Result := SilSfTokens.Token(AType, ID, Value, AClass, Kind);
end;

class function Token.Make(const Data: RToken): TToken;
begin
  Result := SilSfTokens.Token(Data);
end;

class function Token.Data(const Token: TToken): RToken;
begin
  Result := SilSfTokens.TokenData(Token);
end;

class function Token.ClassOf(const Token: TToken): TTokenClass;
begin
  Result := SilSfTokens.TokenClass(Token);
end;

class function Token.KindOf(const Token: TToken): TTokenKind;
begin
  Result := SilSfTokens.TokenKind(Token);
end;

class function Token.TypeOf(const Token: TToken): TTokenType;
begin
  Result := SilSfTokens.TokenType(Token);
end;

class function Token.ValueOf(const Token: TToken): TTokenValue;
begin
  Result := SilSfTokens.TokenValue(Token);
end;

class function Token.ValueOf(const TypeInfo: ITypeInfo): TTokenValue;
begin
  Result := SilSfTokens.TypeKindToTokenValue(TypeInfo.TypeKind);
end;

class function Token.InValue(const Token: TToken; const Tokens: array of TToken): Boolean;
begin
  Result := SilSfTokens.TokenIn(Token, Tokens);
end;

class function Token.Has(const Token: TToken; const Value: TTokenValue): Boolean;
begin
  Result := SilSfTokens.TokenIs(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenValues): Boolean;
begin
  Result := SilSfTokens.TokenIn(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenKinds): Boolean;
begin
  Result := SilSfTokens.TokenIn(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenClass): Boolean;
begin
  Result := SilSfTokens.TokenIs(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenClasses): Boolean;
begin
  Result := SilSfTokens.TokenIn(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenKind): Boolean;
begin
  Result := SilSfTokens.TokenIs(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenType): Boolean;
begin
  Result := SilSfTokens.TokenIs(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenTypes): Boolean;
begin
  Result := SilSfTokens.TokenIn(Token, Value);
end;

class function Token.ID(const Token: TToken): TTokenID;
begin
  Result := SilSfTokens.TokenID(Token);
end;

class function Token.Has(const Token: TToken; const Value: array of TTokenID): Boolean;
begin
  Result := SilSfTokens.TokenIn(Token, Value);
end;

class function Token.Has(const Token: TToken; const Value: TTokenID): Boolean;
begin
  Result := SilSfTokens.TokenIs(Token, Value);
end;

class function Token.ToStr(const Value: array of TToken): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.ToStr(const Value: TTokenKind): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.ToStr(const Value: TTokenValue): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.ToStr(const Value: TTokenClass): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.ToStr(const Value: TToken): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.ToStr(const Value: TTokenID): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.ToStr(const Value: TTokenType): string;
begin
  Result := SilSfTokens.TokenToStr(Value);
end;

class function Token.Compare(const Token1, Token2: TToken): Integer;
begin
  Result := SilSfTokens.TokenCompare(Token1, Token2);
end;

class function Token.IsEqual(const Token1, Token2: TToken): Boolean;
begin
  Result := Compare(Token1, Token2) = 0;
end;

end.
