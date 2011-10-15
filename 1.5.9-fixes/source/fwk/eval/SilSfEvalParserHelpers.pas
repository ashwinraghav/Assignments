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

unit SilSfEvalParserHelpers;

interface

{$I Defines.inc}

uses
  Sil,
  SilLiTypeInfo, 
  SilTokens,
  SilLexer,
  SilSiEvaluator;

procedure DoCheck(const Symbol: TToken; const Lexer: ITokenLexer); overload;
procedure DoCheck(const Symbol: array of TToken; const Lexer: ITokenLexer); overload;
function DoGetLiteral(const Symbol: TToken; const Lexer: ITokenLexer): IToken; overload;

function DoCheckLexer(const Lexer: ITokenLexer): ITokenLexer;
function DoAdvance(const Lexer: ITokenLexer): ITokenLexer;
function DoGetSymbol(const Lexer: ITokenLexer): TToken; overload;
function DoGetSymbol(const Lexer: ITokenLexer; out Tok: IToken): TToken; overload;
function DoAddSymbol(const Lexer: ITokenLexer; const Symbol: TToken; const Name: string; const Lexema: string): IToken; overload;

function DoParseBoolean(const Text: string): Boolean;
function DoParseChar(const Text: string): Char;
function DoParseInteger(const Text: string): Integer;
function DoParseFloat(const Text: string): Double;
function DoParseString(const Text: string): string;
function DoParseDateTime(const Text: string): TDateTime;

function TypeInfo(const Value: TTokenValue): ITypeInfo; overload; 
function TypeInfo(const Symbol: TToken): ITypeInfo; overload;
function TypeInfo(const Tok: IToken): ITypeInfo; overload;
function GetType(const Tok: IToken; out DataType: ITypeInfo): Boolean; overload;

implementation

uses
  SilLmDelphiTypeInfo;

function DoParseBoolean(const Text: string): Boolean;
begin
  if Sil.Text.Compare(Text, 'true') = 0 then
    Result := True
  else if Sil.Text.Compare(Text, 'false') = 0 then
    Result := False
  else
    Result := Sil.Str.ToInt(Text, 0) <> 0;
end;

function DoParseChar(const Text: string): Char;
begin
  Result := Text[1];
  if Result = '#' then
    Result := System.Chr(Sil.Str.ToInt(Sil.Str.Copy(Text, 2), 0));
end;

function DoParseInteger(const Text: string): Integer;
begin
  if Sil.Text.IsEqual(Sil.Str.Left(Text, 2), '0x') or (Text[1] = '$') then
    Result := Sil.Str.ToInt(Sil.Str.Replace(Text, '0x', '$'))
  else
    case Sil.Chr.Lower(Text[Length(Text)]) of
      'b':  Result := Sil.Bin.ToInt(Sil.Str.Left(Text, Length(Text)-1));
      'o':  Result := 0; {Result := Sil.Oct.ToInt(Sil.Str.Left(Text, Length(Text)-1))}
      'h':  Result := Sil.Hex.ToInt(Sil.Str.Left(Text, Length(Text)-1));
      'd':  Result := Sil.Str.ToInt(Sil.Str.Left(Text, Length(Text)-1));
      else  Result := Sil.Str.ToInt(Sil.Str.Left(Text, Length(Text)));
    end;
end;

function DoParseFloat(const Text: string): Double;
begin
  Result := Sil.Str.ToFloat(Text);
end;

function DoParseString(const Text: string): string;
begin
  Result := Sil.Str.RemoveDelimiters(Text, '''', '''');
  Result := Sil.Str.RemoveDelimiters(Result, '"', '"');
end;

function DoParseDateTime(const Text: string): TDateTime;
begin
  Result := Sil.DateTime.FromStr(Sil.Str.RemoveDelimiters(Text, '#', '#'));
end;

{ helpers }

function TypeInfo(const Value: TTokenValue): ITypeInfo; overload;
var
  Info: Pointer;
begin
  case Value of
    tvBoolean:    Info := System.TypeInfo(Boolean);
    tvChar:       Info := System.TypeInfo(Char);
    tvInteger:    Info := System.TypeInfo(Integer);
    tvFloat:      Info := System.TypeInfo(Double);
    tvString:     Info := System.TypeInfo(String);
    tvDateTime:   Info := System.TypeInfo(TDateTime);
    else          Info := nil;
  end;
  
  if Assigned(Info) then
    Result := TSilDelphiTypeDef.CreateNew(Info) else
    Result := nil;
end;

function TypeInfo(const Symbol: TToken): ITypeInfo; overload;
begin
  Result := TypeInfo(Token.ValueOf(Symbol));
end;

function TypeInfo(const Tok: IToken): ITypeInfo; overload;
begin
  if Token.ValueOf(Tok.Symbol) = tvCustom then
    Sil.Vart.ToInterface(Tok.Data['type'], ITypeInfo, Result) else
    Result := TypeInfo(Tok.Symbol);
end;

function GetType(const Tok: IToken; out DataType: ITypeInfo): Boolean;
begin
  DataType := TypeInfo(Tok);
  Result := Assigned(DataType);
end;

procedure DoCheck(const Symbol: TToken; const Lexer: ITokenLexer);
begin
  Lexer.Check(Symbol);
end;

function DoGetLiteral(const Symbol: TToken; const Lexer: ITokenLexer): IToken;
begin
  DoGetSymbol(Lexer, Result);  
  Lexer.Check(Symbol);
end;

procedure DoCheck(const Symbol: array of TToken; const Lexer: ITokenLexer);
var
  Tok: TToken;
begin
  Tok := DoGetSymbol(Lexer);
  if Token.InValue(Tok, Symbol) then
    Lexer.Check(Tok) else
    raise Sil.Error.Create('Invalid token found (%s) while expecting any of (%s)', [Token.ToStr(Tok), Token.ToStr(Symbol)]);
end;

function DoCheckLexer(const Lexer: ITokenLexer): ITokenLexer;
begin
  if not Assigned(Lexer) then
    raise Sil.Error.Create('Lexer is invalid');

  if Lexer.Current = nil then
    raise Sil.Error.Create('End of file while parsing input stream');

  Result := Lexer;
end;

function DoAdvance(const Lexer: ITokenLexer): ITokenLexer;
begin
  Lexer.Next;
  Result := Lexer;
end;

function DoGetSymbol(const Lexer: ITokenLexer): TToken;
var
  Dummy: IToken;
begin
  Result := DoGetSymbol(Lexer, Dummy);
end;

function DoGetSymbol(const Lexer: ITokenLexer; out Tok: IToken): TToken;
begin
  if Assigned(Lexer) and Assigned(Lexer.Current) then
  begin
    Tok := Lexer.Current;
    Result := Tok.Symbol;
    if Token.ClassOf(Result) = tcGroup then
      Tok := Tok.Linked;
  end else
  begin
    Tok := nil;
    Result := TOKEN_EOF;
  end;
end;

function DoAddSymbol(const Lexer: ITokenLexer; const Symbol: TToken; const Name: string; const Lexema: string): IToken; 
begin
  if Assigned(Lexer) then
    Result := Lexer.Table.Add(Symbol, Name, Lexema) else
    Result := nil;
end;

end.
 