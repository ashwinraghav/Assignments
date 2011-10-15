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

unit SilSfHttpParser;

interface

uses
  Sil,
  SilTokens,
  SilLexer,
  SilUrl,
  SilSiHttpData,
  SilSiHttpTypes;

function HttpParseInteger(const Lexer: ITokenLexer): Integer;
function HttpParseToken(const Lexer: ITokenLexer): string; overload;
function HttpParseToken(const Lexer: ITokenLexer; Delimiter: TToken): string; overload;
function HttpParseToken(const Lexer: ITokenLexer; const Delimiters: array of TToken): string; overload;
function HttpParseMethod(const Lexer: ITokenLexer): THttpMethod;
function HttpMethodName(const Method: THttpMethod): string;
function HttpParseUrl(const Lexer: ITokenLexer): IUrl;

procedure HttpSkip(const Lexer: ITokenLexer; const SymbolSet: array of TToken); overload; 
procedure HttpSkip(const Lexer: ITokenLexer; Symbol: TToken); overload; 
procedure HttpCheck(const Lexer: ITokenLexer; Symbol: TToken; Advance: Boolean = True); overload;
procedure HttpCheck(const Lexer: ITokenLexer; Symbol: TToken; const Lexema: string; Advance: Boolean = True); overload;
function HttpGetSymbol(const Lexer: ITokenLexer): TToken; overload;
function HttpGetSymbol(const Lexer: ITokenLexer; out Tok: IToken): TToken; overload;
function HttpIsSymbol(const Tok: IToken; const SymbolSet: array of TToken): Boolean;

function HttpTagID(const Name: string): THttpTag;
function HttpTagName(const ID: THttpTag): string;

procedure HttpWrite(const Writer: IWriter; const Instance: IUnknown); 

implementation

uses
  SilScHttpTokens,
  SilSgHttpTagName;

function HttpParseMethod(const Lexer: ITokenLexer): THttpMethod;
begin
  HttpCheck(Lexer, TOKEN_TOKEN, False);
  Result := THttpMethod(Sil.Enum.Value(TypeInfo(THttpMethod), Lexer.Current.Lexema, 'http'));
  Lexer.Next();
end;

function HttpMethodName(const Method: THttpMethod): string;
begin
  Result := Sil.Str.ToUpper(Sil.Enum.Name(TypeInfo(THttpMethod), Ord(Method), 'http'));
end;

function HttpParseUrl(const Lexer: ITokenLexer): IUrl;
var
  Text: string;
begin
  Text := HttpParseToken(Lexer, [TOKEN_LWS, TOKEN_EOF]);
  
  if Sil.Str.NotEmpty(Text) then
    Result := SilUrl.Url.Create(Text) else
    Result := nil;
end;

function HttpParseInteger(const Lexer: ITokenLexer): Integer;
begin
  HttpCheck(Lexer, TOKEN_DIGITS, False);
  Result := Str.ToInt(Lexer.Current.Lexema);
  Lexer.Next();
end;

function HttpParseToken(const Lexer: ITokenLexer): string;
begin
  HttpCheck(Lexer, TOKEN_TOKEN, False);
  Result := Lexer.Current.Lexema;
  Lexer.Next();
end;

function HttpParseToken(const Lexer: ITokenLexer; Delimiter: TToken): string; overload;
begin
  Result := HttpParseToken(Lexer, [Delimiter]);
end;

function HttpParseToken(const Lexer: ITokenLexer; const Delimiters: array of TToken): string; overload;
begin
  Result := '';
  while not HttpIsSymbol(Lexer.Current, Delimiters) do
  begin
    Sil.Str.Add(Result, Lexer.Current.Lexema);
    Lexer.Next();
  end;
end;

procedure HttpSkip(const Lexer: ITokenLexer; const SymbolSet: array of TToken);
begin
  while Assigned(Lexer) and Assigned(Lexer.Current) and HttpIsSymbol(Lexer.Current, SymbolSet) do
    Lexer.Next();
end;

procedure HttpSkip(const Lexer: ITokenLexer; Symbol: TToken);
begin
  HttpSkip(Lexer, [Symbol]);
end;

procedure HttpCheck(const Lexer: ITokenLexer; Symbol: TToken; Advance: Boolean);
begin
  if not Assigned(Lexer) or not Assigned(Lexer.Current) then
    raise Sil.Error.Create('End Of Input Stream');

  if not HttpIsSymbol(Lexer.Current, [Symbol]) then
    raise Sil.Error.Create('Invalid token');
  if Advance then Lexer.Next();
end;

procedure HttpCheck(const Lexer: ITokenLexer; Symbol: TToken; const Lexema: string; Advance: Boolean = True);
begin
  HttpCheck(Lexer, Symbol, False);
  if Assigned(Lexer) and Assigned(Lexer.Current) then
  begin
    if Sil.Text.Compare(Lexer.Current.Lexema, Lexema) <> 0 then
      raise Sil.Error.Create('invalid token: expected %s found %s', [Lexema, Lexer.Current.Lexema]);
    if Advance then Lexer.Next();
  end;
end;

function HttpGetSymbol(const Lexer: ITokenLexer): TToken;
var
  Dummy: IToken;
begin
  Result := HttpGetSymbol(Lexer, Dummy);
end;

function HttpGetSymbol(const Lexer: ITokenLexer; out Tok: IToken): TToken; 
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

function HttpIsSymbol(const Tok: IToken; const SymbolSet: array of TToken): Boolean;
begin
  Result := Token.InValue(Tok.Symbol, SymbolSet);
  if not Result and (Token.ClassOf(Tok.Symbol) = tcGroup) then
    Result := Token.InValue(Tok.Linked.Symbol, SymbolSet);
end;

function HttpTagID(const Name: string): THttpTag;
begin
  try
    Result := THttpTag(Sil.Enum.Value(TypeInfo(THttpTag), Str.Replace(Name, '-', ''), 'htt'));
  except
    Result := httExtension;
  end;
end;

function HttpTagName(const ID: THttpTag): string;
begin
  Result := GHttpTagName[ID];
end;

procedure HttpWrite(const Writer: IWriter; const Instance: IUnknown);
var
  Item: IHttpObject;
begin
  if Ref.GetInterface(Instance, IHttpObject, Item) then
    Item.WriteTo(Writer);  
end;

end.
