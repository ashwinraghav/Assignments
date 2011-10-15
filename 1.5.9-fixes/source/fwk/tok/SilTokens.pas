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

unit SilTokens;

interface

uses
  SilSeTokens,
  SilSgTokens,
  SilSiToken,
  SilStTokens;

type
  TToken                          = SilSeTokens.TToken;
  TTokenID                        = SilSeTokens.TTokenID;

const
  idUnknown                       = TTokenID(SilSeTokens.idUnknown);
  idAny                           = TTokenID(SilSeTokens.idAny);

type
  TTokenType                      = SilSeTokens.TTokenType;

const
  ttUnknown                       = TTokenType(SilSeTokens.ttUnknown      );
  ttMarker                        = TTokenType(SilSeTokens.ttMarker       );
  ttLiteral                       = TTokenType(SilSeTokens.ttLiteral     );
  ttIdentifier                    = TTokenType(SilSeTokens.ttIdentifier   );
  ttReservedWord                  = TTokenType(SilSeTokens.ttReservedWord );
  ttOperator                      = TTokenType(SilSeTokens.ttOperator     );
  ttPunctuator                    = TTokenType(SilSeTokens.ttPunctuator   );
  ttAny                           = TTokenType(SilSeTokens.ttAny          );

type
  TTokenKind                      = SilSeTokens.TTokenKind;

const
  tkUnknown                       = TTokenKind(SilSeTokens.tkUnknown );
  tkInternal                      = TTokenKind(SilSeTokens.tkInternal);
  tkSystem                        = TTokenKind(SilSeTokens.tkSystem  );
  tkPublic                        = TTokenKind(SilSeTokens.tkPublic  );
  tkPrivate                       = TTokenKind(SilSeTokens.tkPrivate );
  tkAny                           = TTokenKind(SilSeTokens.tkAny     );
  
type
  TTokenClass                     = SilSeTokens.TTokenClass;

const
  tcUnknown                       = TTokenClass(SilSeTokens.tcUnknown     );
  tcLexema                        = TTokenClass(SilSeTokens.tcLexema      );
  tcTerminal                      = TTokenClass(SilSeTokens.tcTerminal    );
  tcNonTerminal                   = TTokenClass(SilSeTokens.tcNonTerminal );
  tcGroup                         = TTokenClass(SilSeTokens.tcGroup       );
  tcAny                           = TTokenClass(SilSeTokens.tcAny         );

type
  TTokenValue                     = SilSeTokens.TTokenValue;

const
  tvUnknown                       = TTokenValue(SilSeTokens.tvUnknown   );
  tvUndefined                     = TTokenValue(SilSeTokens.tvUndefined );
  tvVoid                          = TTokenValue(SilSeTokens.tvVoid      );
  tvBoolean                       = TTokenValue(SilSeTokens.tvBoolean   );
  tvChar                          = TTokenValue(SilSeTokens.tvChar      );
  tvInteger                       = TTokenValue(SilSeTokens.tvInteger   );
  tvFloat                         = TTokenValue(SilSeTokens.tvFloat     );
  tvString                        = TTokenValue(SilSeTokens.tvString    );
  tvDateTime                      = TTokenValue(SilSeTokens.tvDateTime  );
  tvCustom                        = TTokenValue(SilSeTokens.tvCustom    );
  tvAny                           = TTokenValue(SilSeTokens.tvAny       );

type
  TTokenMarker                    = SilSeTokens.TTokenMarker;

const
  tmUnknown                       = TTokenMarker(SilSeTokens.tmUnknown   );
  tmSeparator                     = TTokenMarker(SilSeTokens.tmSeparator );
  tmEOF                           = TTokenMarker(SilSeTokens.tmEOF       );
  tmEOL                           = TTokenMarker(SilSeTokens.tmEOL       );
  tmAny                           = TTokenMarker(SilSeTokens.tmAny       );

type
  TTokenKinds                     = SilSeTokens.TTokenKinds  ;
  TTokenClasses                   = SilSeTokens.TTokenClasses;
  TTokenTypes                     = SilSeTokens.TTokenTypes  ;
  TTokenValues                    = SilSeTokens.TTokenValues ;

const
  tvNumber                        = SilSeTokens.tvNumber;

const
  TOKEN_ID_UNKNOWN                = SilSgTokens.TOKEN_ID_UNKNOWN;
  TOKEN_ID_ANY                    = SilSgTokens.TOKEN_ID_ANY;

const
  TOKEN_KIND_UNKNOWN              = SilSgTokens.TOKEN_KIND_UNKNOWN;
  TOKEN_KIND_INTERNAL             = SilSgTokens.TOKEN_KIND_INTERNAL;
  TOKEN_KIND_SYSTEM               = SilSgTokens.TOKEN_KIND_SYSTEM;
  TOKEN_KIND_PUBLIC               = SilSgTokens.TOKEN_KIND_PUBLIC;
  TOKEN_KIND_PRIVATE              = SilSgTokens.TOKEN_KIND_PRIVATE;
  TOKEN_KIND_ANY                  = SilSgTokens.TOKEN_KIND_ANY;

const
  TOKEN_CLASS_UNKNOWN             = SilSgTokens.TOKEN_CLASS_UNKNOWN;
  TOKEN_CLASS_LEXEMA              = SilSgTokens.TOKEN_CLASS_LEXEMA;
  TOKEN_CLASS_TERMINAL            = SilSgTokens.TOKEN_CLASS_TERMINAL;
  TOKEN_CLASS_NONTERMINAL         = SilSgTokens.TOKEN_CLASS_NONTERMINAL;
  TOKEN_CLASS_GROUP               = SilSgTokens.TOKEN_CLASS_GROUP;
  TOKEN_CLASS_ANY                 = SilSgTokens.TOKEN_CLASS_ANY;

const                                           
  TOKEN_TYPE_UNKNOWN              = SilSgTokens.TOKEN_TYPE_UNKNOWN;
  TOKEN_TYPE_MARKER               = SilSgTokens.TOKEN_TYPE_MARKER;
  TOKEN_TYPE_LITERAL              = SilSgTokens.TOKEN_TYPE_LITERAL;
  TOKEN_TYPE_IDENTIFIER           = SilSgTokens.TOKEN_TYPE_IDENTIFIER;
  TOKEN_TYPE_RESERVEDWORD         = SilSgTokens.TOKEN_TYPE_RESERVEDWORD;
  TOKEN_TYPE_OPERATOR             = SilSgTokens.TOKEN_TYPE_OPERATOR;
  TOKEN_TYPE_PUNCTUATOR           = SilSgTokens.TOKEN_TYPE_PUNCTUATOR;
  TOKEN_TYPE_ANY                  = SilSgTokens.TOKEN_TYPE_ANY;

const
  TOKEN_MARKER_UNKNOWN            = SilSgTokens.TOKEN_MARKER_UNKNOWN;
  TOKEN_MARKER_EOF                = SilSgTokens.TOKEN_MARKER_EOF;
  TOKEN_MARKER_EOL                = SilSgTokens.TOKEN_MARKER_EOL;

const                                           
  TOKEN_VALUE_UNKNOWN             = SilSgTokens.TOKEN_VALUE_UNKNOWN;
  TOKEN_VALUE_UNDEFINED           = SilSgTokens.TOKEN_VALUE_UNDEFINED;
  TOKEN_VALUE_VOID                = SilSgTokens.TOKEN_VALUE_VOID;
  TOKEN_VALUE_BOOLEAN             = SilSgTokens.TOKEN_VALUE_BOOLEAN;
  TOKEN_VALUE_CHAR                = SilSgTokens.TOKEN_VALUE_CHAR;
  TOKEN_VALUE_INTEGER             = SilSgTokens.TOKEN_VALUE_INTEGER;
  TOKEN_VALUE_FLOAT               = SilSgTokens.TOKEN_VALUE_FLOAT;
  TOKEN_VALUE_STRING              = SilSgTokens.TOKEN_VALUE_STRING;
  TOKEN_VALUE_DATETIME            = SilSgTokens.TOKEN_VALUE_DATETIME;
  TOKEN_VALUE_CUSTOM              = SilSgTokens.TOKEN_VALUE_CUSTOM;
  TOKEN_VALUE_ANY                 = SilSgTokens.TOKEN_VALUE_ANY;

const
  TOKEN_SYSTEM_LEXEMA             = SilSgTokens.TOKEN_SYSTEM_LEXEMA;
  TOKEN_SYSTEM_TERMINAL           = SilSgTokens.TOKEN_SYSTEM_TERMINAL;
  TOKEN_SYSTEM_NONTERMINAL        = SilSgTokens.TOKEN_SYSTEM_NONTERMINAL;
  TOKEN_PUBLIC_LEXEMA             = SilSgTokens.TOKEN_PUBLIC_LEXEMA;
  TOKEN_PUBLIC_TERMINAL           = SilSgTokens.TOKEN_PUBLIC_TERMINAL;
  TOKEN_PUBLIC_NONTERMINAL        = SilSgTokens.TOKEN_PUBLIC_NONTERMINAL;
  TOKEN_LEXEMA                    = SilSgTokens.TOKEN_LEXEMA;
  TOKEN_TERMINAL                  = SilSgTokens.TOKEN_TERMINAL;
  TOKEN_NONTERMINAL               = SilSgTokens.TOKEN_NONTERMINAL;       

const
  TOKEN_NULL                      = SilSgTokens.TOKEN_NULL;
  TOKEN_BOOLEAN                   = SilSgTokens.TOKEN_BOOLEAN;
  TOKEN_CHAR                      = SilSgTokens.TOKEN_CHAR;
  TOKEN_INTEGER                   = SilSgTokens.TOKEN_INTEGER;
  TOKEN_FLOAT                     = SilSgTokens.TOKEN_FLOAT;
  TOKEN_STRING                    = SilSgTokens.TOKEN_STRING;
  TOKEN_DATETIME                  = SilSgTokens.TOKEN_DATETIME;

const
  TOKEN_SYMBOL                    = SilSgTokens.TOKEN_SYMBOL;
  TOKEN_UNKNOWN                   = SilSgTokens.TOKEN_UNKNOWN;
  TOKEN_EOF                       = SilSgTokens.TOKEN_EOF;
  TOKEN_ANY                       = SilSgTokens.TOKEN_ANY;

type
  RToken                          = SilSeTokens.RToken;

type
  IToken                          = SilSiToken.IToken;
  ITokens                         = SilSiToken.ITokens;
  ITokenTable                     = SilSiToken.ITokenTable;

type
  Token                           = SilStTokens.Token;

implementation
end.
