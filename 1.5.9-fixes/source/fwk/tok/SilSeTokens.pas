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

unit SilSeTokens;

interface

uses

  Sil;

// 332 222 222 222 1111 1111110000000000
// 109 876 543 210 9876 5432109876543210
//     ^^^ ^^^ ^^^ ^^^^ ----------------
//      |   |   |   |         ID (max 65535)
//      |   |   |   |
//      |   |   |   +- Type (Max 15)
//      |   |   |
//      |   |   +- Class: Terminal o NoTerminal 
//      |   +- Kind: Interno o definido por usuario
//      +- Value: Tipo de datos


type
  TToken        = type LongWord;
  TTokenID      = type Word;

const
  idUnknown     = TTokenID($0000);
  idAny         = TTokenID($FFFF);

type
  TTokenType    = (
      ttUnknown,
      ttMarker,
      ttLiteral,
      ttIdentifier,
      ttReservedWord,
      ttOperator,
      ttPunctuator,
      ttReserved7,
      ttReserved8,
      ttReserved9,
      ttReserved10,
      ttReserved11,
      ttReserved12,
      ttReserved13,
      ttReserved14,
      ttAny
    );

type
  TTokenKind    = (
      tkUnknown,
      tkInternal,
      tkSystem,
      tkReserved3,
      tkReserved4,
      tkPublic,
      tkPrivate,
      tkAny
      );
type
  TTokenClass   = (
      tcUnknown,
      tcLexema,
      tcTerminal,
      tcNonTerminal,
      tcGroup,
      tcReserved5,
      tcReserved6,
      tcAny
    );

type
  TTokenValue   = (
      tvUnknown,            // 00
      tvUndefined,          // 01
      tvVoid,               // 02
      tvBoolean,            // 03
      tvChar,               // 04
      tvInteger,            // 05
      tvFloat,              // 06
      tvString,             // 07
      tvDateTime,           // 08
      tvReserved9,          // 09
      tvReserved10,         // 10
      tvReserved11,         // 11
      tvReserved12,         // 12
      tvReserved13,         // 13
      tvCustom,             // 14
      tvAny                 // 15
    );

type
  TTokenMarker  = type Char;

const
  tmUnknown     = TTokenMarker(#00);
  tmSeparator   = TTokenMarker(#09);
  tmEOF         = TTokenMarker(#26); // ^Z
  tmEOL         = TTokenMarker(#10);
  tmAny         = TTokenMarker(#$FF);

type
  TTokenKinds   = set of TTokenKind;
  TTokenClasses = set of TTokenClass;
  TTokenTypes   = set of TTokenType;
  TTokenValues  = set of TTokenValue;

const
  tvNumber      = [tvFloat, tvInteger];

type
  RToken = packed record
    TokenID: TTokenID;
    TokenType: TTokenType;
    TokenValue: TTokenValue;
    TokenClass: TTokenClass;
    TokenKind: TTokenKind;
  end;

implementation
end.
 