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

unit SilSgTokens;

interface

uses
  Sil,
  SilSeTokens;

const // Token IDs 0..65535
  TOKEN_ID_WEIGHT   = 1;
  TOKEN_ID_FIRST    = Ord(Low (TTokenID)) * TOKEN_ID_WEIGHT;
  TOKEN_ID_LAST     = Ord(High(TTokenID)) * TOKEN_ID_WEIGHT;
  TOKEN_ID_COUNT    = TOKEN_ID_LAST - TOKEN_ID_FIRST + 1;
  TOKEN_ID_BITS     = (TOKEN_ID_COUNT - 1);
  TOKEN_ID_MASK     = TOKEN_ID_BITS * TOKEN_ID_WEIGHT;
  TOKEN_ID_TOTAL    = (TOKEN_ID_BITS + 1);

const // Token Types: 0..15
  TOKEN_TYPE_WEIGHT = TOKEN_ID_TOTAL * TOKEN_ID_WEIGHT;
  TOKEN_TYPE_FIRST  = Ord(Low(TTokenType));
  TOKEN_TYPE_LAST   = Ord(High(TTokenType));
  TOKEN_TYPE_COUNT  = TOKEN_TYPE_LAST - TOKEN_TYPE_FIRST + 1;
  TOKEN_TYPE_BITS   = (TOKEN_TYPE_COUNT - 1);
  TOKEN_TYPE_MASK   = TOKEN_TYPE_BITS * TOKEN_TYPE_WEIGHT;
  TOKEN_TYPE_TOTAL  = (TOKEN_TYPE_BITS + 1);

const // Token Class: 0..3
  TOKEN_CLASS_WEIGHT = TOKEN_TYPE_TOTAL * TOKEN_TYPE_WEIGHT;
  TOKEN_CLASS_FIRST  = Ord(Low(TTokenClass));
  TOKEN_CLASS_LAST   = Ord(High(TTokenClass));
  TOKEN_CLASS_COUNT  = TOKEN_CLASS_LAST - TOKEN_CLASS_FIRST + 1;
  TOKEN_CLASS_BITS   = (TOKEN_CLASS_COUNT - 1);
  TOKEN_CLASS_MASK   = TOKEN_CLASS_BITS * TOKEN_CLASS_WEIGHT;
  TOKEN_CLASS_TOTAL  = (TOKEN_CLASS_BITS + 1);

const // Token Kind: 0..3
  TOKEN_KIND_WEIGHT = TOKEN_CLASS_TOTAL * TOKEN_CLASS_WEIGHT;
  TOKEN_KIND_FIRST  = Ord(Low(TTokenKind));
  TOKEN_KIND_LAST   = Ord(High(TTokenKind));
  TOKEN_KIND_COUNT  = TOKEN_KIND_LAST - TOKEN_KIND_FIRST + 1;
  TOKEN_KIND_BITS   = (TOKEN_KIND_COUNT - 1);
  TOKEN_KIND_MASK   = TOKEN_KIND_BITS * TOKEN_KIND_WEIGHT;
  TOKEN_KIND_TOTAL  = (TOKEN_KIND_BITS + 1);

const // Token Value: 0..15
  TOKEN_VALUE_WEIGHT= TOKEN_KIND_TOTAL * TOKEN_KIND_WEIGHT;
  TOKEN_VALUE_FIRST = Ord(Low(TTokenValue));
  TOKEN_VALUE_LAST  = Ord(High(TTokenValue));
  TOKEN_VALUE_COUNT = TOKEN_VALUE_LAST - TOKEN_VALUE_FIRST + 1;
  TOKEN_VALUE_BITS  = (TOKEN_VALUE_COUNT - 1);
  TOKEN_VALUE_MASK  = TOKEN_VALUE_BITS * TOKEN_VALUE_WEIGHT;
  TOKEN_VALUE_TOTAL = (TOKEN_VALUE_BITS + 1);

const
  TOKEN_ID_UNKNOWN        = TToken(Ord(idUnknown)       * TOKEN_ID_WEIGHT);
  TOKEN_ID_ANY            = TToken(Ord(idAny)           * TOKEN_ID_WEIGHT);

const
  TOKEN_KIND_UNKNOWN      = TToken(Ord(tkUnknown)       * TOKEN_KIND_WEIGHT);
  TOKEN_KIND_INTERNAL     = TToken(Ord(tkInternal)      * TOKEN_KIND_WEIGHT);
  TOKEN_KIND_SYSTEM       = TToken(Ord(tkSystem)        * TOKEN_KIND_WEIGHT);
  TOKEN_KIND_PUBLIC       = TToken(Ord(tkPublic)        * TOKEN_KIND_WEIGHT);
  TOKEN_KIND_PRIVATE      = TToken(Ord(tkPrivate)       * TOKEN_KIND_WEIGHT);
  TOKEN_KIND_ANY          = TToken(Ord(tkAny)           * TOKEN_KIND_WEIGHT);

const
  TOKEN_CLASS_UNKNOWN     = TToken(Ord(tcUnknown)       * TOKEN_CLASS_WEIGHT);
  TOKEN_CLASS_LEXEMA      = TToken(Ord(tcLexema)        * TOKEN_CLASS_WEIGHT);
  TOKEN_CLASS_TERMINAL    = TToken(Ord(tcTerminal)      * TOKEN_CLASS_WEIGHT);
  TOKEN_CLASS_NONTERMINAL = TToken(Ord(tcNonTerminal)   * TOKEN_CLASS_WEIGHT);
  TOKEN_CLASS_GROUP       = TToken(Ord(tcGroup)         * TOKEN_CLASS_WEIGHT);
  TOKEN_CLASS_ANY         = TToken(Ord(tcAny)           * TOKEN_CLASS_WEIGHT);

const
  TOKEN_TYPE_UNKNOWN      = TToken(Ord(ttUnknown)       * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_MARKER       = TToken(Ord(ttMarker)        * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_LITERAL      = TToken(Ord(ttLiteral)       * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_IDENTIFIER   = TToken(Ord(ttIdentifier)    * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_RESERVEDWORD = TToken(Ord(ttReservedWord)  * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_OPERATOR     = TToken(Ord(ttOperator)      * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_PUNCTUATOR   = TToken(Ord(ttPunctuator)    * TOKEN_TYPE_WEIGHT);
  TOKEN_TYPE_ANY          = TToken(Ord(ttAny)           * TOKEN_TYPE_WEIGHT);

const
  TOKEN_MARKER_UNKNOWN    = TToken(TOKEN_TYPE_MARKER    + TTokenID(Ord(tmUnknown)));
  TOKEN_MARKER_SEPARATOR  = TToken(TOKEN_TYPE_MARKER    + TTokenID(Ord(tmSeparator)));
  TOKEN_MARKER_EOF        = TToken(TOKEN_TYPE_MARKER    + TTokenID(Ord(tmEOF)));
  TOKEN_MARKER_EOL        = TToken(TOKEN_TYPE_MARKER    + TTokenID(Ord(tmEOL)));
  TOKEN_MARKER_ANY        = TToken(TOKEN_TYPE_MARKER    + TTokenID(Ord(tmAny)));

const
  TOKEN_VALUE_UNKNOWN     = TToken(Ord(tvUnknown)       * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_UNDEFINED   = TToken(Ord(tvUndefined)     * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_VOID        = TToken(Ord(tvVoid)          * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_BOOLEAN     = TToken(Ord(tvBoolean)       * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_CHAR        = TToken(Ord(tvChar)          * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_INTEGER     = TToken(Ord(tvInteger)       * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_FLOAT       = TToken(Ord(tvFloat)         * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_STRING      = TToken(Ord(tvString)        * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_DATETIME    = TToken(Ord(tvDateTime)      * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_CUSTOM      = TToken(Ord(tvCustom)        * TOKEN_VALUE_WEIGHT );
  TOKEN_VALUE_ANY         = TToken(Ord(tvAny)           * TOKEN_VALUE_WEIGHT );
                                                        
const
  TOKEN_SYSTEM_LEXEMA     = TToken(TOKEN_KIND_SYSTEM    or TOKEN_CLASS_LEXEMA);
  TOKEN_SYSTEM_TERMINAL   = TToken(TOKEN_KIND_SYSTEM    or TOKEN_CLASS_TERMINAL);
  TOKEN_SYSTEM_NONTERMINAL= TToken(TOKEN_KIND_SYSTEM    or TOKEN_CLASS_NONTERMINAL);
  TOKEN_SYSTEM_GROUP      = TToken(TOKEN_KIND_SYSTEM    or TOKEN_CLASS_GROUP      );
  TOKEN_PUBLIC_LEXEMA     = TToken(TOKEN_KIND_PUBLIC    or TOKEN_CLASS_LEXEMA);
  TOKEN_PUBLIC_TERMINAL   = TToken(TOKEN_KIND_PUBLIC    or TOKEN_CLASS_TERMINAL);
  TOKEN_PUBLIC_NONTERMINAL= TToken(TOKEN_KIND_PUBLIC    or TOKEN_CLASS_NONTERMINAL);
  TOKEN_PUBLIC_GROUP      = TToken(TOKEN_KIND_PUBLIC    or TOKEN_CLASS_GROUP      );
  TOKEN_LEXEMA            = TToken(TOKEN_KIND_ANY       or TOKEN_CLASS_LEXEMA);
  TOKEN_TERMINAL          = TToken(TOKEN_KIND_ANY       or TOKEN_CLASS_TERMINAL);
  TOKEN_NONTERMINAL       = TToken(TOKEN_KIND_ANY       or TOKEN_CLASS_NONTERMINAL);
  TOKEN_GROUP             = TToken(TOKEN_KIND_ANY       or TOKEN_CLASS_GROUP      );

const
  TOKEN_NULL              = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_VOID);
  TOKEN_BOOLEAN           = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_BOOLEAN);
  TOKEN_CHAR              = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_CHAR);
  TOKEN_INTEGER           = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_INTEGER);
  TOKEN_FLOAT             = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_FLOAT);
  TOKEN_STRING            = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_STRING);
  TOKEN_DATETIME          = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_LITERAL       or TOKEN_VALUE_DATETIME);

const
  TOKEN_SYMBOL            = TToken(TOKEN_TERMINAL       or TOKEN_TYPE_IDENTIFIER    or TOKEN_VALUE_STRING);
  TOKEN_UNKNOWN           = TToken(TOKEN_CLASS_ANY      or TOKEN_KIND_ANY           or TOKEN_MARKER_UNKNOWN);
  TOKEN_EOF               = TToken(TOKEN_CLASS_ANY      or TOKEN_KIND_SYSTEM        or TOKEN_MARKER_EOF);
  TOKEN_ANY               = TToken(TOKEN_ID_ANY         or TOKEN_CLASS_ANY          or TOKEN_KIND_ANY         or TOKEN_TYPE_ANY  or TOKEN_VALUE_ANY);

const
  TOKEN_TYPE: array[TTokenType] of TToken =
    (
      TOKEN_TYPE_UNKNOWN,          // ttUnknown,
      TOKEN_TYPE_MARKER,           // ttMarker,
      TOKEN_TYPE_LITERAL,          // ttLiteral,
      TOKEN_TYPE_IDENTIFIER,       // ttIdentifier,
      TOKEN_TYPE_RESERVEDWORD,     // ttReservedWord,
      TOKEN_TYPE_OPERATOR,         // ttOperator,
      TOKEN_TYPE_PUNCTUATOR,       // ttPunctuator,
      TOKEN_TYPE_UNKNOWN,          // ttReserved7,
      TOKEN_TYPE_UNKNOWN,          // ttReserved8,
      TOKEN_TYPE_UNKNOWN,          // ttReserved9,
      TOKEN_TYPE_UNKNOWN,          // ttReserved10,
      TOKEN_TYPE_UNKNOWN,          // ttReserved11,
      TOKEN_TYPE_UNKNOWN,          // ttReserved12,
      TOKEN_TYPE_UNKNOWN,          // ttReserved13,
      TOKEN_TYPE_UNKNOWN,          // ttReserved14,
      TOKEN_TYPE_ANY               // ttAny
    );

const
  TOKEN_KIND: array[TTokenKind] of TToken =
    (
      TOKEN_KIND_UNKNOWN,          // tkUnknown,
      TOKEN_KIND_INTERNAL,         // tkInternal,
      TOKEN_KIND_SYSTEM,           // tkSystem,
      TOKEN_KIND_UNKNOWN,          // tkReserved3,
      TOKEN_KIND_UNKNOWN,          // tkReserved4,
      TOKEN_KIND_PUBLIC,           // tkPublic,
      TOKEN_KIND_PRIVATE,          // tkPrivate,
      TOKEN_KIND_ANY               // tkAny
    );

const
  TOKEN_CLASS: array[TTokenClass] of TToken =
    (
      TOKEN_CLASS_UNKNOWN,          // tcUnknown,
      TOKEN_CLASS_LEXEMA,           // tcLexema,
      TOKEN_CLASS_TERMINAL,         // tcTerminal,
      TOKEN_CLASS_NONTERMINAL,      // tcNonTerminal,
      TOKEN_CLASS_GROUP,            // tcGroup,
      TOKEN_CLASS_UNKNOWN,          // tcReserved5,
      TOKEN_CLASS_UNKNOWN,          // tcReserved6,
      TOKEN_CLASS_ANY               // tcAny
    );

  TOKEN_VALUE: array[TTokenValue] of TToken =
    (
      TOKEN_VALUE_UNKNOWN,          // tvUnknown,            // 00
      TOKEN_VALUE_UNDEFINED,        // tvUndefined,          // 01
      TOKEN_VALUE_VOID,             // tvVoid,               // 02
      TOKEN_VALUE_BOOLEAN,          // tvBoolean,            // 03
      TOKEN_VALUE_CHAR,             // tvChar,               // 04
      TOKEN_VALUE_INTEGER,          // tvInteger,            // 05
      TOKEN_VALUE_FLOAT,            // tvFloat,              // 06
      TOKEN_VALUE_STRING,           // tvString,             // 07
      TOKEN_VALUE_DATETIME,         // tvDateTime,           // 08
      TOKEN_VALUE_UNKNOWN,          // tvReserved9,          // 09
      TOKEN_VALUE_UNKNOWN,          // tvReserved10,         // 10
      TOKEN_VALUE_UNKNOWN,          // tvReserved11,         // 11
      TOKEN_VALUE_UNKNOWN,          // tvReserved12,         // 12
      TOKEN_VALUE_UNKNOWN,          // tvReserved13,         // 13
      TOKEN_VALUE_UNKNOWN,          // tvReserved14,         // 14
      TOKEN_VALUE_ANY               // tvAny                 // 15
    );

implementation
end.
 