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

unit SilScEvalTokens;

interface

uses
  SilTokens;

const
  ID_OP_FIRST           = TTokenID(6001);
  
  ID_OP_LOR             = TTokenID(ID_OP_FIRST   +  0);
  ID_OP_LAND            = TTokenID(ID_OP_FIRST   +  1);
  ID_OP_OR              = TTokenID(ID_OP_FIRST   +  2);
  ID_OP_XOR             = TTokenID(ID_OP_FIRST   +  3);
  ID_OP_AND             = TTokenID(ID_OP_FIRST   +  4);
  ID_OP_EQUAL           = TTokenID(ID_OP_FIRST   +  5);
  ID_OP_NOTEQUAL        = TTokenID(ID_OP_FIRST   +  6);
  ID_OP_LESS            = TTokenID(ID_OP_FIRST   +  7);
  ID_OP_GREATER         = TTokenID(ID_OP_FIRST   +  8);
  ID_OP_LESSEQ          = TTokenID(ID_OP_FIRST   +  9);
  ID_OP_GREATEREQ       = TTokenID(ID_OP_FIRST   + 10);
  ID_OP_SHIFT_RIGHT     = TTokenID(ID_OP_FIRST   + 11);
  ID_OP_SHIFT_LEFT      = TTokenID(ID_OP_FIRST   + 12);
  ID_OP_PLUS            = TTokenID(ID_OP_FIRST   + 13);
  ID_OP_MINUS           = TTokenID(ID_OP_FIRST   + 14);
  ID_OP_MUL             = TTokenID(ID_OP_FIRST   + 15);
  ID_OP_DIV             = TTokenID(ID_OP_FIRST   + 16);
  ID_OP_MOD             = TTokenID(ID_OP_FIRST   + 17);
  ID_OP_NEG             = TTokenID(ID_OP_FIRST   + 18);
  ID_OP_NOT             = TTokenID(ID_OP_FIRST   + 19);
  ID_OP_INC             = TTokenID(ID_OP_FIRST   + 20);
  ID_OP_DEC             = TTokenID(ID_OP_FIRST   + 21);
  ID_OP_ASSIGN          = TTokenID(ID_OP_FIRST   + 22);
  ID_OP_ASSIGN_MUL      = TTokenID(ID_OP_FIRST   + 23);
  ID_OP_ASSIGN_DIV      = TTokenID(ID_OP_FIRST   + 24);
  ID_OP_ASSIGN_MOD      = TTokenID(ID_OP_FIRST   + 25);
  ID_OP_ASSIGN_PLUS     = TTokenID(ID_OP_FIRST   + 26);
  ID_OP_ASSIGN_MINUS    = TTokenID(ID_OP_FIRST   + 27);
  ID_OP_ASSIGN_SHR      = TTokenID(ID_OP_FIRST   + 28);
  ID_OP_ASSIGN_SHL      = TTokenID(ID_OP_FIRST   + 29);
  ID_OP_ASSIGN_AND      = TTokenID(ID_OP_FIRST   + 30);
  ID_OP_ASSIGN_XOR      = TTokenID(ID_OP_FIRST   + 31);
  ID_OP_ASSIGN_OR       = TTokenID(ID_OP_FIRST   + 32);
  ID_OP_CALL            = TTokenID(ID_OP_FIRST   + 33);
  ID_OP_INDEX           = TTokenID(ID_OP_FIRST   + 34);
  ID_OP_ARROW           = TTokenID(ID_OP_FIRST   + 35);
  ID_OP_DOT             = TTokenID(ID_OP_FIRST   + 36);

  ID_OP_LAST            = ID_OP_DOT;

const
  TOKEN_BLANKS                = TToken(                       TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PUBLIC);

  TOKEN_ALPHA                 = TToken(TTokenID(1001)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_DIGIT                 = TToken(TTokenID(1002)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_BIT                   = TToken(TTokenID(1003)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_OCT                   = TToken(TTokenID(1004)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
                                                          
  TOKEN_SQ                    = TToken(TTokenID(2001)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_DQ                    = TToken(TTokenID(2002)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
                                                            
  TOKEN_HEXDIGIT              = TToken(TTokenID(3001)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
                                                            
  TOKEN_ALPHANUM              = TToken(TTokenID(4001)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_DIGITS                = TToken(TTokenID(4003)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
                                                            
  TOKEN_SIGN                  = TToken(TTokenID(5001)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_MANTISSA              = TToken(TTokenID(5006)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_FIXED                 = TToken(TTokenID(5007)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);
  TOKEN_EXPONENT              = TToken(TTokenID(5008)     or  TOKEN_CLASS_LEXEMA    or TOKEN_KIND_PRIVATE);

  TOKEN_BOOLEAN               = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_BOOLEAN );
  TOKEN_HEXA                  = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_INTEGER );
  TOKEN_BINARY                = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_INTEGER );
  TOKEN_OCTAL                 = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_INTEGER );
  TOKEN_DECIMAL               = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_INTEGER );
  TOKEN_STRING                = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_STRING  );
  TOKEN_FLOAT                 = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC   or  TOKEN_VALUE_FLOAT   );
                                                                                                                
  TOKEN_LITERAL               = TToken(                       TOKEN_CLASS_GROUP     or TOKEN_TYPE_LITERAL      or TOKEN_KIND_PUBLIC );
  TOKEN_IDENTIFIER            = TToken(                       TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_IDENTIFIER    or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_OP_LOR                = TToken(ID_OP_LOR          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_LAND               = TToken(ID_OP_LAND         or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_OR                 = TToken(ID_OP_OR           or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_XOR                = TToken(ID_OP_XOR          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_AND                = TToken(ID_OP_AND          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_OP_NEG                = TToken(ID_OP_NEG          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_NOT                = TToken(ID_OP_NOT          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_INC                = TToken(ID_OP_INC          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_DEC                = TToken(ID_OP_DEC          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
                                                                                                                
  TOKEN_OP_EQ                 = TToken(ID_OP_EQUAL        or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_NEQ                = TToken(ID_OP_NOTEQUAL     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_OP_LESS               = TToken(ID_OP_LESS         or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_GREATER            = TToken(ID_OP_GREATER      or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_LESSEQ             = TToken(ID_OP_LESSEQ       or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_GREATEREQ          = TToken(ID_OP_GREATEREQ    or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
                                                                                                                
  TOKEN_OP_SHR                = TToken(ID_OP_SHIFT_RIGHT  or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_OP_SHL                = TToken(ID_OP_SHIFT_LEFT   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_PLUS                  = TToken(ID_OP_PLUS         or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_MINUS                 = TToken(ID_OP_MINUS        or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_MUL                   = TToken(ID_OP_MUL          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_DIV                   = TToken(ID_OP_DIV          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_MOD                   = TToken(ID_OP_MOD          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_ASSIGN                = TToken(ID_OP_ASSIGN       or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_MUL            = TToken(ID_OP_ASSIGN_MUL   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_DIV            = TToken(ID_OP_ASSIGN_DIV   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_MOD            = TToken(ID_OP_ASSIGN_MOD   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_PLUS           = TToken(ID_OP_ASSIGN_PLUS  or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_MINUS          = TToken(ID_OP_ASSIGN_MINUS or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_SHR            = TToken(ID_OP_ASSIGN_SHR   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_SHL            = TToken(ID_OP_ASSIGN_SHL   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_AND            = TToken(ID_OP_ASSIGN_AND   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_XOR            = TToken(ID_OP_ASSIGN_XOR   or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ASSIGN_OR             = TToken(ID_OP_ASSIGN_OR    or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
                                                                                                                
  TOKEN_CALL_OP               = TToken(ID_OP_CALL         or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_INDEX_OP              = TToken(ID_OP_INDEX        or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_ARROW_OP              = TToken(ID_OP_ARROW        or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_DOT_OP                = TToken(ID_OP_DOT          or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );

  TOKEN_ASSIGN_OP             = TToken(TTokenID(7001)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_SUM_OP                = TToken(TTokenID(7002)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_MULT_OP               = TToken(TTokenID(7003)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_EQUALITY_OP           = TToken(TTokenID(7004)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_RELATIONAL_OP         = TToken(TTokenID(7005)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_SHIFT_OP              = TToken(TTokenID(7006)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_INCR_OP               = TToken(TTokenID(7007)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );
  TOKEN_UNARY_OP              = TToken(TTokenID(7008)     or  TOKEN_CLASS_GROUP     or TOKEN_TYPE_OPERATOR      or TOKEN_KIND_PUBLIC );

  TOKEN_DOLLAR                = TToken(TTokenID(8000)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_LPAREN                = TToken(TTokenID(8001)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_RPAREN                = TToken(TTokenID(8002)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_SEMICOLON             = TToken(TTokenID(8003)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_COLON                 = TToken(TTokenID(8004)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_COMMA                 = TToken(TTokenID(8005)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_PERIOD                = TToken(TTokenID(8006)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_LCURLY                = TToken(TTokenID(8007)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_RCURLY                = TToken(TTokenID(8008)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_LBRACKET              = TToken(TTokenID(8009)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_RBRACKET              = TToken(TTokenID(8010)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_AT                    = TToken(TTokenID(8011)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );
  TOKEN_QUESTION              = TToken(TTokenID(8012)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_PUNCTUATOR    or TOKEN_KIND_PUBLIC );

  TOKEN_PARAM_FRAME           = TToken(TTokenID(9001)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_MARKER        or TOKEN_KIND_PUBLIC );
  TOKEN_PARAM_PUSH            = TToken(TTokenID(9002)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_MARKER        or TOKEN_KIND_PUBLIC );
                                                                                                                
implementation
end.
