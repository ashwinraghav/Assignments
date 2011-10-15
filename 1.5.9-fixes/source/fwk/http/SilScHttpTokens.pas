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

unit SilScHttpTokens;

interface

uses
  SilTokens;

const
  ID_OCTET                    =  2;
  ID_CHAR                     =  3;
  ID_UPALPHA                  =  4;
  ID_LOALPHA                  =  5;
  ID_ALPHA                    =  6;
  ID_DIGIT                    =  7;
  ID_CTL                      =  8;
  ID_CR                       =  9;
  ID_LF                       = 10;
  ID_SP                       = 11;
  ID_HT                       = 12;
  ID_DQ                       = 13;
  ID_CRLF                     = 14;
  ID_LWS                      = 15;
  ID_HEX                      = 16;
  ID_TOKEN                    = 17;
  ID_SEPARATORS               = 18;
  ID_TEXT                     = 19;
  ID_COMMENT                  = 20;
  ID_QUOTED_PAIR              = 21;
  ID_CTEXT                    = 22;
  ID_QUOTED_STRING            = 23;
  ID_QDTEXT                   = 24;
  ID_DIGITS                   = 25;
  ID_1DIGIT                   = 26;
  ID_2DIGIT                   = 27;
  ID_3DIGIT                   = 28;
  ID_4DIGIT                   = 29;
  ID_SEP_LPAREN               = 30;
  ID_SEP_RPAREN               = 31;
  ID_SEP_MINOR                = 32;
  ID_SEP_MAJOR                = 33;
  ID_SEP_AT                   = 34;
  ID_SEP_COMMA                = 35;
  ID_SEP_SEMICOLON            = 36;
  ID_SEP_COLON                = 37;
  ID_SEP_BACKSLASH            = 38;
  ID_SEP_SLASH                = 39;
  ID_SEP_LBRACKET             = 40;
  ID_SEP_RBRACKET             = 41;
  ID_SEP_QUESTION             = 42;
  ID_SEP_EQUAL                = 43;
  ID_SEP_LCURLY               = 44;
  ID_SEP_RCURLY               = 45;
  ID_SEP_PERIOD               = 46;
  ID_SEP_STAR                 = 46;

const
  TOKEN_OCTET                 = TToken(ID_OCTET                           or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_CHAR                  = TToken(ID_CHAR                            or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_UPALPHA               = TToken(ID_UPALPHA                         or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_LOALPHA               = TToken(ID_LOALPHA                         or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_ALPHA                 = TToken(ID_ALPHA                           or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_DIGIT                 = TToken(ID_DIGIT                           or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_CTL                   = TToken(ID_CTL                             or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_CR                    = TToken(ID_CR                              or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_LF                    = TToken(ID_LF                              or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_SP                    = TToken(ID_SP                              or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PRIVATE);
  TOKEN_HT                    = TToken(ID_HT                              or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_DQ                    = TToken(ID_DQ                              or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_CRLF                  = TToken(ID_CRLF                            or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_LWS                   = TToken(ID_LWS                             or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_HEX                   = TToken(ID_HEX                             or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_TOKEN                 = TToken(ID_TOKEN                           or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_SEPARATORS            = TToken(ID_SEPARATORS                      or TOKEN_CLASS_GROUP      or TOKEN_KIND_PUBLIC);
  TOKEN_TEXT                  = TToken(ID_TEXT                            or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_COMMENT               = TToken(ID_COMMENT                         or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_QUOTED_PAIR           = TToken(ID_QUOTED_PAIR                     or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_CTEXT                 = TToken(ID_CTEXT                           or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_QUOTED_STRING         = TToken(ID_QUOTED_STRING                   or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_QDTEXT                = TToken(ID_QDTEXT                          or TOKEN_CLASS_LEXEMA     or TOKEN_KIND_PRIVATE);
  TOKEN_DIGITS                = TToken(ID_DIGITS                          or TOKEN_CLASS_GROUP      or TOKEN_KIND_PUBLIC);
  TOKEN_1DIGIT                = TToken(ID_1DIGIT                          or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_2DIGIT                = TToken(ID_2DIGIT                          or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_3DIGIT                = TToken(ID_3DIGIT                          or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_4DIGIT                = TToken(ID_4DIGIT                          or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC);
  TOKEN_SEP_LPAREN            = TToken(ID_SEP_LPAREN                      or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_RPAREN            = TToken(ID_SEP_RPAREN                      or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_MINOR             = TToken(ID_SEP_MINOR                       or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_MAJOR             = TToken(ID_SEP_MAJOR                       or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_AT                = TToken(ID_SEP_AT                          or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_COMMA             = TToken(ID_SEP_COMMA                       or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_SEMICOLON         = TToken(ID_SEP_SEMICOLON                   or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_COLON             = TToken(ID_SEP_COLON                       or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_BACKSLASH         = TToken(ID_SEP_BACKSLASH                   or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_SLASH             = TToken(ID_SEP_SLASH                       or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_LBRACKET          = TToken(ID_SEP_LBRACKET                    or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_RBRACKET          = TToken(ID_SEP_RBRACKET                    or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_QUESTION          = TToken(ID_SEP_QUESTION                    or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_EQUAL             = TToken(ID_SEP_EQUAL                       or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_LCURLY            = TToken(ID_SEP_LCURLY                      or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_RCURLY            = TToken(ID_SEP_RCURLY                      or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_PERIOD            = TToken(ID_SEP_PERIOD                      or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);
  TOKEN_SEP_STAR              = TToken(ID_SEP_STAR                        or TOKEN_CLASS_TERMINAL   or TOKEN_KIND_PUBLIC    or TOKEN_TYPE_PUNCTUATOR);

implementation
end.
