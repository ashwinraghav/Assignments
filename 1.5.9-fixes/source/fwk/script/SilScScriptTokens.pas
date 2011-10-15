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

unit SilScScriptTokens;

interface

uses
  SilTokens;

const
  TOKEN_STMT_BEGIN            = TToken(TTokenID(7001)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_END              = TToken(TTokenID(7002)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_IF               = TToken(TTokenID(7003)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_ELSE             = TToken(TTokenID(7004)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_FOR              = TToken(TTokenID(7005)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_WHILE            = TToken(TTokenID(7006)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_DO               = TToken(TTokenID(7007)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_REPEAT           = TToken(TTokenID(7008)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_UNTIL            = TToken(TTokenID(7009)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_SWITCH           = TToken(TTokenID(7010)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_CASE             = TToken(TTokenID(7011)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_BREAK            = TToken(TTokenID(7012)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_CONTINUE         = TToken(TTokenID(7013)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_RETURN           = TToken(TTokenID(7014)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );
  TOKEN_STMT_DEFAULT          = TToken(TTokenID(7015)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );

  TOKEN_TYPEID_ANY            = TToken(TTokenID(7050)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_ANY );
  TOKEN_TYPEID_VOID           = TToken(TTokenID(7051)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_VOID );
  TOKEN_TYPEID_BOOLEAN        = TToken(TTokenID(7052)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_BOOLEAN );
  TOKEN_TYPEID_CHAR           = TToken(TTokenID(7053)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_CHAR );
  TOKEN_TYPEID_WIDECHAR       = TToken(TTokenID(7054)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_CHAR );
  TOKEN_TYPEID_INTEGER        = TToken(TTokenID(7055)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_INTEGER );
  TOKEN_TYPEID_FLOAT          = TToken(TTokenID(7056)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_FLOAT );
  TOKEN_TYPEID_SINGLE         = TToken(TTokenID(7057)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_FLOAT );
  TOKEN_TYPEID_DOUBLE         = TToken(TTokenID(7058)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_FLOAT );
  TOKEN_TYPEID_STRING         = TToken(TTokenID(7059)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_STRING );
  TOKEN_TYPEID_WIDESTRING     = TToken(TTokenID(7059)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_STRING );
  TOKEN_TYPEID_DATETIME       = TToken(TTokenID(7060)     or  TOKEN_CLASS_TERMINAL  or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC or TOKEN_VALUE_DATETIME );

  TOKEN_TYPEID                = TToken(                       TOKEN_CLASS_GROUP     or TOKEN_TYPE_RESERVEDWORD  or TOKEN_KIND_PUBLIC );

implementation
end.
