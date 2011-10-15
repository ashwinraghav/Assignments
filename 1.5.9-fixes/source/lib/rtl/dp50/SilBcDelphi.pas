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

unit SilBcDelphi;

{$I Defines.inc}

interface

const                                     
  varEmpty                             = System.varEmpty;
  varNull                              = System.varNull;
  varSmallint                          = System.varSmallint;
  varInteger                           = System.varInteger;
  varSingle                            = System.varSingle;
  varDouble                            = System.varDouble;
  varCurrency                          = System.varCurrency;
  varDate                              = System.varDate;
  varOleStr                            = System.varOleStr;
  varDispatch                          = System.varDispatch;
  varError                             = System.varError;
  varBoolean                           = System.varBoolean;
  varVariant                           = System.varVariant;
  varUnknown                           = System.varUnknown;
  varByte                              = System.varByte;
  varStrArg                            = System.varStrArg;
  varString                            = System.varString;
  varAny                               = System.varAny;
  varTypeMask                          = System.varTypeMask;
  varArray                             = System.varArray;
  varByRef                             = System.varByRef;

const
  varDecimal  = $000E; { vt_decimal     } {UNSUPPORTED}
  varShortInt = $0010; { vt_i1          }
  varWord     = $0012; { vt_ui2         }
  varLongWord = $0013; { vt_ui4         }
  varInt64    = $0014; { vt_i8          }

const
  E_NOTIMPL   = HRESULT($80004001);

implementation
end.
