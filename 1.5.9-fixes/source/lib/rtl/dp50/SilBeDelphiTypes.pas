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

unit SilBeDelphiTypes;

{$I Defines.inc}

interface

uses
  SysUtils;

type
  Int64                                 = System.Int64;
  IInterface                            = System.IUnknown;

type
{$M+}
  IInvokable = interface(IInterface)
  end;
{$M-}

type
  TMethod                               = SysUtils.TMethod;
  
type
  PByte                                 = ^Byte;
  PWord                                 = ^Word;
  PLongint                              = ^Longint;
  PInteger                              = ^Integer;
  PLongWord                             = ^LongWord;
  PSmallInt                             = ^Smallint;
  PDouble                               = ^Double;
  PShortInt                             = ^ShortInt;
  PExtended                             = System.PExtended;
  PComp                                 = ^Comp;
  PCurrency                             = System.PCurrency;
  PVariant                              = System.PVariant;
  POleVariant                           = System.POleVariant;
  PPointer                              = ^Pointer;
  PBoolean                              = ^Boolean;
  PCardinal                             = ^Cardinal;
  PInt64                                = System.PInt64;
  PSingle                               = ^Single;
  PDate                                 = ^Double;
  PDispatch                             = ^IDispatch;
  PPDispatch                            = ^PDispatch;
  PError                                = ^LongWord;
  PWordBool                             = ^WordBool;
  PUnknown                              = ^IUnknown;

type
  PInterface                            = PUnknown;

type
  PPUnknown                             = ^PUnknown;
  PPWideChar                            = ^PWideChar;
  PPChar                                = ^PChar;
  PPAnsiChar                            = ^PAnsiChar;


implementation
end.
