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

type
  Int64                                 = System.Int64;
  LongBool                              = System.LongBool;
  TMethod                               = System.TMethod;

type
  TClass                                = System.TClass;
  TObject                               = System.TObject;
  IInterface                            = System.IInterface;
  IInvokable                            = System.IInvokable;

type
  PInt64                                = System.PInt64;
  PByte                                 = System.PByte;
  PWord                                 = System.PWord;
  PLongint                              = System.PLongint;
  PInteger                              = System.PInteger;
  PLongWord                             = System.PLongWord;
  PSmallInt                             = System.PSmallint;
  PDouble                               = System.PDouble;
  PShortInt                             = System.PShortInt;
  PLargeInt                             = PInt64;
  PExtended                             = System.PExtended;
  PComp                                 = System.PComp;
  PCurrency                             = System.PCurrency;
  PVariant                              = System.PVariant;
  POleVariant                           = System.POleVariant;
  PPointer                              = System.PPointer;
  PBoolean                              = System.PBoolean;
  PCardinal                             = System.PCardinal;
  PSingle                               = System.PSingle;
  PDate                                 = System.PDate;
  PDispatch                             = System.PDispatch;
  PPDispatch                            = System.PPDispatch;
  PError                                = System.PError;
  PWordBool                             = System.PWordBool;
  PUnknown                              = System.PUnknown;

type
  PInterface                            = PUnknown;
  PClass                                = ^TClass;
  PObject                               = ^TObject;
                                                           
type
  PPUnknown                             = System.PPUnknown;
  PPWideChar                            = System.PPWideChar;
  PPChar                                = System.PPChar;
  PPAnsiChar                            = System.PPAnsiChar;

implementation
end.
