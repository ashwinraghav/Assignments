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

unit SilLeTypeReader;

{$I Defines.inc}

interface

const
  tiInteger     = 0;
  tiBoolean     = 1;
  tiByte        = 2;
  tiDouble      = 3;
  tiDate        = 4;
  tiChar        = 5;
//  tiWideChar    = 9;
  tiAnsiString  = 11;
  tiWideString  = 15;
  tiLargeInt    = 16;
  tiWord        = 20;
  tiLongWord    = 21;
//tiLargeWord   = 22;
  tiShortInt    = 23;
  tiSmallInt    = 24;
  tiSingle      = 25;
  tiVariant     = 26;
  tiCurrency    = 27;
  tiGuid        = 28;

  tiPChar       = 30;

implementation

end.
 