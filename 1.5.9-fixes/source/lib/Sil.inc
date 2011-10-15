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

{$IFDEF VER80}
  {$DEFINE DP10}
{$ENDIF}

{$IFDEF VER90}
  {$DEFINE DP20}
{$ENDIF}

{$IFDEF VER93}
  {$DEFINE CB10}
{$ENDIF}

{$IFDEF VER100}
  {$DEFINE DP30}
{$ENDIF}

{$IFDEF VER120}
  {$DEFINE DP40}
{$ENDIF}

{$IFDEF VER130}
  {$DEFINE DP50}
{$ENDIF}

{$IFDEF VER140}
  {$DEFINE DP60}
{$ENDIF}

{$IFDEF VER141}
  {$DEFINE KX20}
{$ENDIF}

{$IFDEF VER150}
  {$DEFINE DP70}
{$ENDIF}

{$IFDEF VER170}
  {$DEFINE DP90}
{$ENDIF}

{$IFDEF VER180}
  {$DEFINE DP90}
{$ENDIF}

{$IFDEF DP20}
  {$DEFINE D10}
{$ENDIF}

{$IFDEF DP20}
  {$DEFINE D10}
  {$DEFINE D20}
{$ENDIF}

{$IFDEF DP30}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
{$ENDIF}

{$IFDEF DP40}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
{$ENDIF}

{$IFDEF DP50}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
  {$DEFINE D50}
{$ENDIF}

{$IFDEF DP60}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
  {$DEFINE D50}
  {$DEFINE D60}
{$ENDIF}

{$IFDEF KX20}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
  {$DEFINE D50}
  {$DEFINE D60}
  {$DEFINE K20}
{$ENDIF}

{$IFDEF DP70}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
  {$DEFINE D50}
  {$DEFINE D60}
  {$DEFINE D70}
{$ENDIF}

{$IFDEF DP90}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
  {$DEFINE D50}
  {$DEFINE D60}
  {$DEFINE D70}
  {$DEFINE D90}
{$ENDIF}

{$IFDEF FP20}
  {$DEFINE D10}
  {$DEFINE D20}
  {$DEFINE D30}
  {$DEFINE D40}
  {$DEFINE D50}
  {$DEFINE D60}
  {$DEFINE D70}
{$ENDIF}

{$IFDEF D10}
  {$DEFINE USE_EXPORTS}
{$ENDIF}

{$IFDEF D30}
  {$DEFINE USE_INTERFACES}
  {$DEFINE USE_PROVIDER}
{$ENDIF}

{$IFDEF D40}
  {$DEFINE USE_OVERLOAD}
  {$DEFINE USE_IMPLEMENTS}
  {$DEFINE USE_DYNARRAYS}
  {$DEFINE USE_DEFPARAMS}
  {$DEFINE USE_HRESULT}
  {$DEFINE USE_REINTRODUCE}
  {$DEFINE USE_CONST_DISPATCH}
{$ENDIF}

{$IFDEF D50}
  {$DEFINE USE_FRAMES}
  {$UNDEF  USE_PROVIDER}
{$ENDIF}

{$IFDEF D60}
  {$DEFINE USE_DEPRECATED}
  {$DEFINE USE_PLATFORM}
  {$DEFINE USE_LIBRARY}
  {$WARN SYMBOL_LIBRARY OFF}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}

{$IFDEF D70}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$DEFINE USE_FORCEDENUM}
{$ENDIF}

