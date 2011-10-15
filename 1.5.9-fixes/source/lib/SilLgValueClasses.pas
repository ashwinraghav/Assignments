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

unit SilLgValueClasses;

{$INCLUDE Defines.inc}

interface                                                

uses
  SilLhValue,
  SilLsValue;

const
  GValueClasses: RValueClasses = (
      nil                         ,   //  dtEmpty
      nil                         ,   //  dtNull
      TSilValueSmallint           ,   //  dtSmallint
      TSilValueLongInt            ,   //  dtLongInt
      TSilValueSingle             ,   //  dtSingle
      TSilValueDouble             ,   //  dtDouble
      TSilValueCurrency           ,   //  dtCurrency
      TSilValueDateTime           ,   //  dtDate
      TSilValueWideString         ,   //  dtWideString
      nil                         ,   //  dtDispatch
      nil                         ,   //  dtError
      nil                         ,   //  dtWordBool
      TSilValueVariant            ,   //  dtVariant
      nil                         ,   //  dtInterface
      nil                         ,   //  dtDecimal
      TSilValueExtended           ,   //  dtExtended
      TSilValueShortInt           ,   //  dtShortInt
      TSilValueByte               ,   //  dtByte
      TSilValueWord               ,   //  dtWord
      TSilValueLongWord           ,   //  dtLongWord
      TSilValueLargeInt           ,   //  dtLargeInt
      TSilValueLargeInt           ,   //  dtLargeWord
      TSilValueLongInt            ,   //  dtInteger
      TSilValueLongWord           ,   //  dtCardinal
      nil                         ,   //  dtVoid
      nil                         ,   //  dtHRESULT
      nil                         ,   //  dtPointer
      nil                         ,   //  dtSafearray
      nil                         ,   //  dtDynarray
      nil                         ,   //  dtUserdefined
      nil                         ,   //  dtPAnsiChar
      nil                         ,   //  dtPWideChar
      TSilValueGUID               ,   //  dtGUID
      nil                         ,   //  dtClass
      nil                         ,   //  dtObject
      TSilValueBoolean            ,   //  dtBoolean
      nil                         ,   //  dtLongBool
      nil                         ,   //  dtAnsiChar
      nil                         ,   //  dtWideChar
      TSilValueAnsiString             //  dtAnsiString
    );

  GVariableClasses: RValueClasses = (
      nil                         ,   //  dtEmpty
      nil                         ,   //  dtNull
      TSilVariableSmallint        ,   //  dtSmallint
      TSilVariableLongInt         ,   //  dtLongInt
      TSilVariableSingle          ,   //  dtSingle
      TSilVariableDouble          ,   //  dtDouble
      TSilVariableCurrency        ,   //  dtCurrency
      TSilVariableDateTime        ,   //  dtDate
      TSilVariableWideString      ,   //  dtWideString
      nil                         ,   //  dtDispatch
      nil                         ,   //  dtError
      nil                         ,   //  dtWordBool
      TSilVariableVariant         ,   //  dtVariant
      nil                         ,   //  dtInterface
      nil                         ,   //  dtDecimal
      TSilVariableExtended        ,   //  dtExtended
      TSilVariableShortInt        ,   //  dtShortInt
      TSilVariableByte            ,   //  dtByte
      TSilVariableWord            ,   //  dtWord
      TSilVariableLongWord        ,   //  dtLongWord
      TSilVariableLargeInt        ,   //  dtLargeInt
      TSilVariableLargeInt        ,   //  dtLargeWord
      TSilVariableLongInt         ,   //  dtInteger
      TSilVariableLongWord        ,   //  dtCardinal
      nil                         ,   //  dtVoid
      nil                         ,   //  dtHRESULT
      nil                         ,   //  dtPointer
      nil                         ,   //  dtSafearray
      nil                         ,   //  dtDynarray
      nil                         ,   //  dtUserdefined
      nil                         ,   //  dtPAnsiChar
      nil                         ,   //  dtPWideChar
      TSilVariableGUID            ,   //  dtGUID
      nil                         ,   //  dtClass
      nil                         ,   //  dtObject
      TSilVariableBoolean         ,   //  dtBoolean
      nil                         ,   //  dtLongBool
      nil                         ,   //  dtAnsiChar
      nil                         ,   //  dtWideChar
      TSilVariableAnsiString          //  dtAnsiString
    );

implementation
end.
 