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

unit SilLsValue;

{$I Defines.inc}

interface

uses
  SilLiValue,
  SilLmDataTypeAnsiString,
  SilLmDataTypeBoolean,
  SilLmDataTypeByte,
  SilLmDataTypeCurrency,
  SilLmDataTypeDateTime,
  SilLmDataTypeDouble,
  SilLmDataTypeExtended,
  SilLmDataTypeGuid,
  SilLmDataTypeLargeInt,
  SilLmDataTypeLongInt,
  SilLmDataTypeLongWord,
  SilLmDataTypeShortInt,
  SilLmDataTypeSingle,
  SilLmDataTypeSmallint,
  SilLmDataTypeVariant,
  SilLmDataTypeWideString,
  SilLmDataTypeWord;

type
  TSilValueAnsiString = SilLmDataTypeAnsiString._ValueClass;
  TSilValueBoolean    = SilLmDataTypeBoolean._ValueClass;
  TSilValueByte       = SilLmDataTypeByte._ValueClass;
  TSilValueCurrency   = SilLmDataTypeCurrency._ValueClass;
  TSilValueDateTime   = SilLmDataTypeDateTime._ValueClass;
  TSilValueDouble     = SilLmDataTypeDouble._ValueClass;
  TSilValueExtended   = SilLmDataTypeExtended._ValueClass;
  TSilValueGuid       = SilLmDataTypeGuid._ValueClass;
  TSilValueLargeInt   = SilLmDataTypeLargeInt._ValueClass;
  TSilValueLongInt    = SilLmDataTypeLongInt._ValueClass;
  TSilValueLongWord   = SilLmDataTypeLongWord._ValueClass;
  TSilValueShortInt   = SilLmDataTypeShortInt._ValueClass;
  TSilValueSingle     = SilLmDataTypeSingle._ValueClass;
  TSilValueSmallint   = SilLmDataTypeSmallint._ValueClass;
  TSilValueVariant    = SilLmDataTypeVariant._ValueClass;
  TSilValueWideString = SilLmDataTypeWideString._ValueClass;
  TSilValueWord       = SilLmDataTypeWord._ValueClass;

type
  TSilVariableAnsiString = SilLmDataTypeAnsiString._VariableClass;
  TSilVariableBoolean    = SilLmDataTypeBoolean._VariableClass;
  TSilVariableByte       = SilLmDataTypeByte._VariableClass;
  TSilVariableCurrency   = SilLmDataTypeCurrency._VariableClass;
  TSilVariableDateTime   = SilLmDataTypeDateTime._VariableClass;
  TSilVariableDouble     = SilLmDataTypeDouble._VariableClass;
  TSilVariableExtended   = SilLmDataTypeExtended._VariableClass;
  TSilVariableGuid       = SilLmDataTypeGuid._VariableClass;
  TSilVariableLargeInt   = SilLmDataTypeLargeInt._VariableClass;
  TSilVariableLongInt    = SilLmDataTypeLongInt._VariableClass;
  TSilVariableLongWord   = SilLmDataTypeLongWord._VariableClass;
  TSilVariableShortInt   = SilLmDataTypeShortInt._VariableClass;
  TSilVariableSingle     = SilLmDataTypeSingle._VariableClass;
  TSilVariableSmallint   = SilLmDataTypeSmallint._VariableClass;
  TSilVariableVariant    = SilLmDataTypeVariant._VariableClass;
  TSilVariableWideString = SilLmDataTypeWideString._VariableClass;
  TSilVariableWord       = SilLmDataTypeWord._VariableClass;

implementation
end.
 