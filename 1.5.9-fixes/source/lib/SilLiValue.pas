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

unit SilLiValue;

{$I Defines.inc}

interface

uses
  SilBeDataType,
  SilBeTypes,
  SilBeError,
  SilBeTypeInfo,
  SilLiDataType,
  SilLiValueChar,
  SilLiValueAnsiString,
  SilLiValueBoolean,
  SilLiValueByte,
  SilLiValueShortint,
  SilLiValueWord,
  SilLiValueSmallint,
  SilLiValueCurrency,
  SilLiValueDateTime,
  SilLiValueSingle,
  SilLiValueDouble,
  SilLiValueExtended,
  SilLiValueInteger,
  SilLiValueInterface,
  SilLiValueLargeInt,
  SilLiValueLongWord,
  SilLiValueWideString,
  SilLiValueGuid,
  SilLiValuePointer,
  SilLiValueVariant;

type
  IValueChar              = SilLiValueChar._Value;
  IValueAnsiString        = SilLiValueAnsiString._Value;
  IValueBoolean           = SilLiValueBoolean._Value;
  IValueByte              = SilLiValueByte._Value;
  IValueShortint          = SilLiValueShortint._Value;
  IValueWord              = SilLiValueWord._Value;
  IValueSmallint          = SilLiValueSmallint._Value;
  IValueCurrency          = SilLiValueCurrency._Value;
  IValueDateTime          = SilLiValueDateTime._Value;
  IValueDouble            = SilLiValueDouble._Value;
  IValueSingle            = SilLiValueSingle._Value;
  IValueExtended          = SilLiValueExtended._Value;
  IValueInteger           = SilLiValueInteger._Value;
  IValueInterface         = SilLiValueInterface._Value;
  IValueLargeInt          = SilLiValueLargeInt._Value;
  IValueLongWord          = SilLiValueLongWord._Value;
  IValueWideString        = SilLiValueWideString._Value;
  IValueGuid              = SilLiValueGuid._Value;
  IValuePointer           = SilLiValuePointer._Value;
  IValueVariant           = SilLiValueVariant._Value;

type
  IVariableChar           = SilLiValueChar._Variable;
  IVariableAnsiString     = SilLiValueAnsiString._Variable;
  IVariableBoolean        = SilLiValueBoolean._Variable;
  IVariableByte           = SilLiValueByte._Variable;
  IVariableShortint       = SilLiValueShortint._Variable;
  IVariableWord           = SilLiValueWord._Variable;
  IVariableSmallint       = SilLiValueSmallint._Variable;
  IVariableCurrency       = SilLiValueCurrency._Variable;
  IVariableDateTime       = SilLiValueDateTime._Variable;
  IVariableDouble         = SilLiValueDouble._Variable;
  IVariableSingle         = SilLiValueSingle._Variable;
  IVariableExtended       = SilLiValueExtended._Variable;
  IVariableInteger        = SilLiValueInteger._Variable;
  IVariableInterface      = SilLiValueInterface._Variable;
  IVariableLargeInt       = SilLiValueLargeInt._Variable;
  IVariableLongWord       = SilLiValueLongWord._Variable;
  IVariableWideString     = SilLiValueWideString._Variable;
  IVariableGuid           = SilLiValueGuid._Variable;
  IVariablePointer        = SilLiValuePointer._Variable;
  IVariableVariant        = SilLiValueVariant._Variable;

type
  IConstant               = SilLiValueVariant.IValueVariant;
  IVariant                = SilLiValueVariant.IVariableVariant;

type
  IDataType1 = interface
    ['{427301FC-44B8-4B25-A57A-5EAA08EE4EF4}']
    function GetDataType: TDataFieldType;
    function GetSize: LongWord;
    property Value: TDataFieldType read GetDataType;
    property Size: LongWord read GetSize;
  end;

  IDataTypeDef1 = interface (IDataType1)
    ['{8CF978C7-FD3B-4BF5-BBCD-7B437F6D2AAD}']
    procedure SetDataType(Value: TDataFieldType);
    procedure SetSize(Value: LongWord);
    property Value: TDataFieldType read GetDataType write SetDataType;
    property Size: LongWord read GetSize write SetSize;
  end;

type
  IValue = interface
    ['{6ADF80C7-7DA8-46F9-983D-EDBCA73E1421}']
    function GetValueVariant: IConstant;
    function GetValueChar: IValueChar;
    function GetValueByte: IValueByte;
    function GetValueShortint: IValueShortint;
    function GetValueWord: IValueWord;
    function GetValueSmallint: IValueSmallint;
    function GetValueString: IValueAnsiString;
    function GetValueWideString: IValueWideString;
    function GetValueInteger: IValueInteger;
    function GetValueLargeInt: IValueLargeInt;
    function GetValueLongWord: IValueLongWord;
    function GetValueBoolean: IValueBoolean;
    function GetValueDouble: IValueDouble;
    function GetValueSingle: IValueSingle;
    function GetValueExtended: IValueExtended;
    function GetValueCurrency: IValueCurrency;
    function GetValueDateTime: IValueDateTime;
    function GetValueGuid: IValueGuid;
    function GetValuePointer: IValuePointer;
    function GetValueInterface: IValueInterface;
    property Char: IValueChar read GetValueChar;
    property Byte: IValueByte read GetValueByte;
    property Shortint: IValueShortint read GetValueShortint;
    property Word: IValueWord read GetValueWord;
    property Smallint: IValueSmallint read GetValueSmallint;
    property AnsiString: IValueAnsiString read GetValueString;
    property WideString: IValueWideString read GetValueWideString;
    property Integer: IValueInteger read GetValueInteger;
    property LargeInt: IValueLargeInt read GetValueLargeInt;
    property LongWord: IValueLongWord read GetValueLongWord;
    property Boolean: IValueBoolean read GetValueBoolean;
    property Double: IValueDouble read GetValueDouble;
    property Single: IValueSingle read GetValueSingle;
    property Extended: IValueExtended read GetValueExtended;
    property Currency: IValueCurrency read GetValueCurrency;
    property DateTime: IValueDateTime read GetValueDateTime;
    property Guid: IValueGuid read GetValueGuid;
    property Pointer: IValuePointer read GetValuePointer;
    property Unknown: IValueInterface read GetValueInterface;
  end;

  IVariable = interface (IValue)
    ['{A1977464-C16A-4707-9ED9-753B3ED2EA22}']
    function GetVariableVariant: IVariant;
    function GetVariableChar: IVariableChar;
    function GetVariableByte: IVariableByte;
    function GetVariableShortint: IVariableShortint;
    function GetVariableWord: IVariableWord;
    function GetVariableSmallint: IVariableSmallint;
    function GetVariableString: IVariableAnsiString;
    function GetVariableWideString: IVariableWideString;
    function GetVariableInteger: IVariableInteger;
    function GetVariableLargeInt: IVariableLargeInt;
    function GetVariableLongWord: IVariableLongWord;
    function GetVariableBoolean: IVariableBoolean;
    function GetVariableDouble: IVariableDouble;
    function GetVariableSingle: IVariableSingle;
    function GetVariableExtended: IVariableExtended;
    function GetVariableCurrency: IVariableCurrency;
    function GetVariableDateTime: IVariableDateTime;
    function GetVariableGuid: IVariableGuid;
    function GetVariablePointer: IVariablePointer;
    function GetVariableInterface: IVariableInterface;
    property Variant: IVariant read GetVariableVariant;
    property Char: IVariableChar read GetVariableChar;
    property Byte: IVariableByte read GetVariableByte;
    property Word: IVariableWord read GetVariableWord;
    property Smallint: IVariableSmallint read GetVariableSmallint;
    property AnsiString: IVariableAnsiString read GetVariableString;
    property WideString: IVariableWideString read GetVariableWideString;
    property Integer: IVariableInteger read GetVariableInteger;
    property LargeInt: IVariableLargeInt read GetVariableLargeInt;
    property LongWord: IVariableLongWord read GetVariableLongWord;
    property Boolean: IVariableBoolean read GetVariableBoolean;
    property Double: IVariableDouble read GetVariableDouble;
    property Single: IVariableSingle read GetVariableSingle;
    property Extended: IVariableExtended read GetVariableExtended;
    property Currency: IVariableCurrency read GetVariableCurrency;
    property DateTime: IVariableDateTime read GetVariableDateTime;
    property Guid: IVariableGuid read GetVariableGuid;
    property Pointer: IVariablePointer read GetVariablePointer;
    property Unknown: IVariableInterface read GetVariableInterface;
  end;

  IValueStorage = interface
    ['{5C7288FE-6987-422D-8B50-C9D925207707}']
    procedure Initialize(const Field: IUnknown; out FieldRef: Pointer);
    procedure Finalize(var FieldRef: Pointer);
    procedure Read(const FieldRef: Pointer; var Buffer; Size: LongWord);
    procedure Write(const FieldRef: Pointer; const Buffer; Size: LongWord);
    function IsAssigned(FieldRef: Pointer): Boolean;
  end;

implementation
end.
