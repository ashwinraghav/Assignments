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

unit SilLgDataTypeDefs;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBeDataType,
  SilLhDataType,
  SilLmDataHandler,
  SilLmDataTypeSmallint,
  SilLmDataTypeLongInt,      
  SilLmDataTypeSingle,       
  SilLmDataTypeDouble,       
  SilLmDataTypeCurrency,     
  SilLmDataTypeDateTime,         
  SilLmDataTypeWideString,   
  SilLmDataTypeDispatch,     
  SilLmDataTypeVariant,      
  SilLmDataTypeInterface,    
  SilLmDataTypeExtended,
  SilLmDataTypeShortInt,
  SilLmDataTypeByte,
  SilLmDataTypeWord,
  SilLmDataTypeLongWord,
  SilLmDataTypeLargeInt,
  SilLmDataTypeGuid,
  SilLmDataTypeClass,
  SilLmDataTypeObject,
  SilLmDataTypeBoolean,
  SilLmDataTypeAnsiString;

const
  GTypeEmpty        : RDataTypeDef    = ( Name: 'Empty'         ; DataType: (Value:  dtEmpty        ; ) ; Handler: TSilDataHandler            ; );
  GTypeNull         : RDataTypeDef    = ( Name: 'Null'          ; DataType: (Value:  dtNull         ; ) ; Handler: TSilDataHandler            ; );
  GTypeSmallint     : RDataTypeDef    = ( Name: 'Smallint'      ; DataType: (Value:  dtSmallint     ; ) ; Handler: TSilDataHandlerSmallint    ; );
  GTypeLongInt      : RDataTypeDef    = ( Name: 'LongInt'       ; DataType: (Value:  dtLongInt      ; ) ; Handler: TSilDataHandlerLongInt     ; );
  GTypeSingle       : RDataTypeDef    = ( Name: 'Single'        ; DataType: (Value:  dtSingle       ; ) ; Handler: TSilDataHandlerSingle      ; );
  GTypeDouble       : RDataTypeDef    = ( Name: 'Double'        ; DataType: (Value:  dtDouble       ; ) ; Handler: TSilDataHandlerDouble      ; );
  GTypeCurrency     : RDataTypeDef    = ( Name: 'Currency'      ; DataType: (Value:  dtCurrency     ; ) ; Handler: TSilDataHandlerCurrency    ; );
  GTypeDate         : RDataTypeDef    = ( Name: 'Date'          ; DataType: (Value:  dtDate         ; ) ; Handler: TSilDataHandlerDateTime    ; );
  GTypeWideString   : RDataTypeDef    = ( Name: 'WideString'    ; DataType: (Value:  dtWideString   ; ) ; Handler: TSilDataHandlerWideString  ; );
  GTypeDispatch     : RDataTypeDef    = ( Name: 'Dispatch'      ; DataType: (Value:  dtDispatch     ; ) ; Handler: TSilDataHandlerDispatch    ; );
  GTypeError        : RDataTypeDef    = ( Name: 'Error'         ; DataType: (Value:  dtError        ; ) ; Handler: TSilDataHandler            ; );
  GTypeWordBool     : RDataTypeDef    = ( Name: 'WordBool'      ; DataType: (Value:  dtWordBool     ; ) ; Handler: TSilDataHandlerBoolean     ; );
  GTypeVariant      : RDataTypeDef    = ( Name: 'Variant'       ; DataType: (Value:  dtVariant      ; ) ; Handler: TSilDataHandlerVariant     ; );
  GTypeInterface    : RDataTypeDef    = ( Name: 'Interface'     ; DataType: (Value:  dtInterface    ; ) ; Handler: TSilDataHandlerInterface   ; );
  GTypeDecimal      : RDataTypeDef    = ( Name: 'Decimal'       ; DataType: (Value:  dtDecimal      ; ) ; Handler: TSilDataHandler            ; );
  GTypeExtended     : RDataTypeDef    = ( Name: 'Extended'      ; DataType: (Value:  dtExtended     ; ) ; Handler: TSilDataHandlerExtended    ; );
  GTypeShortInt     : RDataTypeDef    = ( Name: 'ShortInt'      ; DataType: (Value:  dtShortInt     ; ) ; Handler: TSilDataHandlerShortInt    ; );
  GTypeByte         : RDataTypeDef    = ( Name: 'Byte'          ; DataType: (Value:  dtByte         ; ) ; Handler: TSilDataHandlerByte        ; );
  GTypeWord         : RDataTypeDef    = ( Name: 'Word'          ; DataType: (Value:  dtWord         ; ) ; Handler: TSilDataHandlerWord        ; );
  GTypeLongWord     : RDataTypeDef    = ( Name: 'LongWord'      ; DataType: (Value:  dtLongWord     ; ) ; Handler: TSilDataHandlerLongWord    ; );
  GTypeLargeInt     : RDataTypeDef    = ( Name: 'LargeInt'      ; DataType: (Value:  dtLargeInt     ; ) ; Handler: TSilDataHandlerLargeInt    ; );
  GTypeLargeWord    : RDataTypeDef    = ( Name: 'LargeWord'     ; DataType: (Value:  dtLargeWord    ; ) ; Handler: TSilDataHandlerLargeInt    ; );  // <--- WARNING!
  GTypeInteger      : RDataTypeDef    = ( Name: 'Integer'       ; DataType: (Value:  dtInteger      ; ) ; Handler: TSilDataHandlerLongInt     ; );
  GTypeCardinal     : RDataTypeDef    = ( Name: 'Cardinal'      ; DataType: (Value:  dtCardinal     ; ) ; Handler: TSilDataHandlerLongWord    ; );
  GTypeVoid         : RDataTypeDef    = ( Name: 'Void'          ; DataType: (Value:  dtVoid         ; ) ; Handler: TSilDataHandler            ; );
  GTypeHRESULT      : RDataTypeDef    = ( Name: 'HRESULT'       ; DataType: (Value:  dtHRESULT      ; ) ; Handler: TSilDataHandler            ; );
  GTypePointer      : RDataTypeDef    = ( Name: 'Pointer'       ; DataType: (Value:  dtPointer      ; ) ; Handler: TSilDataHandler            ; );
  GTypeSafearray    : RDataTypeDef    = ( Name: 'Safearray'     ; DataType: (Value:  dtSafearray    ; ) ; Handler: TSilDataHandler            ; );
  GTypeArray        : RDataTypeDef    = ( Name: 'Array'         ; DataType: (Value:  dtDynarray     ; ) ; Handler: TSilDataHandler            ; );
  GTypeUserdefined  : RDataTypeDef    = ( Name: 'UserDefined'   ; DataType: (Value:  dtUserdefined  ; ) ; Handler: TSilDataHandler            ; );
  GTypePAnsiChar    : RDataTypeDef    = ( Name: 'PAnsiChar'     ; DataType: (Value:  dtPAnsiChar    ; ) ; Handler: TSilDataHandler            ; );
  GTypePWideChar    : RDataTypeDef    = ( Name: 'PWideChar'     ; DataType: (Value:  dtPWideChar    ; ) ; Handler: TSilDataHandler            ; );
  GTypeGuid         : RDataTypeDef    = ( Name: 'Guid'          ; DataType: (Value:  dtGuid         ; ) ; Handler: TSilDataHandlerGuid        ; );
  GTypeClass        : RDataTypeDef    = ( Name: 'Class'         ; DataType: (Value:  dtClass        ; ) ; Handler: TSilDataHandlerClass       ; );
  GTypeObject       : RDataTypeDef    = ( Name: 'Object'        ; DataType: (Value:  dtObject       ; ) ; Handler: TSilDataHandlerObject      ; );
  GTypeBoolean      : RDataTypeDef    = ( Name: 'Boolean'       ; DataType: (Value:  dtBoolean      ; ) ; Handler: TSilDataHandlerBoolean     ; );
  GTypeLongBool     : RDataTypeDef    = ( Name: 'LongBool'      ; DataType: (Value:  dtLongBool     ; ) ; Handler: TSilDataHandler            ; );
  GTypeAnsiChar     : RDataTypeDef    = ( Name: 'AnsiChar'      ; DataType: (Value:  dtAnsiChar     ; ) ; Handler: TSilDataHandler            ; );
  GTypeWideChar     : RDataTypeDef    = ( Name: 'WideChar'      ; DataType: (Value:  dtWideChar     ; ) ; Handler: TSilDataHandler            ; );
  GTypeAnsiString   : RDataTypeDef    = ( Name: 'AnsiString'    ; DataType: (Value:  dtAnsiString   ; ) ; Handler: TSilDataHandlerAnsiString  ; );

const
  GTypedefs: array[TDataType] of PDataTypeDef = (
      @GTypeEmpty               ,
      @GTypeNull                ,
      @GTypeSmallint            ,
      @GTypeLongInt             ,
      @GTypeSingle              ,
      @GTypeDouble              ,
      @GTypeCurrency            ,
      @GTypeDate                ,
      @GTypeWideString          ,
      @GTypeDispatch            ,
      @GTypeError               ,
      @GTypeWordBool            ,
      @GTypeVariant             ,
      @GTypeInterface           ,
      @GTypeDecimal             ,
      @GTypeExtended            ,
      @GTypeShortInt            ,
      @GTypeByte                ,
      @GTypeWord                ,
      @GTypeLongWord            ,
      @GTypeLargeInt            ,
      @GTypeLargeWord           ,
      @GTypeInteger             ,
      @GTypeCardinal            ,
      @GTypeVoid                ,
      @GTypeHRESULT             ,
      @GTypePointer             ,
      @GTypeSafearray           ,
      @GTypeArray               ,
      @GTypeUserdefined         ,
      @GTypePAnsiChar           ,
      @GTypePWideChar           ,
      @GTypeGuid                ,
      @GTypeClass               ,
      @GTypeObject              ,
      @GTypeBoolean             ,
      @GTypeLongBool            ,
      @GTypeAnsiChar            ,
      @GTypeWideChar            ,
      @GTypeAnsiString
    );                          

implementation
end.
 