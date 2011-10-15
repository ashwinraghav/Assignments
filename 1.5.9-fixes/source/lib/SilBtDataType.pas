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

unit SilBtDataType;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilBeTypeInfo,
  SilBeDataType;

type
  DataTypeClass = class of DataTypeTool;

  DataTypeTool = class (Tool)
    class function ToFieldType(const Data: TDataType): TDataFieldType; overload;
    class function ToFieldType(const Data: RDataType): TDataFieldType; overload;
    class function FromFieldType(const Data: TDataFieldType): RDataType; overload;
    class function ToVarType(const Data: TDataType; Flags: TDataFlags = []): TVarType; overload;
    class function ToVarType(const Data: RDataType): TVarType; overload;
    class function FromVarType(const Data: TVarType): RDataType; overload;
    class function DataType(const Data: TDataType; Flags: TDataFlags = []; Size: Byte = 0): RDataType; overload;
    class function FromTypeKind(const Data: TTypeKind): RDataType; overload;
    class function FromTypeInfo(const Data: PTypeInfo): RDataType; overload;
    class function ToTypeInfo(const Data: TDataType; Flags: TDataFlags = []): PTypeInfo; overload;
    class function ToTypeInfo(const Data: RDataType): PTypeInfo; overload;
    class function FromVarRec(VType: Byte): RDataType; overload;
    class function ToVarRec(const Data: TDataType; Flags: TDataFlags = []): Byte; overload;
    class function ToVarRec(const Data: RDataType): Byte; overload;
  end;

  DataType = DataTypeTool;

implementation

uses
  SilBtTypeTool;

{ DataTypeTool }

class function DataTypeTool.ToFieldType(const Data: TDataType): TDataFieldType;
begin
  case Data of
    dtSmallint          :   Result := ftSmallInt;
    dtLongInt           :   Result := ftInteger;
    dtSingle            :   Result := ftFloat;
    dtDouble            :   Result := ftFloat;
    dtCurrency          :   Result := ftCurrency;
    dtDate              :   Result := ftDateTime;
    dtWideString        :   Result := ftWideString;
    dtDispatch          :   Result := ftInterface;
    dtError             :   Result := ftLongWord;
    dtWordBool          :   Result := ftBoolean;
    dtVariant           :   Result := ftVariant;
    dtInterface         :   Result := ftInterface;
    dtDecimal           :   Result := ftFloat;
    dtExtended          :   Result := ftFloat;
    dtShortInt          :   Result := ftByte;
    dtByte              :   Result := ftByte;
    dtWord              :   Result := ftWord;
    dtLongWord          :   Result := ftLongWord;
    dtLargeInt          :   Result := ftLargeInt;
    dtLargeWord         :   Result := ftLargeInt;
    dtInteger           :   Result := ftInteger;
    dtCardinal          :   Result := ftLongWord;
    dtHRESULT           :   Result := ftLongWord;
    dtPointer           :   Result := ftPointer;
    dtSafearray         :   Result := ftPointer;
    dtDynarray          :   Result := ftPointer;
    dtPAnsiChar         :   Result := ftPointer;
    dtPWideChar         :   Result := ftPointer;
    dtGUID              :   Result := ftGuid;
    dtClass             :   Result := ftPointer;
    dtObject            :   Result := ftPointer;
    dtBoolean           :   Result := ftBoolean;
    dtLongBool          :   Result := ftBoolean;
    dtAnsiChar          :   Result := ftChar;
    dtWideChar          :   Result := ftWord;
    else                    Result := ftUnknown;
  end;
end;

class function DataTypeTool.ToFieldType(const Data: RDataType): TDataFieldType;
begin
  Result := ToFieldType(Data.Value);
end;

class function DataTypeTool.FromFieldType(const Data: TDataFieldType): RDataType;
begin
  case Data of
    ftChar            :   Result := DataType(dtAnsiChar);
    ftString          :   Result := DataType(dtEmpty, [dfPointer]);
    ftWideString      :   Result := DataType(dtWideString);
    ftSmallInt        :   Result := DataType(dtSmallint);
    ftInteger         :   Result := DataType(dtLongInt);
    ftLargeInt        :   Result := DataType(dtLargeInt);
    ftByte            :   Result := DataType(dtByte);
    ftWord            :   Result := DataType(dtWord);
    ftLongWord        :   Result := DataType(dtLongWord);
    ftBoolean         :   Result := DataType(dtWordBool);
    ftFloat           :   Result := DataType(dtDouble);
    ftCurrency        :   Result := DataType(dtCurrency);
    ftDate            :   Result := DataType(dtDate);
    ftTime            :   Result := DataType(dtDate);
    ftDateTime        :   Result := DataType(dtDate);
    ftBytes           :   Result := DataType(dtByte, [dfPointer]);
    ftVarBytes        :   Result := DataType(dtByte, [dfPointer]);
    ftAutoInc         :   Result := DataType(dtInteger);
    ftBlob            :   Result := DataType(dtByte, [dfPointer]);
    ftMemo            :   Result := DataType(dtAnsiChar, [dfPointer]);
    ftVariant         :   Result := DataType(dtVariant);
    ftGuid            :   Result := DataType(dtGUID);
    ftInterface       :   Result := DataType(dtInterface);
    ftPointer         :   Result := DataType(dtPointer);
    else                  Result := DataType(dtUnknown);
  end;
end;

class function DataTypeTool.ToVarType(const Data: TDataType; Flags: TDataFlags = []): TVarType;
begin
  case Data of
    dtSmallint          :   Result := varSmallInt;
    dtLongInt           :   Result := varInteger;
    dtSingle            :   Result := varSingle;
    dtDouble            :   Result := varDouble;
    dtCurrency          :   Result := varCurrency;
    dtDate              :   Result := varDate;
    dtWideString        :   Result := varOleStr;
    dtDispatch          :   Result := varDispatch;
    dtError             :   Result := varError;
    dtWordBool          :   Result := varBoolean;
    dtVariant           :   Result := varVariant;
    dtInterface         :   Result := varUnknown;
    dtShortInt          :   Result := varShortInt;
    dtByte              :   Result := varByte;
    dtWord              :   Result := varWord;
    dtLongWord          :   Result := varLongWord;
    dtLargeInt          :   Result := varInt64;
    dtInteger           :   Result := varInteger;
    dtCardinal          :   Result := varLongWord;
    else                    Result := varEmpty;
  end;
  if dfArray in Flags then Inc(Result, varArray); 
  if dfByRef in Flags then Inc(Result, varByRef); 
end;

class function DataTypeTool.ToVarType(const Data: RDataType): TVarType;
begin
  Result := ToVarType(Data.Value, Data.Flags);
end;

class function DataTypeTool.FromVarType(const Data: TVarType): RDataType;
begin
  case Data and varTypeMask of
    varEmpty            :   Result := DataType(dtEmpty);
    varNull             :   Result := DataType(dtNull);
    varSmallint         :   Result := DataType(dtSmallint);
    varInteger          :   Result := DataType(dtInteger);
    varSingle           :   Result := DataType(dtSingle);
    varDouble           :   Result := DataType(dtDouble);
    varCurrency         :   Result := DataType(dtCurrency);
    varDate             :   Result := DataType(dtDate);
    varOleStr           :   Result := DataType(dtWideString);
    varDispatch         :   Result := DataType(dtDispatch);
    varError            :   Result := DataType(dtError);
    varBoolean          :   Result := DataType(dtWordBool);
    varVariant          :   Result := DataType(dtVariant);
    varUnknown          :   Result := DataType(dtInterface);
    varShortInt         :   Result := DataType(dtShortInt);
    varByte             :   Result := DataType(dtByte);
    varWord             :   Result := DataType(dtWord);
    varLongWord         :   Result := DataType(dtLongWord);
    varInt64            :   Result := DataType(dtLargeInt);
    varStrArg           :   Result := DataType(dtGUID);
    varString           :   Result := DataType(dtByte, [dfPointer]);
    else                    Result := DataType(dtUnknown);
  end;
  if varArray and Data <> 0 then Include(Result.Flags, dfArray);
  if varByRef and Data <> 0 then Include(Result.Flags, dfByRef);
end;

class function DataTypeTool.DataType(const Data: TDataType; Flags: TDataFlags; Size: Byte): RDataType;
begin
  Result.Value := Data;
  Result.Flags := Flags;
  Result.Size := Size;
end;

class function DataTypeTool.FromTypeKind(const Data: TTypeKind): RDataType;
begin
  case Data of
    tkInteger,tkChar,tkWChar,tkSet,tkEnumeration:
                      Result := DataType(dtInteger);
    tkInt64:          Result := DataType(dtLargeInt);
    tkFloat:          Result := DataType(dtExtended);
    tkString:         Result := DataType(dtAnsiString);
    tkLString:        Result := DataType(dtAnsiString);
    tkWString:        Result := DataType(dtWideString);
    tkVariant:        Result := DataType(dtVariant);
    tkInterface:      Result := DataType(dtInterface);
    tkClass:          Result := DataType(dtClass);
    else              Result := DataType(dtUnknown);
  end;
end;

class function DataTypeTool.FromTypeInfo(const Data: PTypeInfo): RDataType;
begin
  Result := FromTypeKind(Data.Kind);
  case Data.Kind of
    tkInteger,tkChar,tkWChar,tkSet,tkEnumeration:
      case Typ.GetData(Data).OrdType of
        otSByte:      Result := DataType(dtShortInt);
        otUByte:      Result := DataType(dtByte);
        otSWord:      Result := DataType(dtSmallint);
        otUWord:      Result := DataType(dtWord);
        otSLong:      Result := DataType(dtLongInt);
        otULong:      Result := DataType(dtLongWord);
      end;
    tkInt64:          Result := DataType(dtLargeInt);   
    tkFloat:
      case Typ.GetData(Data).FloatType of
        ftSingle:     Result := DataType(dtSingle);
        ftDouble:     Result := DataType(dtDouble);
        ftExtended:   Result := DataType(dtExtended);
        ftCurr:       Result := DataType(dtCurrency);
        else          Result := DataType(dtUnknown);
      end;
    tkString:         Result := DataType(dtAnsiString);
    tkLString:        Result := DataType(dtAnsiString);
    tkWString:        Result := DataType(dtWideString);
    tkVariant:        Result := DataType(dtVariant);
    tkInterface:      Result := DataType(dtInterface);
    tkClass:          Result := DataType(dtClass);
    //tkRecord,
    //tkArray,
    //tkDynArray,
    //tkMethod,
    else              Result := DataType(dtUnknown);
  end;
end;

class function DataTypeTool.ToTypeInfo(const Data: TDataType; Flags: TDataFlags): PTypeInfo;
begin
  case Data of
    dtSmallint      : Result := System.TypeInfo(Smallint);
    dtLongInt       : Result := System.TypeInfo(LongInt); 
    dtSingle        : Result := System.TypeInfo(Single);
    dtDouble        : Result := System.TypeInfo(Double);
    dtCurrency      : Result := System.TypeInfo(Currency);
    dtDate          : Result := System.TypeInfo(TDateTime);
    dtWideString    : Result := System.TypeInfo(WideString);
    dtDispatch      : Result := System.TypeInfo(IDispatch);
    dtError         : Result := System.TypeInfo(HRESULT);
    dtWordBool      : Result := System.TypeInfo(WordBool);
    dtVariant       : Result := System.TypeInfo(Variant);
    dtInterface     : Result := System.TypeInfo(IUnknown);
    dtExtended      : Result := System.TypeInfo(Extended);
    dtShortInt      : Result := System.TypeInfo(ShortInt);
    dtByte          : Result := System.TypeInfo(Byte);
    dtWord          : Result := System.TypeInfo(Word);
    dtLongWord      : Result := System.TypeInfo(LongWord);
    dtLargeInt      : Result := System.TypeInfo(Int64);
    dtInteger       : Result := System.TypeInfo(Integer);
    dtCardinal      : Result := System.TypeInfo(Cardinal);
    //dtVoid          : Result := System.TypeInfo(Pointer);
    dtHRESULT       : Result := System.TypeInfo(HRESULT);
    //dtPointer       : Result := System.TypeInfo(Smallint);
    //dtSafearray     : Result := System.TypeInfo(Smallint);
    //dtDynarray      : Result := System.TypeInfo(Smallint);
    //dtUserdefined   : Result := System.TypeInfo(Smallint);
    //dtPAnsiChar     : Result := System.TypeInfo(PAnsiChar);
    //dtPWideChar     : Result := System.TypeInfo(PWideChar);
    //dtGUID          : Result := System.TypeInfo(TGUID);
    //dtClass         : Result := System.TypeInfo(TClass);
    dtObject        : Result := System.TypeInfo(TObject);
    dtBoolean       : Result := System.TypeInfo(Boolean);
    dtLongBool      : Result := System.TypeInfo(LongBool);
    dtAnsiChar      : Result := System.TypeInfo(AnsiChar);
    dtWideChar      : Result := System.TypeInfo(WideChar);
    dtAnsiString    : Result := System.TypeInfo(AnsiString);
    else              Result := nil;
  end;
end;

class function DataTypeTool.ToTypeInfo(const Data: RDataType): PTypeInfo;
begin
  Result := ToTypeInfo(Data.Value, Data.Flags);
end;

class function DataTypeTool.FromVarRec(VType: Byte): RDataType;
begin
  case VType of
    vtInteger:        Result := DataType(dtInteger);
    vtBoolean:        Result := DataType(dtBoolean);
    vtChar:           Result := DataType(dtAnsiChar);
    vtExtended:       Result := DataType(dtExtended);
    vtString:         Result := DataType(dtAnsiChar, [dfArray], 255);
    vtPointer:        Result := DataType(dtPointer);
    vtPChar:          Result := DataType(dtPAnsiChar);
    vtObject:         Result := DataType(dtObject);
    vtClass:          Result := DataType(dtClass);
    vtWideChar:       Result := DataType(dtWideChar);
    vtPWideChar:      Result := DataType(dtPWideChar);
    vtAnsiString:     Result := DataType(dtAnsiString);
    vtCurrency:       Result := DataType(dtCurrency);
    vtVariant:        Result := DataType(dtVariant);
    vtInterface:      Result := DataType(dtInterface);
    vtWideString:     Result := DataType(dtWideString);
    vtInt64:          Result := DataType(dtLargeInt); 
    else              Result := DataType(dtUnknown); 
  end;
end;

class function DataTypeTool.ToVarRec(const Data: TDataType; Flags: TDataFlags): Byte;
begin
  case Data of
    dtEmpty         : Result := vtVariant;
    dtNull          : Result := vtVariant;
    dtSmallint      : Result := vtInteger;
    dtLongInt       : Result := vtInteger;
    dtSingle        : Result := vtExtended;
    dtDouble        : Result := vtExtended;
    dtCurrency      : Result := vtCurrency;
    dtDate          : Result := vtExtended;
    dtWideString    : Result := vtWideString;
    dtDispatch      : Result := vtInterface;
    dtError         : Result := vtInteger;
    dtWordBool      : Result := vtBoolean;
    dtVariant       : Result := vtVariant;
    dtInterface     : Result := vtInterface;
    dtDecimal       : Result := vtExtended;
    dtExtended      : Result := vtExtended;
    dtShortInt      : Result := vtInteger;
    dtByte          : Result := vtInteger;
    dtWord          : Result := vtInteger;
    dtLongWord      : Result := vtInteger;
    dtLargeInt      : Result := vtInt64;
    dtLargeWord     : Result := vtVariant;
    dtInteger       : Result := vtInteger;
    dtCardinal      : Result := vtInteger;
    dtVoid          : Result := vtPointer;
    dtHRESULT       : Result := vtInteger;
    dtPointer       : Result := vtPointer;
    dtSafearray     : Result := vtVariant;
    dtDynarray      : Result := vtPointer;
    dtUserdefined   : Result := vtPointer;
    dtPAnsiChar     : Result := vtPChar;
    dtPWideChar     : Result := vtPWideChar;
    dtGUID          : Result := vtPointer;
    dtClass         : Result := vtClass;
    dtObject        : Result := vtObject;
    dtBoolean       : Result := vtBoolean;
    dtLongBool      : Result := vtBoolean;
    dtAnsiChar      : Result := vtChar;
    dtWideChar      : Result := vtWideChar;
    dtAnsiString    : Result := vtAnsiString;
    else              Result := vtVariant;
  end;
end;

class function DataTypeTool.ToVarRec(const Data: RDataType): Byte;
begin
  Result := ToVarRec(Data.Value, Data.Flags);
end;

end.
