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

unit SilBtVart;

{$I Defines.inc}

interface

uses
  SysUtils,
  SilBeTypes,
  SilBeDataType,
  SilBkTool;

type
  RVariantInfo = record
    Data: Pointer;
    Size: Integer;
  end;

  Vart = class(Tool)
    class procedure Clear(var V: Variant);
    class procedure Copy(var V: Variant; const Value: Variant);
    class function Compare(const V1, V2: Variant): Integer;
    class function Value(const V, IfEmpty, IfNull: Variant; AType: Word = varString): Variant;
    class function VType(const V: Variant): Word;
    class function VTypeToDataType(VType: Word): TDataFieldType;

    class function ToType(const V, Default: Variant; AType: Word): Variant;
    class function ToBool(const V: Variant; const Default: Boolean {$IFDEF USE_DEFPARAMS} = False {$ENDIF}): Boolean;
    class function ToStr(const V: Variant; const Default: string {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): string;
    class function ToWideStr(const V: Variant; const Default: WideString {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): WideString;
    class function ToLiteral(const V: Variant; const Default: string = ''; const Quotes: Char = ''''): string;
    class function ToInt(const V: Variant; const Default: Integer {$IFDEF USE_DEFPARAMS} = -1 {$ENDIF}): Integer;
    class function ToLarge(const V: Variant; const Default: Integer {$IFDEF USE_DEFPARAMS} = -1 {$ENDIF}): LargeInt;
    class function ToVariant(const V: Variant; const Default: Variant): Variant; overload;
    class function ToVariant(const V: Variant; const Default, Null: Variant): Variant; overload;
    class function ToFloat(const V: Variant; const Default: Double {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}): Double;
    class function ToDateTime(const V: Variant; const Default: TDateTime {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}): TDateTime;
    class function ToUnknown(const V: Variant): IUnknown;
    class function ToDispatch(const V: Variant): IDispatch;
    class function ToInterface(const V: Variant; const IID: TGUID; out Obj): Boolean;
    class function ToPtr(const V: Variant; Default: pointer = nil): Pointer;
    class procedure ToConst(const V: Variant; var Value: TVarRec);
    
    class function ToByteArray(const V: Variant): TByteArray;
    class function ToWordArray(const V: Variant): TWordArray;
    class function ToIntegerArray(const V: Variant): TIntegerArray;
    class function ToLongArray(const V: Variant): TLongArray;

    class function FromByteArray(const V: TByteArray; const AType: Word): Variant;
    class function FromWordArray(const V: TWordArray; const AType: Word = varWord): Variant;
    class function FromIntegerArray(const V: TIntegerArray; const AType: Word): Variant;
    class function FromLongArray(const V: TLongArray; const AType: Word): Variant;

    class function FromDate(const D: TDateTime): Variant;
    class function FromPtr(const P: Pointer): Variant;
    class function FromStr(const S: string; AType: Word; const Empty: Variant): Variant; overload;
    class function FromStr(const S: string; AType: Word = varString): Variant; overload;
    class function FromStr(const S: string; const Empty: Variant): Variant; overload;
    class function FromStr(const S: WideString; AType: Word; const Empty: Variant): Variant; overload;
    class function FromStr(const S: WideString; AType: Word = varOleStr): Variant; overload;
    class function FromStr(const S: WideString; const Empty: Variant): Variant; overload;
    class function FromConst(const Value: TVarRec): Variant;

    class function IsNull(const V: Variant): Boolean; overload; 
    class function IsNull(const V: Variant; const Default: Variant): Variant; overload;
    class function IsEmpty(const V: Variant): Boolean;
    class function IsTrue(const V: Variant; Default: Boolean): Boolean;
    class function IsFalse(const V: Variant; Default: Boolean): Boolean;
    class function IsOK(const V: Variant): Boolean;
    class function IsArray(const V: Variant): Boolean;
    class function IsObject(const V: Variant): Boolean;
    class function IsString(const V: Variant): Boolean;
    class function IsNumber(const V: Variant): Boolean;
    class function IsLarge(const V: Variant): Boolean;
    class function IsInteger(const V: Variant): Boolean;
    class function IsFloat(const V: Variant): Boolean;
    class function IsBoolean(const V: Variant): Boolean;
    class function IsDateTime(const V: Variant): Boolean;

    class function IsEqual(const V1, V2: Variant; Default: Boolean): Boolean;
    class function IsDifferent(const V1, V2: Variant; Default: Boolean): Boolean;
    class function IsGreater(const V1, V2: Variant; Default: Boolean): Boolean;
    class function IsLower(const V1, V2: Variant; Default: Boolean): Boolean;
    class function IsGTE(const V1, V2: Variant; Default: Boolean): Boolean;
    class function IsLTE(const V1, V2: Variant; Default: Boolean): Boolean;
    class function InArray(const V1, V2: Variant; Default: Boolean): Boolean;

    class function IfEmpty(const Value: Variant; const Default: Variant): Variant; overload;
    class function IfEmpty(const Value: Variant): Variant; overload;

    class function IIf(const Expr: Boolean; const RTrue, RFalse: Variant): Variant; overload;
    class function IIf(const Expr: Boolean; const RTrue: Variant): Variant; overload;

    class function Max(const V1, V2: Variant): Variant;
    class function Min(const V1, V2: Variant): Variant;
    class function Choose(const V1, V2: Variant): Variant;
    class procedure Swap(var V1, V2: Variant);

    class function Null: Variant;
    class function Unassigned: Variant;
    class function Empty: Variant; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function EmptyParam: Variant;
    class function Coalesce(const V1: Array of Variant; const Default: Variant): Variant;
    class function GetInfo(const Src: Variant; out Data: Pointer): LongWord;
    class function Info(const Src: Variant): RVariantInfo;
    class procedure Move(const Src: Variant; var Dst; Size: LongWord);
    class function FromGuid(const Value: TGuid): Variant;
    class function ToGuid(const Value: Variant): TGuid;
  end;

implementation

uses
  SilBfVariants,
  SilBtError,
  SilBtMem,
  SilBtStr,
  SilBtVarray,
  SilLtReference,
  SilOsTool;

class function Vart.IsArray(const V: Variant): Boolean;
begin
  Result := VarIsArray(V);
end;

class function Vart.IsObject(const V: Variant): Boolean;
begin
  Result := (VType(V) = varUnknown) or (VType(V) = varDispatch);
end;

class function Vart.IsBoolean(const V: Variant): Boolean;
begin
  Result := (VType(V) = varBoolean);
end;

class function Vart.IsDateTime(const V: Variant): Boolean;
begin
  Result := (VType(V) = varDate);
end;

class function Vart.IsInteger(const V: Variant): Boolean;
begin
  Result := (VType(V) in [varByte, varInteger, varWord, varSmallint, varLongWord, varDecimal]);
end;

class function Vart.IsFloat(const V: Variant): Boolean;
begin
  Result := (VType(V) in [varDouble, varSingle, varCurrency]);
end;

class function Vart.IsLarge(const V: Variant): Boolean;
begin
  Result := VType(V) = varInt64;
end;

class function Vart.IsNumber(const V: Variant): Boolean;
begin
  Result := IsInteger(V) or IsFloat(V) or IsLarge(V);
end;

class function Vart.IsString(const V: Variant): Boolean;
begin
  Result := (VType(V) = varOleStr) or (VType(V) = varString);
end;

class function Vart.IsEmpty(const V: Variant): Boolean;
begin
  Result := VarIsEmpty(V);
end;

class function Vart.IsNull(const V: Variant): Boolean;
begin
  Result := VarIsNull(V);
end;

class function Vart.IsNull(const V, Default: Variant): Variant;
begin
  if IsOK(V) then
    Result := V else
    Result := Default;
end;

class function Vart.InArray(const V1, V2: Variant; Default: Boolean): Boolean;
var
  i1: integer;
begin
  Result := Default;
  if not IsOK(V1) or not IsArray(V2) then exit;

  Result := true;
  for i1 := varArrayLowBound( V2, 1 ) to varArrayHighBound( V2, 1 ) do
    if ( VType(V1) = VType(V2[i1]) ) and ( V1 = V2[i1] ) then
      exit;

  Result := false;
end;

class function Vart.IsEqual(const V1, V2: Variant; Default: Boolean): Boolean;
begin
  if (IsEmpty(V1) and IsEmpty(V2)) or (IsNull(V1) and IsNull(V2)) then
    Result := True
  else if IsOK(V1) and IsOK(V2) then
    Result := Compare(V1, V2) = 0 
  else
    Result := Default;
end;

class function Vart.IsDifferent(const V1, V2: Variant; Default: Boolean): Boolean;
begin
  if (IsEmpty(V1) and IsEmpty(V2)) or (IsNull(V1) and IsNull(V2)) then
    Result := False
  else if IsOK(V1) and IsOK(V2) and (VType(V1) = VType(V2)) then
    Result := Compare(V1, V2) <> 0 
  else
    Result := Default;
end;

class function Vart.IsGTE(const V1, V2: Variant; Default: Boolean): Boolean;
begin
  if IsOK(V1) and IsOK(V2) and (VType(V1) = VType(V2)) then
    Result := (V1 >= V2) else
    Result := Default;
end;

class function Vart.IsLTE(const V1, V2: Variant; Default: Boolean): Boolean;
begin
  if IsOK(V1) and IsOK(V2) and (VType(V1) = VType(V2)) then
    Result := (V1 <= V2) else
    Result := Default;
end;

class function Vart.IsGreater(const V1, V2: Variant; Default: Boolean): Boolean;
begin
  if IsOK(V1) and IsOK(V2) and (VType(V1) = VType(V2)) then
    Result := (V1 > V2) else
    Result := Default;
end;

class function Vart.IsLower(const V1, V2: Variant; Default: Boolean): Boolean;
begin
  if IsOK(V1) and IsOK(V2) and (VType(V1) = VType(V2)) then
    Result := (V1 < V2) else
    Result := Default;
end;

class function Vart.IsOK(const V: Variant): Boolean;
begin
  Result := not (IsEmpty(V) or IsNull(V));
end;

class function Vart.IsFalse(const V: Variant; Default: Boolean): Boolean;
begin
  if IsOK(V)
    then Result := ToBool(V, Default) = False
    else Result := Default;
end;

class function Vart.IsTrue(const V: Variant; Default: Boolean): Boolean;
begin
  if IsOK(V)
    then Result := ToBool(V, Default) = True
    else Result := Default;
end;

class function Vart.ToType(const V, Default: Variant; AType: Word): Variant;
begin
  if IsOK(V) then
    try
      Result := VarAsType(V, Atype);
    except
      Result := Default;
    end
  else
    Result := Default;
end;

class function Vart.ToBool(const V: Variant; const Default: Boolean {$IFDEF USE_DEFPARAMS} = False {$ENDIF}): Boolean;
begin
  Result := ToType(V, Default, varBoolean);
end;

class function Vart.ToStr(const V: Variant; const Default: string {$IFDEF USE_DEFPARAMS} = '' {$ENDIF}): string;
begin
  if not IsArray(V) then
    Result := ToType(V, Default, varString) else
    Result := Varray.ToStr(V, Default);
end;

class function Vart.ToWideStr(const V: Variant; const Default: WideString): WideString;
begin
  if not IsArray(V) then
    Result := ToType(V, Default, varOleStr) else
    Result := Varray.ToWideStr(V, Default);
end;

class function Vart.ToLiteral(const V: Variant; const Default: string; const Quotes: Char): string;
var
  VarType: Integer;
begin
  VarType := VType(V);
  Result := ToType(V, Default, varString);
  if (VarType = varString) or (VarType in [varDate, varOleStr]) then
    Result := Str.Quoted(Result);
end;

class function Vart.ToInt(const V: Variant; const Default: Integer {$IFDEF USE_DEFPARAMS} = -1 {$ENDIF}): Integer;
begin
  Result := ToType(V, Default, varInteger);
end;

class function Vart.ToLarge(const V: Variant; const Default: Integer): LargeInt;
begin
{$IFDEF D60}
  Result := ToType(V, Default, varInt64);
{$ELSE}
  Error.Throw('not supported by delphi < 6.0');
  Result := 0;
{$ENDIF}
end;

class function Vart.ToFloat(const V: Variant; const Default: Double {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}): Double;
begin
  Result := ToType(V, Default, varDouble);
end;

class function Vart.ToDateTime(const V: Variant; const Default: TDateTime {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}): TDateTime;
begin
  Result := ToType(V, Default, varDate);
end;

class function Vart.ToVariant(const V: Variant; const Default: Variant): Variant;
begin
  if IsOK(V) then
    Result := V else
    Result := Default;
end;

class function Vart.ToVariant(const V, Default, Null: Variant): Variant;
begin
  if   (IsOK(V) and IsOK(Null) and not IsEqual(V, Null, True))
    or (IsNull(Null) and not IsNull(V))
    or (IsEmpty(Null) and not IsEmpty(V))   then
    Result := V else
    Result := Default;
end;

class function Vart.FromDate(const D: TDateTime): Variant;
begin
  Result := ToType(D, Unassigned, varDate); 
end;

class function Vart.Value(const V, IfEmpty, IfNull: Variant; AType: Word): Variant;
begin
  case VType(V) of
    varEMPTY: Result := IfEmpty;
    varNULL:  Result := IfNull;
  else
    Result := ToType(V, Unassigned, AType);
  end;
end;

class function Vart.VType(const V: Variant): Word;
begin
  Result := TVarData(V).VType;
end;

class function Vart.IIf(const Expr: Boolean; const RTrue: Variant): Variant;
begin
  if Expr then
    Result := RTrue else
    Result := Vart.Null;
end;

class function Vart.IIf(const Expr: Boolean; const RTrue, RFalse: Variant): Variant;
begin
  if Expr then
    Result := RTrue else
    Result := RFalse;
end;

class function Vart.Max(const V1, V2: Variant): Variant;
begin
  if IsOK(V1) and IsOK(V2) then
    begin
      if V1 > V2 then
        Result := V1 else
        Result := V2;
    end
  else if IsOK(V1) then
    Result := V1
  else
    Result := V2;
end;

class function Vart.Min(const V1, V2: Variant): Variant;
begin
  if IsOK(V1) and IsOK(V2) then
    begin
      if V1 < V2 then
        Result := V1 else
        Result := V2;
    end
  else if IsOK(V1) then
    Result := V1
  else
    Result := V2;
end;

class function Vart.Choose(const V1, V2: Variant): Variant;
begin
  if IsOK(V1) then
    Result := V1 else
    Result := V2;
end;

class procedure Vart.Swap(var V1, V2: Variant);
var
  V: Variant;
begin
  V := V1;
  V1 := V2;
  V2 := V;
end;

class function Vart.ToInterface(const V: Variant; const IID: TGUID; out Obj): Boolean;
var
  Ptr: Pointer;
  Data: TVarData absolute V;
begin
  Result := False;
  Pointer(Obj) := nil;

  case (Data.VType and varTypeMask) of
    varDispatch: Ptr := Data.VDispatch;
    varUnknown:  Ptr := Data.VUnknown;
  else
    Exit;
  end;

  if (Data.VType and varByRef) <> 0
    then Ptr := Pointer(Ptr^);

  Result := Ptr <> nil;
  if Result then
    Result := IUnknown(Ptr).QueryInterface( iid, obj ) = 0;
end;

class function Vart.ToUnknown(const V: Variant): IUnknown;
begin
  if not ToInterface(V, IUnknown, Result) then
    Error.Throw('El variant no contiene un objeto!');
end;

class function Vart.ToDispatch(const V: Variant): IDispatch;
begin
  Result := ToUnknown(V) as IDispatch;
end;

class procedure Vart.Clear(var V: Variant);
begin
  SilBfVariants.VarClear(V);
end;

class procedure Vart.Copy(var V: Variant; const Value: Variant);
begin
  VarCopy(V, Value);
end;

class function Vart.Compare(const V1, V2: Variant): Integer;
begin
  if not IsObject(V1) or not IsObject(V2) then
    case VarCompareValue(V1, V2) of
      vrEqual:        Result :=  0;
      vrLessThan:     Result := -1;
      vrGreaterThan:  Result := +1;
      else            Result := -2;
    end
  else
    Result := Ref.Compare(ToUnknown(V1), ToUnknown(V2));
end;

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
class function Vart.Empty: Variant;
begin
  Result := SilBfVariants.Unassigned;
end;
{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED ON}
{$ENDIF}

class function Vart.Unassigned: Variant;
begin
  Result := SilBfVariants.Unassigned;
end;

class function Vart.EmptyParam: Variant;
begin
  Result := SilBfVariants.EmptyParam;
end;

class function Vart.Null: Variant;
begin
  Result := SilBfVariants.Null;
end;

const // de activex;
  VT_CLSID  = 72;

class function Vart.FromPtr(const P: Pointer): Variant;
begin
  TVarData(Result).VInteger := Integer(P);
  TVarData(Result).VType := varInteger;
end;

class function Vart.FromStr(const S: string; AType: Word; const Empty: Variant): Variant;
begin
  if OsWStr.NotEmpty(S) then
    Result := ToType(S, Empty, AType) else
    Result := Empty;
end;

class function Vart.FromStr(const S: string; AType: Word): Variant;
begin
  Result := FromStr(S, AType, '');
end;

class function Vart.FromStr(const S: string; const Empty: Variant): Variant;
begin
  Result := FromStr(S, varString, Empty);
end;

class function Vart.FromStr(const S: WideString; AType: Word; const Empty: Variant): Variant;
begin
  if Str.NotEmpty(S) then
    Result := ToType(S, Empty, AType) else
    Result := Empty;
end;

class function Vart.FromStr(const S: WideString; AType: Word): Variant;
begin
  Result := FromStr(S, AType, '');
end;

class function Vart.FromStr(const S: WideString; const Empty: Variant): Variant;
begin
  Result := FromStr(S, varOleStr, Empty);
end;

class function Vart.FromConst(const Value: TVarRec): Variant;
begin
  case Value.VType of
    vtInteger     : Result := Value.VInteger;
    vtBoolean     : Result := Value.VBoolean;
    vtChar        : Result := Value.VChar;
    vtExtended    : Result := Value.VExtended^;
    vtString      : Result := AnsiString(Value.VString);
    vtPChar       : Result := AnsiString(Value.VPChar);
    vtWideChar    : Result := Value.VWideChar;
    vtPWideChar   : Result := WideString(Value.VPWideChar);
    vtAnsiString  : Result := AnsiString(Value.VAnsiString);
    vtCurrency    : Result := Value.VCurrency^;
    vtVariant     : Result := Value.VInteger;
    vtInterface   : Result := Value.VInteger;
    vtWideString  : Result := Value.VInteger;
    vtInt64       : Result := Value.VInteger;
    else
  end;
end;

class function Vart.FromGuid(const Value: TGuid): Variant;
begin
  Result := OsGuid.ToStr(Value);
end;

class function Vart.ToGuid(const Value: Variant): TGuid;
begin
  Result := OsGuid.FromStr(Value);
end;

class function Vart.ToPtr(const V: Variant; Default: pointer): Pointer;
begin
  if TVarData(V).VType <> varInteger then
  begin
    //Abort;
    Result := Default;
  end else
    Result := Pointer(TVarData(V).VInteger);
end;

class procedure Vart.ToConst(const V: Variant; var Value: TVarRec);
begin
  
end;

class procedure Vart.Move(const Src: Variant; var Dst; Size: LongWord);
var
  pData: Pointer;
  lwSize: LongWord;
begin
  lwSize := GetInfo(Src, pData);
  if lwSize <= Size then
    System.Move(pData^, Dst, lwSize) else
    Error.Throw('error');
end;

class function Vart.GetInfo(const Src: Variant; out Data: Pointer): LongWord;
begin
  Result := SilBfVariants.VarGetInfo(Src, Data);
end;

class function Vart.Info(const Src: Variant): RVariantInfo;
begin
  Result.Size := GetInfo(Src, Result.Data);
end;

class function Vart.ToByteArray(const V: Variant): TByteArray;
begin
  if not VarIsArray(V) then
    with Info(V) do
      Result := Mem.ToByteArray(Data, Size)
  else
    Result := Varray.ToByteArray(V);
end;

class function Vart.ToIntegerArray(const V: Variant): TIntegerArray;
begin
  if not VarIsArray(V) then
    with Info(V) do
      Result := Mem.ToIntegerArray(Data, Size)
  else
    Result := Varray.ToIntegerArray(V);
end;

class function Vart.ToLongArray(const V: Variant): TLongArray;
begin
  if not VarIsArray(V) then
    with Info(V) do
      Result := Mem.ToLongArray(Data, Size)
  else
    Result := Varray.ToLongArray(V);
end;

class function Vart.ToWordArray(const V: Variant): TWordArray;
begin
  if not VarIsArray(V) then
    with Info(V) do
      Result := Mem.ToWordArray(Data, Size)
  else
    Result := Varray.ToWordArray(V);
end;

class function Vart.FromByteArray(const V: TByteArray; const AType: Word): Variant;
begin
end;

class function Vart.FromIntegerArray(const V: TIntegerArray; const AType: Word): Variant;
begin
end;

class function Vart.FromLongArray(const V: TLongArray; const AType: Word): Variant;
begin
end;

class function Vart.FromWordArray(const V: TWordArray; const AType: Word): Variant;
var
  i: integer;
begin
  Result := VArray.Create([0, Length(v) - 1], AType);

  for i := 0 to Length(v) - 1 do
    Result[i] := v[i];
end;

class function Vart.VTypeToDataType(VType: Word): TDataFieldType;
begin
  case VType of
    varEmpty,
    varNull:      Result := ftUnknown;
    varSmallint,
    varShortInt:  Result := ftSmallInt;
    varInteger:   Result := ftInteger;
    varSingle,
    varDouble:    Result := ftFloat;
    varCurrency:  Result := ftCurrency;
    varDate:      Result := ftDate;
    varOleStr:    Result := ftWideString;
    varBoolean:   Result := ftBoolean;
    varVariant:   Result := ftVariant;
    varUnknown:   Result := ftInterface;
    varDecimal:   Result := ftInteger;
    varByte:      Result := ftByte;
    varWord:      Result := ftWord;
    {$IFDEF D60}
    varLongWord:  Result := ftLongWord;
    varInt64:     Result := ftLargeInt;
    {$ELSE}
    varLongWord,
    varInt64:     Result := ftInteger;
    {$ENDIF}
    varString:    Result := ftString;
    else          Result := ftUnknown;
  end;
end;

class function Vart.IfEmpty(const Value, Default: Variant): Variant;
begin
  if IsOK(Value) then
    Result := Value else
    Result := Default;
end;

class function Vart.IfEmpty(const Value: Variant): Variant;
begin
  if IsOK(Value) then
    Result := Value else
    Result := Null;
end;

class function Vart.Coalesce(const V1: array of Variant; const Default: Variant): Variant;
var
  i: Integer;
begin
  result := Default;
  
  for i := Low(V1) to High(V1) do
    if IsOK(V1[i]) then
    begin
      result := V1[i];
      break;
    end;
end;

end.
