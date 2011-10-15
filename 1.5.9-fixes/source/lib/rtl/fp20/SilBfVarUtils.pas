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

unit SilBfVarUtils;

{$mode objfpc}

interface

uses
  Variants, SysUtils;

const
  CBool: array [Boolean] of AnsiString = ('False', 'True');
  CNoYes: array [Boolean] of AnsiString = ('No', 'Yes');

function VariantToInteger(const v: Variant): Integer;
function VariantToSmallint(const v: Variant): Smallint;
function VariantToShortint(const v: Variant): Shortint;
function VariantToWord(const v: Variant): Word;
function VariantToInt64(const v: variant): Int64;
function VariantToWord64(const v: variant): QWord;
function VariantToLongWord(const v: Variant): LongWord;
function VariantToByte(const v: Variant): Byte;
function VariantToBoolean(const v: Variant): Boolean;
function VariantToReal(const v: Variant): Extended;
function VariantToCurrency(const v: Variant): Currency;
function VariantToDouble(const v: Variant): Double;
function VariantToSingle(const v: Variant): Single;
function VariantToDate(const v: Variant): TDateTime;
procedure VariantToPStr(var s; const v: Variant);
procedure VariantToShortString(var s: ShortString; const v: Variant);
procedure VariantToAnsiStringData(var s: AnsiString; const v: TVarData);
procedure VariantToAnsiString(var s: AnsiString; const v: Variant);
procedure VariantToWideStringData(var s: WideString; const v: TVarData);
procedure VariantToWideString(var s: WideString; const v: Variant);
procedure VariantToInterface(var s: IInterface; const v: Variant);
procedure VariantToDispatch(var disp: IDispatch; const v: variant);
procedure VariantToDynArray(var dynarr: Pointer; const v: Variant; TypeInfo: Pointer);

procedure VariantFromBoolean(var dest: Variant; const source: Boolean);
procedure VariantFromInteger(var dest: Variant; const source, range: Longint);
procedure VariantFromInt64(var dest: Variant; const source: Int64);
procedure VariantFromWord64(var dest: Variant; const source: qword);
procedure VariantFromReal(var dest: Variant; const source: Extended);
procedure VariantFromSingle(var dest: Variant; const source: Single);
procedure VariantFromDate(var dest: Variant; const source: TDateTime);
procedure VariantFromCurrency(var dest: Variant; const source: Currency);
procedure VariantFromShortString(var dest: variant; const source: ShortString);
procedure VariantFromAnsiString(var dest: Variant; const source: AnsiString);
procedure VariantFromWideString(var dest: variant; const source: WideString);
procedure VariantFromInterface(var dest: Variant; const source: IInterface);
procedure VariantFromDispatch(var dest: Variant; const source: IDispatch);
procedure VariantFromDynArray(var dest: variant;const source: pointer; typeinfo: pointer);

procedure OleVarFromShortString(var dest: olevariant; const source: shortstring);
procedure OleVarFromAnsiString(var dest: olevariant; const source: ansistring);
procedure OleVarFromVariant(var dest: olevariant; const source: variant);
procedure OleVarFromInteger(var dest: olevariant; const source: longint;const range: shortint);

procedure VarOp(var left: Variant; const right: Variant; opcode: TVarOp);
function CmpOp(const left, right: Variant; const OpCode: TVarOp): Boolean;
procedure VarNeg(var v: Variant);
procedure VarNot(var v: Variant);
procedure VarInitData(var v: TVarData);
procedure VarInit(var v: Variant);
procedure VarClear(var v: Variant);
procedure VarClearData(var v: TVarData);
procedure VarAddRef(var v: Variant);
procedure VarAddRefData(var v: TVarData);
procedure VarCopy(var dest: variant; const source: variant);
procedure VarCopyData(var dest: TVarData; const source: TVarData);
procedure VarCast(var dest: variant; const source: variant; vartype: longint);
procedure VarCastOle(var dest: variant; const source: variant; vartype: longint);
procedure DispInvoke(dest: pvardata; const source: tvardata; calldesc: pcalldesc; params: pointer); cdecl;
procedure VarArrayRedim(var a: variant; highbound: SizeInt);
function VarArrayGet(const a: variant; indexcount: LongInt; indices: PSizeInt): variant; cdecl;
procedure VarArrayPut(var a: variant; const value: variant; indexcount: SizeInt; indices: PSizeInt); cdecl;
function WriteVariant(var t: system.text; const v: variant; width: longint): Pointer;
function Write0Variant(var t: system.text; const v: Variant): Pointer;

procedure VariantTypeMismatch(const Proc: AnsiString ='');
procedure NotImplemented(const proc: AnsiString);

implementation

uses
  SilBtStr,
  SilBtText,
  SilBtInt,
  SilBtFloat,
  SilBtDateTime;

procedure VariantTypeMismatch(const Proc: AnsiString);
begin
  writeln('type mismatch ' + proc);
  raise Exception.Create('type mismatch ' + proc);
end;

procedure NotImplemented(const proc: AnsiString);
begin
  writeln('not implemented: ' + proc);
  raise Exception.Create('not implemented: ' + proc);
end;

function GetStrAsBool(const Value: AnsiString): Boolean;
begin
  Result := Text.IsEqual(Value, CBool[true]) or Text.IsEqual(Value, CNoYes[true]) or (Value = '1');
end;

// var to

function VariantToInteger(const v: Variant): Integer;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('vartoint');
    end;
end;

function VariantToSmallint(const v: Variant): Smallint;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Smallint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('vartosmall');
    end;
end;

function VariantToShortint(const v: Variant): Shortint;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Shortint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('vartoshort');
    end;
end;

function VariantToWord(const v: Variant): Word;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Shortint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('vartoword');
    end;
end;

function VariantToInt64(const v: variant): Int64;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('varttoint64');
    end;
end;

function VariantToWord64(const v: variant): QWord;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('varttoword64');
    end;
end;

function VariantToLongWord(const v: Variant): LongWord;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('vartolongw');
    end;
end;

function VariantToByte(const v: Variant): Byte;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := Trunc(VSingle);
      varDouble:    Result := Trunc(VDouble);
      varCurrency:  Result := Trunc(VCurrency);
      varDate:      Result := Trunc(VDate);
      varOleStr:    Result := Str.ToInt(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Str.ToInt(AnsiString(VString));
      else          VariantTypeMismatch('varttobyte');
    end;
end;

function VariantToBoolean(const v: Variant): Boolean;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt <> 0;
      varInteger:   Result := VInteger <> 0;
      varSingle:    Result := VSingle <> 0;
      varDouble:    Result := VDouble <> 0;
      varCurrency:  Result := VCurrency <> 0;
      varDate:      Result := VDate <> 0;
      varOleStr:    Result := GetStrAsBool(WideCharToString(VOleStr));
      varBoolean:   Result := VBoolean;
      //varUnknown:   error
      varShortInt:  Result := VShortInt <> 0;
      varByte:      Result := VByte <> 0;
      varWord:      Result := VWord <> 0;
      varLongWord:  Result := VLongWord <> 0;
      varInt64:     Result := VInt64 <> 0;
      varQword:     Result := VQWord <> 0;
      varString:    Result := GetStrAsBool(AnsiString(VString));
      else          VariantTypeMismatch('varttobool');
    end;
end;

function VariantToReal(const v: Variant): Extended;
begin
  Result := VariantToDouble(v);
end;

function VariantToCurrency(const v: Variant): Currency;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := VSingle;
      varDouble:    Result := VDouble;
      varCurrency:  Result := VCurrency;
      varDate:      Result := VDate;
      varOleStr:    Result := Float.FromStr(WideCharToString(VOleStr), 0);
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Float.FromStr(AnsiString(VString));
      else          VariantTypeMismatch('varttocurr');
    end;
end;

function VariantToDouble(const v: Variant): Double;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := VSingle;
      varDouble:    Result := VDouble;
      varCurrency:  Result := VCurrency;
      varDate:      Result := VDate;
      varOleStr:    Result := Float.FromStr(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Float.FromStr(AnsiString(VString));
      else          VariantTypeMismatch('vartodouble');
    end;
end;

function VariantToSingle(const v: Variant): Single;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := VSingle;
      varDouble:    Result := VDouble;
      varCurrency:  Result := VCurrency;
      varDate:      Result := VDate;
      varOleStr:    Result := Float.FromStr(WideCharToString(VOleStr));
      varBoolean:   Result := Longint(VBoolean);
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := VLongWord;
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := Float.FromStr(AnsiString(VString));
      else          VariantTypeMismatch('vartosingle');
    end;
end;

function VariantToDate(const v: Variant): TDateTime;
begin
  with TVarData(v) do
    case (VType and VarTypeMask) of
      varSmallInt:  Result := VSmallInt;
      varInteger:   Result := VInteger;
      varSingle:    Result := VSingle;
      varDouble:    Result := VDouble;
      varCurrency:  Result := VCurrency;
      varDate:      Result := VDate;
      varOleStr:    Result := DateTime.FromStr(WideCharToString(VOleStr));
      //varBoolean:   NotImplemented('bool to date');
      //varUnknown:   error
      varShortInt:  Result := VShortInt;
      varByte:      Result := VByte;
      varWord:      Result := VWord;
      varLongWord:  Result := DateTime.FromDays(VLongWord);
      varInt64:     Result := VInt64;
      varQword:     Result := VQWord;
      varString:    Result := DateTime.FromStr(AnsiString(VString));
      else          VariantTypeMismatch('vartodate');
    end;
end;

procedure VariantToPStr(var s; const v: Variant);
var
  t: AnsiString;
begin
  VariantToAnsiString(t, v);
  ShortString(s) := t;
end;

procedure VariantToShortString(var s: ShortString; const v: Variant);
var
  a: AnsiString;
begin
  VariantToAnsiString(a, v);
  s := a;
end;

procedure VariantToAnsiString(var s: AnsiString; const v: Variant);
begin
  VariantToAnsiStringData(s, TVarData(v));
end;

procedure VariantToAnsiStringData(var s: AnsiString; const v: TVarData);
begin
  case v.VType of
    varSmallint:  s := Int.ToStr(v.VSmallint);
    varInteger:   s := Int.ToStr(v.VInteger);
    varSingle:    s := Float.ToStr(v.VSingle);
    varDouble:    s := Float.ToStr(v.VDouble);
    varCurrency:  s := Float.ToStr(v.VCurrency);
    varDate:      s := DateTime.ToStr(v.VDate);
    varOleStr:    s := WideString(Pointer(V.VOleStr));
    varBoolean:   s := CBool[v.VBoolean];
      //varUnknown:   error
    varShortint:  s := Int.ToStr(v.VShortint);
    varByte:      s := Int.ToStr(v.VByte);
    varWord:      s := Int.ToStr(v.VWord);
    varLongWord:  s := Int.ToStr(v.VLongWord);
    varInt64:     s := Int.ToStr(v.VInt64);
    varQWord:     s := Int.ToStr(v.VQWord);
    varString:    s := AnsiString(v.VString);
    else          VariantTypeMismatch('vartoansi');
  end;
end;

procedure VariantToWideString(var s: WideString; const v: Variant);
begin
  VariantToWideStringData(s, TVarData(v));
end;

procedure VariantToWideStringData(var s: WideString; const v: TVarData);
begin
  case v.VType of
    varSmallint:  s := Int.ToStr(v.VSmallint);
    varInteger:   s := Int.ToStr(v.VInteger);
    varSingle:    s := Float.ToStr(v.VSingle);
    varDouble:    s := Float.ToStr(v.VDouble);
    varCurrency:  s := Float.ToStr(v.VCurrency);
    varDate:      s := DateTime.ToStr(v.VDate);
    varOleStr:    s := WideString(Pointer(V.VOleStr));
    varBoolean:   s := CBool[v.VBoolean];
      //varUnknown:   error
    varShortint:  s := Int.ToStr(v.VShortint);
    varByte:      s := Int.ToStr(v.VByte);
    varWord:      s := Int.ToStr(v.VWord);
    varLongWord:  s := Int.ToStr(v.VLongWord);
    varInt64:     s := Int.ToStr(v.VInt64);
    varQWord:     s := Int.ToStr(v.VQWord);
    varString:    s := AnsiString(v.VString);
    else          VariantTypeMismatch('vartowide');
  end;
end;

procedure VariantToInterface(var s: IInterface; const v: Variant);
begin
  with TVarData(v) do
    case VType of
      //varSmallint:
      varInteger:   s := IUnknown(VInteger);
      //varSingle:
      //varDouble:
      //varCurrency:
      //varDate:
      //varOleStr:
      //varBoolean:
       varUnknown:   s := IUnknown(VUnknown);
      //varShortint:
      //varByte:
      //varWord:
      varLongWord:  s := PUnknown(VLongWord)^;
      //varInt64:
      //varQWord:
      //varString:
      else          VariantTypeMismatch('vartointf');
    end;
end;

procedure VariantToDispatch(var disp: IDispatch; const v: variant);
begin
  NotImplemented('VariantToDispatch');
end;

procedure VariantToDynArray(var dynarr: Pointer; const v: Variant; TypeInfo: Pointer);
begin
  NotImplemented('VariantToDynArray');
end;

procedure VariantFromBoolean(var dest: Variant; const source: Boolean);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  with TVarData(dest) do
  begin
    VType := varBoolean;
    VBoolean := source;
  end;
end;

procedure VariantFromInteger(var dest: Variant; const source, range: Longint);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  with TVarData(dest) do
    case range of
      -4:
        begin
          VType := varInteger;
          VInteger := source;
        end;
      -2:
        begin
          VType := varSmallInt;
          VSmallInt := source;
        end;
      -1:
        begin
          VType := varShortInt;
          VShortint := source;
        end;
      1:
        begin
          VType := varByte;
          VByte := source;
        end;
      2:
        begin
          VType := varWord;
          VWord := source;
        end;
      4:
        begin
          VType := varLongWord;
          VLongWord := source;
        end;
      else
        VariantTypeMismatch('VariantFromInteger');
        //VariantError(Format(SErrInvalidIntegerRange,[Range]));
    end;
end;

procedure VariantFromInt64(var dest: Variant; const source: Int64);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varInt64;
  TVarData(dest).VInt64 := source;
end;

procedure VariantFromWord64(var dest: Variant; const source: qword);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varQWord;
  TVarData(dest).VQWord := source;
end;

procedure VariantFromReal(var dest: Variant; const source: Extended);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varDouble;
  TVarData(dest).VDouble := source;
end;

procedure VariantFromSingle(var dest: Variant; const source: Single);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varSingle;
  TVarData(dest).VSingle := source;
end;

procedure VariantFromDate(var dest: Variant; const source: TDateTime);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varDate;
  TVarData(dest).VDate := source;
end;

procedure VariantFromCurrency(var dest: Variant; const source: Currency);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varCurrency;
  TVarData(dest).VCurrency := source;
end;

procedure VariantFromShortString(var dest: variant; const source: ShortString);
begin
  VariantFromAnsiString(dest, source);
end;

procedure VariantFromAnsiString(var dest: Variant; const source: AnsiString);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varString;
  AnsiString(TVarData(dest).VString) := source;
end;

procedure VariantFromWideString(var dest: variant; const source: WideString);
begin
  NotImplemented('VariantFromWideString');
end;

procedure VariantFromInterface(var dest: Variant; const source: IInterface);
begin
  if TVarData(dest).VType >= varOleStr then
    VarClear(dest);

  TVarData(dest).VType := varUnknown;
  IUnknown(TVarData(dest).VUnknown) := Source;
end;

procedure VariantFromDispatch(var dest: Variant; const source: IDispatch);
begin
  NotImplemented('VariantFromDispatch');
end;

procedure VariantFromDynArray(var dest: variant;const source: pointer; typeinfo: pointer);
begin
  NotImplemented('VariantFromDynArray');
end;

procedure OleVarFromShortString(var dest: olevariant; const source: shortstring);
begin
  NotImplemented('OleVarFromShortString');
end;

procedure OleVarFromAnsiString(var dest: olevariant; const source: ansistring);
begin
  NotImplemented('OleVarFromAnsiString');
end;

procedure OleVarFromVariant(var dest: olevariant; const source: variant);
begin
  NotImplemented('OleVarFromVariant');
end;

procedure OleVarFromInteger(var dest: olevariant; const source: longint;const range: shortint);
begin
  NotImplemented('OleVarFromInteger');
end;

{ operators }
procedure VarOp(var left: Variant; const right: Variant; opcode: TVarOp);
begin
  {opAdd =        0;
  opSubtract =   1;
  opMultiply =   2;
  opDivide =     3;
  opIntDivide =  4;
  opModulus =    5;
  opShiftLeft =  6;
  opShiftRight = 7;
  opAnd =        8;
  opOr =         9;
  opXor =        10;
  opCompare =    11;
  opNegate =     12;
  opNot =        13;}

  NotImplemented('VarOp');
end;

function CmpOp(const left, right: Variant; const OpCode: TVarOp): Boolean;
begin
  {opCmpEQ =      14;
  opCmpNE =      15;
  opCmpLT =      16;
  opCmpLE =      17;
  opCmpGT =      18;
  opCmpGE =      19;}

  NotImplemented('CmpOp');
end;

procedure VarNeg(var v: Variant);
begin
  NotImplemented('VarNeg');
end;

procedure VarNot(var v: Variant);
begin
  NotImplemented('VarNot');
end;

{ misc }
procedure VarInit(var v: Variant);
begin
  VarInitData(TVarData(v));
end;

procedure VarInitData(var v: TVarData);
begin
  FillChar(v.VBytes, SizeOf(v.VBytes), 0);
  v.VType := varEmpty;
end;

procedure VarClear(var v: Variant);
begin
  VarClearData(TVarData(v));
end;

procedure VarClearData(var v: TVarData);
begin
  // if VType and varArray <> 0 then not implemented

  case v.VType of
    varOleStr:    WideString(Pointer(V.VOleStr)) := '';
    varUnknown:   IUnknown(v.VUnknown) := nil;
    varString:    AnsiString(v.VString) := '';
  end;

  VarInitData(v);
end;

procedure VarAddRef(var v: Variant);
begin
  VarAddRefData(TVarData(v));
end;

procedure VarAddRefData(var v: TVarData);
var
  dummy : TVarData;
begin
  FillChar(dummy, SizeOf(dummy), 0);
  VarCopyData(dummy, v);
end;

procedure VarCopy(var dest: variant; const source: variant);
begin
  VarCopyData(TVarData(dest), TVarData(source));
end;

procedure VarCopyData(var dest: TVarData; const source: TVarData);
begin
  if @dest = @source then
    Exit;

  if source.VType and varArray <> 0 then
    NotImplemented('VarCopy array');

  // case (source.VType and varTypeMask) of
  case source.VType of
    varOleStr:
    begin
      VarClearData(dest);
      WideString(Pointer(dest.VOleStr)) := WideString(Pointer(source.VOleStr));
    end;

    varDispatch:
    begin
      VarClearData(dest);
      IUnknown(dest.VDispatch) := IUnknown(source.VDispatch);
    end;

    varUnknown:
    begin
      VarClearData(dest);
      IUnknown(dest.VUnknown):= IUnknown(source.VUnknown);
    end;

    varString:
    begin
      VarClearData(dest);
      AnsiString(dest.VString) := AnsiString(source.VString);
    end;
    else
      dest := source;
  end;

  dest.VType := source.VType;
end;

procedure VarCast(var dest: Variant; const source: Variant; VarType: Integer);
var
  s: AnsiString;
  w: WideString;
  i: IUnknown;
begin
  case Vartype of
    varSmallint:  VariantFromInteger(dest, VariantToSmallint(source), -2);
    varInteger:   VariantFromInteger(dest, VariantToInteger(source), -4);
    varSingle:    VariantFromSingle(dest, VariantToSingle(source));
    varDouble:    VariantFromReal(dest, VariantToDouble(source));
    varCurrency:  VariantFromCurrency(dest, VariantToCurrency(source));
    varDate:      VariantFromDate(dest, VariantToDate(source));
    varOleStr:
    begin
      VariantToWideString(w, source);
      VariantFromWideString(dest, w);
    end;
    varBoolean:   VariantFromBoolean(dest, VariantToBoolean(source));
    varUnknown:
    begin
      VariantToInterface(i, source);
      VariantFromInterface(dest, i);
    end;
    varShortint:  VariantFromInteger(dest, VariantToShortint(source), -1);
    varByte:      VariantFromInteger(dest, VariantToByte(source), 1);
    varWord:      VariantFromInteger(dest, VariantToWord(source), 2);
    varLongWord:  VariantFromInteger(dest, VariantToLongWord(source), 4);
    varInt64:     VariantFromInt64(dest, VariantToInt64(source));
    varQWord:     VariantFromWord64(dest, VariantToWord64(source));
    varString:
    begin
      VariantToAnsiString(s, source);
      VariantFromAnsiString(dest, s);
    end;
    else
      VariantTypeMismatch('varcast');
  end;
end;

procedure VarCastOle(var dest: variant; const source: variant; vartype: longint);
begin
  NotImplemented('VarCastOle');
end;

procedure DispInvoke(dest: pvardata; const source: tvardata; calldesc: pcalldesc; params: pointer); cdecl;
begin
  NotImplemented('DispInvoke');
end;

procedure VarArrayRedim(var a: variant; highbound: SizeInt);
begin
  NotImplemented('VarArrayRedim');
end;

function VarArrayGet(const a: variant; indexcount: LongInt; indices: PSizeInt): variant; cdecl;
begin
  NotImplemented('VarArrayGet');
end;

procedure VarArrayPut(var a: variant; const value: variant; indexcount: SizeInt; indices: PSizeInt); cdecl;
begin
  NotImplemented('VarArrayPut');
end;

type
  FileFunc = Procedure(var t: TextRec);

procedure fpc_WriteBuffer(var f: System.Text; const b; len: longint);
var
  p: pchar;
  left, idx: longint;
begin
  p := pchar(@b);
  idx := 0;
  left := TextRec(f).BufSize - TextRec(f).BufPos;

  while len > left do
  begin
    move(p[idx], TextRec(f).Bufptr^[TextRec(f).BufPos], left);
    dec(len, left);
    inc(idx, left);
    inc(TextRec(f).BufPos, left);
    FileFunc(TextRec(f).InOutFunc)(TextRec(f));
    left := TextRec(f).BufSize - TextRec(f).BufPos;
  end;

  move(p[idx], TextRec(f).Bufptr^[TextRec(f).BufPos], len);
  inc(TextRec(f).BufPos, len);
end;

procedure fpc_WriteBlanks(var f: System.Text; len: longint);
var
  left: longint;
begin
  left := TextRec(f).BufSize - TextRec(f).BufPos;

  while len > left do
  begin
    FillChar(TextRec(f).Bufptr^[TextRec(f).BufPos], left, ' ');
    dec(len, left);
    inc(TextRec(f).BufPos, left);
    FileFunc(TextRec(f).InOutFunc)(TextRec(f));
    left := TextRec(f).BufSize - TextRec(f).BufPos;
  end;

  FillChar(TextRec(f).Bufptr^[TextRec(f).BufPos], len, ' ');
  inc(TextRec(f).BufPos, len);
end;

procedure fpc_Write_Text_AnsiStr(Len: Longint; var f: System.Text; S: AnsiString);
var
  SLen: longint;
begin
  If (InOutRes <> 0) then
   exit;

  case TextRec(f).mode of
    fmOutput { fmAppend gets changed to fmOutPut in do_open (JM) }:
    begin
      SLen := Length(s);
      if Len > SLen then
        fpc_WriteBlanks(f, Len - SLen);
      if slen > 0 then
        fpc_WriteBuffer(f, PChar(S)^, SLen);
    end;

    fmInput:
      InOutRes := 105

    else
      InOutRes := 103;
  end;
end;

function WriteVariant(var t: system.text; const v: variant; width: longint): Pointer;
var
  buf: AnsiString;
begin
  VariantToAnsiString(buf, v);
  fpc_write_text_ansistr(width, t, buf);
  result := nil;
end;

function Write0Variant(var t: system.text; const v: Variant): Pointer;
var
  buf: AnsiString;
begin
  VariantToAnsiString(buf, v);
  fpc_write_text_ansistr(-1, t, buf);
  result := nil;
end;

// variant manager

var
  OldMgr: TVariantManager;

procedure InitVarMgr;
var
  mgr: TVariantManager;
begin
  mgr.vartoint        := @VariantToInteger;
  mgr.vartoint64      := @VariantToInt64;
  mgr.vartoword64     := @VariantToWord64;
  mgr.vartobool       := @VariantToBoolean;
  mgr.vartoreal       := @VariantToReal;
  mgr.vartocurr       := @VariantToCurrency;
  mgr.vartopstr       := @VariantToPStr;
  mgr.vartolstr       := @VariantToAnsiString;
  mgr.vartowstr       := @VariantToWideString;
  mgr.vartointf       := @VariantToInterface;
  mgr.vartodisp       := @VariantToDispatch;
  mgr.vartodynarray   := @VariantToDynArray;
  mgr.varfrombool     := @VariantFromBoolean;
  mgr.varfromint      := @VariantFromInteger;
  mgr.varfromint64    := @VariantFromInt64;
  mgr.varfromword64   := @VariantFromWord64;
  mgr.varfromreal     := @VariantFromReal;
  mgr.varfrompstr     := @VariantFromShortString;
  mgr.varfromlstr     := @VariantFromAnsiString;
  mgr.varfromwstr     := @VariantFromWideString;
  mgr.varfromintf     := @VariantFromInterface;
  mgr.varfromdisp     := @VariantFromDispatch;
  mgr.varfromdynarray := @VariantFromDynArray;
  mgr.olevarfrompstr  := @OleVarFromShortString;
  mgr.olevarfromlstr  := @OleVarFromAnsiString;
  mgr.olevarfromvar   := @OleVarFromVariant;
  mgr.olevarfromint   := @OleVarFromInteger;
  mgr.varop           := @VarOp;
  mgr.cmpop           := @CmpOp;
  mgr.varneg          := @VarNeg;
  mgr.varnot          := @VarNot;
  mgr.varinit         := @VarInit;
  mgr.varclear        := @VarClear;
  mgr.varaddref       := @VarAddRef;
  mgr.varcopy         := @VarCopy;
  mgr.varcast         := @VarCast;
  mgr.varcastole      := @VarCastOle;
  mgr.dispinvoke      := @DispInvoke;
  mgr.vararrayredim   := @VarArrayRedim;
  mgr.vararrayget     := @VarArrayGet;
  mgr.vararrayput     := @VarArrayPut;
  mgr.writevariant    := @WriteVariant;
  mgr.write0Variant   := @Write0Variant;

  GetVariantManager(OldMgr);
  SetVariantManager(mgr);

  Pointer(VarClearProc) := @VarClearData;    // procedure(var v : TVarData) = nil;
  Pointer(VarAddRefProc) := @VarAddRefData;  // procedure(var v : TVarData) = nil;
  Pointer(VarCopyProc) := @VarCopyData;      // procedure(var d : TVarData;const s : TVarData) = nil;
  Pointer(VarToLStrProc) := @VariantToAnsiStringData;  // procedure(var d : AnsiString;const s : TVarData) = nil;
  Pointer(VarToWStrProc) := @VariantToWideStringData;  // procedure(var d : WideString;const s : TVarData) = nil;
end;

procedure FinalizeVarMgr;
begin
  SetVariantManager(OldMgr);
end;

initialization
  InitVarMgr;

Finalization
  FinalizeVarMgr;

end.