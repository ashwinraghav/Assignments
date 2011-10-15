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

unit SilBtVArray;

{$I Defines.inc}

interface

uses
  SysUtils,
  SilBeTypes,
  SilBtVart,
  SilBkTool; 

type
  VArray = class(Vart)
    class function Create(const Bounds: array of Integer; VarType: Integer): Variant;
    class function Build(const Values: array of Variant): Variant;
    class function ToStr(const V, Default: Variant; const Separator: string = ''): string;
    class function ToWideStr(const V, Default: Variant; const Separator: WideString = ''): WideString;
    class function Dims(const V: Variant): Integer;
    class function High(const V: Variant; Dim: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF}): Integer;
    class function Low(const V: Variant; Dim: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF}): Integer;
    class function Count(const V: Variant; Dim: Integer {$IFDEF USE_DEFPARAMS} = 1 {$ENDIF}): Integer;
    class function Lock(const V: Variant): Pointer;
    class procedure Unlock(const V: Variant);
    class procedure Clear(var V: Variant; const Value: Variant);

    class procedure Assign(var V: Variant; const Values: array of Variant); overload;
    class procedure Assign(var V: Variant; const Value: Variant); overload;
    
    class function ToByteArray(const V: Variant): TByteArray;
    class function ToWordArray(const V: Variant): TWordArray;
    class function ToIntegerArray(const V: Variant): TIntegerArray;
    class function ToLongArray(const V: Variant): TLongArray;
    class function ToVariantArray(const V: Variant): TVariantArray;
  end;

implementation

uses
  SilBfVariants,
  SilBtInt,
  SilBtStr,
  SilOtTool;

class function VArray.Create(const Bounds: array of Integer; VarType: Integer): Variant;
begin
  Result := VarArrayCreate(Bounds, VarType);
end;

class function VArray.Build(const Values: array of Variant): Variant;
begin
  Result := VarArrayOf(Values);
end;

class function VArray.Count(const V: Variant; Dim: Integer): Integer;
begin
  Result := High(V, Dim) - Low(V, Dim) + 1;
end;

class function VArray.High(const V: Variant; Dim: Integer): Integer;
begin
  Result := VarArrayHighBound(V, Dim);
end;

class function VArray.Low(const V: Variant; Dim: Integer): Integer;
begin
  Result := VarArrayLowBound(V, Dim);
end;

class function VArray.Dims(const V: Variant): Integer;
begin
  Result := VarArrayDimCount(V);
end;

class function VArray.ToStr(const V, Default: Variant; const Separator: string): string;
var
  S: string;
  I: Integer;
begin
  if IsArray(V) then
    begin
      if Str.IsEmpty(Separator) then
        S := SysUtils.ListSeparator else
        S := Separator;
      Result := '';
      for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
      begin
        if Str.NotEmpty(Result) then Result := Result + S;
        Result := Result + Value(V[I], Default, Default);
      end;
    end
  else
      Result := Value(V, Default, Default);
end;

class function VArray.ToWideStr(const V, Default: Variant; const Separator: WideString): WideString;
var
  S: WideString;
  I: Integer;
begin
  if IsArray(V) then
    begin
      if Wstr.IsEmpty(Separator) then
        S := SysUtils.ListSeparator else
        S := Separator;
      Result := '';
      for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
      begin
        if Wstr.NotEmpty(Result) then Result := Result + S;
        Result := Result + inherited ToWideStr(V[I], Default);
      end;
    end
  else
      Result := Value(V, Default, Default);
end;

class function VArray.Lock(const V: Variant): Pointer;
begin
  Result := VarArrayLock(V);
end;

class procedure VArray.Unlock(const V: Variant);
begin
  VarArrayUnlock(V);
end;

class procedure VArray.Clear(var V: Variant; const Value: Variant);
begin
  Assign(V, Value);
end;

class procedure VArray.Assign(var V: Variant; const Values: array of Variant);
var
  I, L, N, H: Integer;
begin
  if IsArray(V) then
    begin
      L := VarArrayLowBound(V, 1);
      N := System.Low(Values);
      H := Int.Min(VarArrayHighBound(V, 1), System.High(Values));
      for I := N to H do
        V[I + L - N] := Values[I]
    end
  else
    V := VarArrayOf(Values);
end;

class procedure VArray.Assign(var V: Variant; const Value: Variant);
var
  I: Integer;
begin
  if IsArray(V) then
    begin
      for I := VarArrayLowBound(V, 1) to VarArrayHighBound(V, 1) do
        V[I] := Value;
    end;
end;

class function VArray.ToByteArray(const V: Variant): TByteArray;
var
  I: Integer;
  L: Integer;
begin
  if IsArray(V) then
    begin
      SetLength(Result, Count(V, 1));
      L := VarArrayLowBound(V, 1);
      for I := L to VarArrayHighBound(V, 1) do
        Result[I - L] := V[I];
    end
  else
    Result := inherited ToByteArray(V);
end;

class function VArray.ToIntegerArray(const V: Variant): TIntegerArray;
var
  I: Integer;
  L: Integer;
begin
  if IsArray(V) then
    begin
      SetLength(Result, Count(V, 1));
      L := VarArrayLowBound(V, 1);
      for I := L to VarArrayHighBound(V, 1) do
        Result[I - L] := V[I];
    end
  else
    Result := inherited ToIntegerArray(V);
end;

class function VArray.ToLongArray(const V: Variant): TLongArray;
var
  I: Integer;
  L: Integer;
begin
  if IsArray(V) then
    begin
      SetLength(Result, Count(V, 1));
      L := VarArrayLowBound(V, 1);
      for I := L to VarArrayHighBound(V, 1) do
        Result[I - L] := V[I];
    end
  else
    Result := inherited ToLongArray(V);
end;

class function VArray.ToWordArray(const V: Variant): TWordArray;
var
  I: Integer;
  L: Integer;
begin
  if IsArray(V) then
    begin
      SetLength(Result, Count(V, 1));
      L := VarArrayLowBound(V, 1);
      for I := L to VarArrayHighBound(V, 1) do
        Result[I - L] := V[I];
    end
  else
    Result := inherited ToWordArray(V);
end;

class function VArray.ToVariantArray(const V: Variant): TVariantArray;
var
  I: Integer;
  L: Integer;
begin
  if IsArray(V) then
    begin
      SetLength(Result, Count(V, 1));
      L := VarArrayLowBound(V, 1);
      for I := L to VarArrayHighBound(V, 1) do
        Result[I - L] := V[I];
    end
  else
      SetLength(Result, 0);
end;

end.
