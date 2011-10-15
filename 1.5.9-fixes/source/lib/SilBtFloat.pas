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

unit SilBtFloat;

{$I Defines.inc}

interface

uses
  SilBkTool,
  SilBeTypes;

type
  Float = class(Tool)
    class function Max(const f1, f2: Double): Double;
    class function Min(const f1, f2: Double): Double;
    class function Abs(const f: Double): Double;
    class function AbsSign(const f: Double; var sign: Integer): Double;
    class function Between(Value, Low, High: Double): Boolean;
    class function ToStr(const f: Double; const prec, dec: Integer; const DecimalSeparator: Char = #0): String; {$IFDEF USE_OVERLOAD} overload; {$ENDIF}
    {$IFDEF USE_OVERLOAD}
    class function ToStr(const f: Double; const DecimalSeparator: Char = #0): String; overload;
    {$ENDIF}
    class function Fmt(const f: Double; const Format: string): String;
    class function IIf(const Expr: Boolean; const RTrue, RFalse: Double): Double;
    class function IsValid(const Buffer: string): Boolean;
    class function FromStr(const Buffer: string): Double; overload;
    class function FromStr(const Buffer: string; Def: Double): Double; overload;
    class function Trunc(const f: double): integer;
    class function Decimal(const f: double): double;
    {$IFDEF USE_DYNARRAYS}
    class function ArrayAdd(var A: TFloatArray; Value: Double): Integer;
    class function ArrayFind(var A: TFloatArray; Value: Double): Integer;
    class function ArrayDelete(var A: TFloatArray; Index: Integer): Boolean;
    {$ENDIF}
  end;

implementation

uses
  SysUtils,
  SysConst,
  SilBtStr;

class function Float.Max(const f1, f2: Double): Double;
begin
  if f1 > f2
    then Result := f1
    else Result := f2;
end;

class function Float.Min(const f1, f2: Double): Double;
begin
  if f1 < f2
    then Result := f1
    else Result := f2;
end;

class function Float.Abs(const f: Double): Double;
begin
  if f < 0 then
    Result := -f else
    Result := f;
end;

class function Float.AbsSign(const f: Double; var sign: Integer): Double;
begin
  if f < 0 then
    begin
      result := -f;
      sign := -1;
    end
  else
    begin
      result := f;
      sign := 1;
    end;
end;

class function Float.Between(Value, Low, High: Double): Boolean;
begin
  Result := (Low <= Value) and (Value <= High);
end;

class function Float.ToStr(const f: Double; const prec, dec: Integer; const DecimalSeparator: Char): String;
var
  SavedSeparator: Char;
begin
  SavedSeparator := SysUtils.DecimalSeparator;
  try
    if DecimalSeparator <> #0 then
      SysUtils.DecimalSeparator := DecimalSeparator;
    Result := FloatToStrF(f, ffFixed  , prec, dec);
  finally
    if DecimalSeparator <> #0 then
      SysUtils.DecimalSeparator := SavedSeparator;
  end;
end;

{$IFDEF USE_OVERLOAD}

class function Float.ToStr(const f: Double; const DecimalSeparator: Char): String;
var
  SavedSeparator: Char;
begin
  SavedSeparator := SysUtils.DecimalSeparator;
  try
    if DecimalSeparator <> #0 then
      SysUtils.DecimalSeparator := DecimalSeparator;
    Result := SysUtils.FloatToStr(f);
  finally
    if DecimalSeparator <> #0 then
      SysUtils.DecimalSeparator := SavedSeparator;
  end;
end;

{$ENDIF}

class function Float.Fmt(const f: Double; const Format: string): String;
begin
  Result := FormatFloat(Format, f);
end;

class function Float.IIf(const Expr: Boolean; const RTrue, RFalse: Double): Double;
begin
  if Expr then
    Result := RTrue else
    Result := RFalse;
end;

class function Float.FromStr(const Buffer: string): Double;
var
  E: Integer;
  Buf: string;
begin
  Buf := Buffer;
  Str.Replace(Buf, DecimalSeparator, '.');
  Val(Buf, Result, E);
  if E <> 0 then EConvertError.CreateResFmt(@SInvalidFloat, [Buffer]);
end;

class function Float.FromStr(const Buffer: string; Def: Double): Double;
var
  E: Integer;
  Buf: string;
begin
  Buf := Buffer;
  Str.Replace(Buf, DecimalSeparator, '.');
  Val(Buf, Result, E);
  if E <> 0 then Result := Def;
end;

{$HINTS OFF}
class function Float.IsValid(const Buffer: string): Boolean;
var
  E: Integer;
  Value: double;
  Buf: string;
begin
  Buf := Buffer;
  Str.Replace(Buf, DecimalSeparator, '.');
  Val(Buf, Value, E);
  Result := ( E = 0 );
end;
{$HINTS ON}

class function Float.Decimal(const f: double): double;
begin
  Result:= f - Trunc(f);
end;

class function Float.Trunc(const f: double): integer;
begin
  Result:= System.Trunc(f);
end;

class function Float.ArrayAdd(var A: TFloatArray; Value: Double): Integer;
begin
  Result := Length(A) + 1;
  SetLength(A, Result);
  A[Result - 1] := Value;
end;

class function Float.ArrayDelete(var A: TFloatArray; Index: Integer): Boolean;
var
  i: Integer;
begin
  Result := (Index >= 0) and (Index <= High(A));

  if Result then
  begin
    for i := Index to High(A) - 1 do A[i] := A[i + 1];
    SetLength(A, Length(A) - 1);
  end;
end;

class function Float.ArrayFind(var A: TFloatArray; Value: Double): Integer;
begin
  for Result := 0 to High(A) do if A[Result] = Value then Exit;
  Result := -1;
end;


end.

