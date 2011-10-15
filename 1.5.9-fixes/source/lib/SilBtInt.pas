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

unit SilBtInt;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool;

type
  Int = class(Tool)
    class function Max(const i1, i2: Integer): Integer;
    class function Min(const i1, i2: Integer): Integer;
    class function Abs(const i1: Integer): Integer;
    class function AbsSign(const i1: Integer; var sign: Integer): Integer;
    class function Range(i, b, e: Integer): Integer;
    class function Between(Value, Low, High: Integer): Boolean;
    class function Sign(const Number: Integer): Integer;
    class procedure DivMod(const Dividend: Integer; const Divisor: Word; var Result, Remainder: Word);
    class function ToStr(const i: Integer): String;
    class function IIf(const Expr: Boolean; const RTrue: Integer; const RFalse: Integer {$IFDEF USE_DEFPARAMS} = 0 {$ENDIF}): Integer;
    class function ToHex(Number, Digits: Integer): String;
    class function ToBin(Number, Len: Cardinal): String;
    class function IfEmpty(Value, Default: Integer): Integer; 
    class procedure Swap(var i1, i2: Integer);
    class function PadL(Number: Integer; const Len: Integer; const Character: Char = ' '): String;
    class function PadR(Number: Integer; const Len: Integer; const Character: Char = ' '): String;
    class function Inc(var Number: Integer; Preinc: Boolean = False): Integer; overload;
    class function Dec(var Number: Integer; Preinc: Boolean = False): Integer; overload;
    class function Inc(var Number: LongWord; Preinc: Boolean = False): Integer; overload;
    class function Dec(var Number: LongWord; Preinc: Boolean = False): Integer; overload;
    class function Inc(var Number: Integer; Delta: Integer; Preinc: Boolean = False): Integer; overload;
    class function Dec(var Number: Integer; Delta: Integer; Preinc: Boolean = False): Integer; overload;
    class function Inc(var Number: LongWord; Delta: Integer; Preinc: Boolean = False): Integer; overload;
    class function Dec(var Number: LongWord; Delta: Integer; Preinc: Boolean = False): Integer; overload;
    class function Align(Number: Integer; Base: Word = SizeOf(Integer)): Integer; overload;
    class function Align(Number: PInteger; Base: Word = SizeOf(Integer)): Integer; overload;
    class function Compare(const Value1, Value2: Integer): Integer; overload; 
    class function Compare(const Value1, Value2: LongWord): Integer; overload; 
{$IFDEF USE_DYNARRAYS}
    class function ToArray(const Values: array of Integer): TIntegerArray;
    class function ArrayAdd(var A: TIntegerArray; Value: Integer): Integer;
    class function ArrayFind(var A: TIntegerArray; Value: Integer): Integer;
    class function ArrayDelete(var A: TIntegerArray; Index: Integer): Boolean;
    class function ArrayCompare(const A, B: TIntegerArray): Integer;
    class function ArrayCombine(var A: TIntegerArray): Boolean;
{$ENDIF}
  end;

implementation

uses
  SysUtils,
  SilBtStr,
  SilAfInt;

class function Int.Abs(const i1: Integer): Integer;
begin
  if i1 < 0 then
    result := -i1 else
    result := i1;
end;

class function Int.AbsSign(const i1: Integer; var sign: Integer): Integer;
begin
  if i1 < 0 then
  begin
    result := -i1;
    sign := -1;
  end
  else
  begin
    result := i1;
    sign := 1;
  end;
end;

class procedure Int.DivMod(const Dividend: Integer; const Divisor: Word; var Result, Remainder: Word);
asm
    PUSH    EBX
    MOVZX   EBX,Divisor {EDX}
    MOV     EDX,Dividend{EAX}
    SHR     EDX,16
    DIV     BX
    MOV     EBX,Remainder
    MOV     ECX, Result
    MOV     [ECX],AX
    MOV     [EBX],DX
    POP     EBX
end;

class function Int.Max(const i1, i2: Integer): Integer;
begin
  if i1 < i2 then
    Result := i2 else
    Result := i1;
end;

class function Int.Min(const i1, i2: Integer): Integer;
begin
  if i1 > i2 then
    Result := i2 else
    Result := i1;
end;

class function Int.Range(i, b, e: Integer): Integer;
begin
  if i < b then Result := b else
  if i > e then Result := e else Result := i;
end;

class function Int.Between(Value, Low, High: Integer): Boolean;
begin
  Result := (Low <= Value) and (Value <= High);
end;

class function Int.Sign(const Number: Integer): Integer;
begin
  if Number >= 0 then
    Result :=  1 else
    Result := -1;
end;

class function Int.ToStr(const i: Integer): String;
begin
  System.Str(i, Result);
end;

class function Int.IIf(const Expr: Boolean; const RTrue, RFalse: Integer): Integer;
begin
  if Expr then
    Result := RTrue else
    Result := RFalse;
end;

class function Int.ToHex(Number, Digits: Integer): String;
const
  HexDigits: PChar = '0123456789ABCDEF';
begin
  if Odd(Digits) then System.Inc(Digits);
  SetLength(Result, Digits);

    while Digits > 0 do
    begin
      Result[Digits - 1] := HexDigits[(Number and $ff) shr 4];
      Result[Digits] := HexDigits[(Number and $ff) and 15];
      Number := Number shr 8;
      System.Dec(Digits, 2);
  end;
end;

class function Int.ToBin(Number, Len: Cardinal): String;
begin
  Result := Str.ReplicateChar('0', Len);

  while (Number <> 0) and (Len > 0) do
  begin
    Result[Len] := Char(Ord(Number mod 2 = 1) + 48);
    Number := Number shr 1;
    System.Dec(Len);
  end;
end;

{$IFDEF USE_DYNARRAYS}
class function Int.ToArray(const Values: array of Integer): TIntegerArray;
var
  i: LongWord;
begin
  SetLength(Result, Length(Values));
  for i := 0 to High(Values) do Result[i] := Values[i];
end;
{$ENDIF}

class function Int.ArrayAdd(var A: TIntegerArray; Value: Integer): Integer;
begin
  Result := Length(A) + 1;
  SetLength(A, Result);
  A[Result - 1] := Value;
end;

class function Int.ArrayDelete(var A: TIntegerArray; Index: Integer): Boolean;
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

class function Int.ArrayFind(var A: TIntegerArray; Value: Integer): Integer;
begin
  for Result := 0 to High(A) do if A[Result] = Value then Exit;
  Result := -1;
end;

class procedure Int.Swap(var i1, i2: Integer);
var
  i: Integer;
begin
  i := i1;
  i1 := i2;
  i2 := i;
end;

class function Int.PadL(Number: Integer; const Len: Integer; const Character: Char): String;
begin
  Result := Str.PadL(ToStr(Number), Len, Character);
end;

class function Int.PadR(Number: Integer; const Len: Integer; const Character: Char): String;
begin
  Result := Str.PadR(ToStr(Number), Len, Character);
end;

{$WARNINGS OFF} // Apago los warnings porque Delphi no "entiende" que el Result abajo está bien definido
class function Int.Inc(var Number: Integer; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Inc(Number);
  if not Preinc then Result := Number;
end;

class function Int.Inc(var Number: LongWord; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Inc(Number);
  if not Preinc then Result := Number;
end;

class function Int.Dec(var Number: Integer; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Dec(Number);
  if not Preinc then Result := Number;
end;

class function Int.Dec(var Number: LongWord; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Dec(Number);
  if not Preinc then Result := Number;
end;

class function Int.Inc(var Number: Integer; Delta: Integer; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Inc(Number, Delta);
  if not Preinc then Result := Number;
end;

class function Int.Dec(var Number: Integer; Delta: Integer; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Dec(Number, Delta);
  if not Preinc then Result := Number;
end;

class function Int.Inc(var Number: LongWord; Delta: Integer; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Inc(Number, Delta);
  if not Preinc then Result := Number;
end;

class function Int.Dec(var Number: LongWord; Delta: Integer; Preinc: Boolean): Integer;
begin
  if Preinc then Result := Number;
  System.Dec(Number, Delta);
  if not Preinc then Result := Number;
end;
{$WARNINGS ON} // Fin de la zona de warnings incorrectos

class function Int.Align(Number: Integer; Base: Word): Integer;
begin
  Result := SilAfInt.DoAlign(@Number, Base);
end;

class function Int.Align(Number: PInteger; Base: Word): Integer;
begin
  Result := SilAfInt.DoAlign(Number, Base);
end;

class function Int.Compare(const Value1, Value2: Integer): Integer;
begin
  Result := (Value1 - Value2);
end;

class function Int.Compare(const Value1, Value2: LongWord): Integer;
begin
  if Value1 > Value2 then
    Result := Integer(Value1 - Value2) else
    Result := -Integer(Value2 - Value1);
end;

class function Int.ArrayCompare(const A, B: TIntegerArray): Integer;
var
  i, len: Integer;
begin
  len := Int.Min(High(A), High(B));
  Result := 0;

  for i := 0 to len do
  begin
    Result := A[i] - B[i];
    if Result <> 0 then Break;
  end;
end;

class function Int.ArrayCombine(var A: TIntegerArray): Boolean;
var
  cnt: integer;

  function PosLowerGT(APos: integer; AGT: Integer): integer;
  var
    i1: integer;
  begin
    result := -1;

    for i1 := cnt downto APos do
      if ((result = -1) or (A[i1] < A[result])) and (A[i1] > AGT) then
        result := i1;
  end;

  function PosLower(APos: integer): integer;
  var
    i1: integer;
  begin
    result := -1;

    for i1 := cnt downto APos do
      if ((result = -1) or (A[i1] < A[result])) then
        result := i1;
  end;

  procedure Swap(APos1, APos2: integer);
  var
    c1: Integer;
  begin
    if (APos1 <> APos2) then
    begin
      c1 := A[APos1];
      A[APos1] := A[APos2];
      A[APos2] := c1;
    end;
  end;

var
  i1, i2: integer;
begin
  cnt := Length(A) - 1;
  result := false;

  for i1 := cnt downto 1 do
    if (A[i1 - 1] < A[i1]) then
    begin
      Swap(i1 - 1, PosLowerGT(i1, A[i1 - 1]));

      for i2 := i1 to cnt - 1 do
        Swap(i2, PosLower(i2));

      result := true;
      break;
    end;
end;

class function Int.IfEmpty(Value, Default: Integer): Integer;
begin
  if value = 0 then
    Result := Default
  else
    Result := value;
end;

end.
