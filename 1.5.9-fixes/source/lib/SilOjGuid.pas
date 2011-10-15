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

unit SilOjGuid;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool;

type
  SilGuid = class(Tool)
    class function Create: TGUID; virtual; abstract;
    class function Null: TGUID; virtual; abstract;
    class function ToStr(const Value: TGUID): String; virtual; abstract;
    class function FromStr(const Value: String): TGUID; virtual; abstract;
    class function IsEqual(const Value1, Value2: TGUID): Boolean; overload; virtual; abstract;
    class function IsEmpty(const Value: TGUID): Boolean;
    class function Iif(Condition: Boolean; const IfTrue, IfFalse: TGUID): TGUID;
    class function NotEmpty(const Value: TGUID): Boolean;
    class function Compare(const Value1, Value2: TGUID): Integer;
    class function Build(D1: LongWord): TGUID; overload;
    class function Build(D1: LongWord; D2: Word; D3: Word = 0): TGUID; overload;
    class function Build(D1: LongWord; D2: Word; D3: Word; const D4: LargeInt): TGUID; overload;
    class function Index(const Value: TGUID; const List: array of TGUID): Integer; overload;
    class function IsEqual(const Value: TGUID; const List: array of TGUID): Boolean; overload;
    class function ArrayAdd(var A: TGuidArray; const Value: TGuid): Integer;
    class function ArrayFind(const A: TGuidArray; const Value: TGuid): Integer;
    class function ArrayDelete(var A: TGuidArray; Index: Integer): Boolean;
    class function ArrayLast(const A: TGuidArray): TGuid;
    class procedure ArraySet(var A: TGuidArray; const Values: array of TGuid);
  end;

implementation

uses
  SilBtMem,
  SilBtLarge;

{ Guid }

class function SilGuid.IsEmpty(const Value: TGUID): Boolean;
begin
  Result := IsEqual(Value, Null);
end;

class function SilGuid.Iif(Condition: Boolean; const IfTrue, IfFalse: TGUID): TGUID;
begin
  if Condition then
    Result := IfTrue else
    Result := IfFalse;
end;

class function SilGuid.NotEmpty(const Value: TGUID): Boolean;
begin
  Result := not IsEqual(Value, Null);
end;

class function SilGuid.Compare(const Value1, Value2: TGUID): Integer;
begin
  Result := Large.Compare(Value1.D1, Value2.D1);
  if Result = 0 then
  begin
    Result := Large.Compare(Value1.D2, Value2.D2);
    if Result = 0 then
    begin
      Result := Large.Compare(Value1.D3, Value2.D3);
      if Result = 0 then
        Result := Large.Compare(PLargeint(@Value1.D4)^, PLargeint(@Value2.D4)^);
    end;
  end;
end;

class function SilGuid.Build(D1: LongWord): TGUID;
begin
  Result.D1 := D1;
  Result.D2 := 0;
  Result.D3 := 0;
  LargeInt(Result.D4) := 0;
end;

class function SilGuid.Build(D1: LongWord; D2, D3: Word): TGUID;
begin
  Result.D1 := D1;
  Result.D2 := D2;
  Result.D3 := D3;
  LargeInt(Result.D4) := 0;
end;

class function SilGuid.Build(D1: LongWord; D2, D3: Word; const D4: LargeInt): TGUID;
begin
  Result.D1 := D1;
  Result.D2 := D2;
  Result.D3 := D3;
  LargeInt(Result.D4) := D4;
end;

class function SilGuid.Index(const Value: TGUID; const List: array of TGUID): Integer;
begin
  for Result := Low(List) to High(List) do if IsEqual(List[Result], Value) then Exit;
  Result := -1;
end;

class function SilGuid.IsEqual(const Value: TGUID; const List: array of TGUID): Boolean;
begin
  Result := Index(Value, List) <> -1;
end;

class function SilGuid.ArrayAdd(var A: TGuidArray; const Value: TGuid): Integer;
begin
  Result := Length(A) + 1;
  SetLength(A, Result);
  A[Result - 1] := Value;
end;

class function SilGuid.ArrayFind(const A: TGuidArray; const Value: TGuid): Integer;
begin
  for Result := 0 to High(A) do if IsEqual(A[Result], Value) then Exit;
  Result := -1;
end;

class function SilGuid.ArrayDelete(var A: TGuidArray; Index: Integer): Boolean;
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

class function SilGuid.ArrayLast(const A: TGuidArray): TGuid;
var
  i: Integer;
begin
  i := Length(A);

  if i > 0 then
    Result := A[i - 1]
  else
    Result := SilGuid.Null;
end;

class procedure SilGuid.ArraySet(var A: TGuidArray; const Values: array of TGuid);
var
  i: Integer;
begin
  i := Length(Values);
  SetLength(A, i);

  while i > 0 do
  begin
    A[i - 1] := Values[i - 1];
    Dec(i);
  end;
end;

end.
 