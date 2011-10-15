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

unit SilVectorSkVector;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector;

const
  MaxListSize = High(Integer) div SizeOf(Pointer);

type
  PListData = ^TListData;
  TListData = array[0..MaxListSize - 1] of Pointer;

  ESilVectorError = class(Exception);

  TSilVector = class (TSilObject, IVector, IVectorComparator, IVectorComparable)
  protected
    FCount: Integer;
    FCapacity: Integer;
    FList: PListData;
    FLock: ICriticalSection;
    // tiene sentido tener flags para evitar un delete mientras se enumera?
  protected
    procedure Grow;
    procedure SetCapacity(NewCapacity: Integer);
    procedure IndexError(Index: Integer);
    procedure ItemInsert(Index: Integer);
    function ItemAdd: integer;
    procedure DisposeItem(Index: Integer); virtual;
  protected // IVector
    function GetCount: integer;
    procedure SetCount(NewCount: integer);
    procedure Clear;
    procedure Delete(Index: integer);
    procedure Exchange(Index1, Index2: integer);
    procedure Sort(const Comparator: IVectorComparator);
    function Search(const Value; const Comparable: IVectorComparable): integer;
    procedure Lock;
    procedure Unlock;
  protected // IVectorComparator
    function CompareItems(const Vector: IVector; index1, index2: integer): integer;
  protected // IVectorComparable
    function CompareValue(const Vector: IVector; index: integer; const value): integer;
  public
    constructor Create(locked: boolean);
    destructor Destroy; override;
  end;

implementation

uses
  SilLdBaseList;

{ TSilVector }

constructor TSilVector.Create(locked: boolean);
begin
  inherited Create;
  if locked then
    FLock := Sil.OS.Ipc.CriticalSection;
end;

destructor TSilVector.Destroy;
begin
  clear;
  SetCapacity(0);
  inherited;
end;

procedure TSilVector.Clear;
begin
  SetCount(0);
end;

procedure TSilVector.Delete(Index: integer);
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  DisposeItem(Index);
  Dec(FCount);

  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Pointer));

  FList^[FCount] := nil;
end;

procedure TSilVector.Exchange(Index1, Index2: integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    IndexError(Index1);
  if (Index2 < 0) or (Index2 >= FCount) then
    IndexError(Index2);

  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TSilVector.GetCount: integer;
begin
  Result := FCount;
end;

procedure TSilVector.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
  if FCapacity > 8 then Delta := 16 else
  if FCapacity > 0 then Delta := 4 else Delta := 1;
  SetCapacity(FCapacity + Delta);
end;

procedure TSilVector.SetCapacity(NewCapacity: Integer);
begin
  try
    Lock;

    if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
      Error.Throw(SListCapacityError, [NewCapacity], ESilVectorError);

    if NewCapacity <> FCapacity then
    begin
      ReallocMem(FList, NewCapacity * SizeOf(Pointer));
      FCapacity := NewCapacity;
    end;
  finally
    Unlock;
  end;
end;

procedure TSilVector.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  try
    Lock;

    if (NewCount < 0) or (NewCount > MaxListSize) then
      Error.Throw(SListCountError, [NewCount], ESilVectorError);

    if NewCount > FCapacity then
      SetCapacity(NewCount);

    if NewCount > FCount then
      FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
      for I := FCount - 1 downto NewCount do Delete(I);

    FCount := NewCount;
  finally
    Unlock;
  end;
end;

procedure TSilVector.IndexError(Index: Integer);
begin
  Error.Throw(SListIndexError, [Index, FCount], ESilVectorError);
end;

procedure TSilVector.ItemInsert(Index: Integer);
begin
  if FCount = FCapacity then Grow;

  if Index < FCount then
  begin
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Pointer));
    FList^[Index] := nil;
  end;

  Inc(FCount);
end;

function TSilVector.ItemAdd: integer;
begin
  Result := FCount;
  if Result = FCapacity then Grow;
  Inc(FCount);
end;

procedure TSilVector.DisposeItem(Index: Integer);
begin
end;

function TSilVector.CompareItems(const Vector: IVector; index1, index2: integer): integer;
begin
  Result := integer(FList^[Index1]) - integer(FList^[Index2]);
end;

function TSilVector.CompareValue(const Vector: IVector; index: integer; const value): integer;
begin
  Result := integer(FList^[Index]) - integer(value);
end;

procedure TSilVector.Sort(const Comparator: IVectorComparator);
var
  cmp: IVectorComparator;

  procedure DoQuickSort(L, R: Integer);
  var
    I, J, P: Integer;
  begin
    repeat
      I := L;
      J := R;
      P := (L + R) shr 1;
      repeat
        while cmp.CompareItems(Self, I, P) < 0 do Inc(I);
        while cmp.CompareItems(Self, J, P) > 0 do Dec(J);

        if I <= J then
        begin
          Exchange(I, J);

          if P = I then P := J else
          if P = J then P := I;

          Inc(I);
          Dec(J);
        end;
      until I > J;
      if L < J then DoQuickSort(L, J);
      L := I;
    until I >= R;
  end;

begin
  if Comparator = nil then
    cmp := Self as IVectorComparator
  else
    cmp := Comparator;

  try
    Lock;
    DoQuickSort(0, FCount - 1);
  finally
    Unlock;
  end;
end;

procedure TSilVector.Lock;
begin
  if FLock <> nil then
    FLock.Lock;
end;

procedure TSilVector.Unlock;
begin
  if FLock <> nil then
    FLock.Unlock;
end;

function TSilVector.Search(const Value; const Comparable: IVectorComparable): integer;
var
  L, H, I, C: Integer;
  found: boolean;
  cmp: IVectorComparable;
begin
  if Comparable = nil then
    cmp := Self as IVectorComparable
  else
    cmp := Comparable;

  try
    Lock;

    L := 0;
    H := FCount - 1;
    found := false;

    while L <= H do
    begin
      I := (L + H) shr 1;
      C := cmp.CompareValue(self, I, Value);

      if C < 0 then
        L := I + 1
      else
      begin
        H := I - 1;
        if C = 0 then found := true;
      end;
    end;

    if found then
      Result := L
    else
      Result := -1;
  finally
    Unlock;
  end;
end;

end.
