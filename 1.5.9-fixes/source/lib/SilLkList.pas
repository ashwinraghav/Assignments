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

unit SilLkList;

{$I Defines.inc}

interface

uses
  SysUtils,
  SilBeError,
  SilBkPtr,
  SilLdBaseList,
  SilLiList,
  SilLiSort,
  SilLiSearch,
  SilLiCompare,
  SilLiLock,
  SilLiEventList,
  SilLiConnection,
  SilLiEnumerator,
  SilLkInterfaced;

const
  MaxListSize = High(Integer) div SizeOf(Pointer);

type
  ESilListError = class(Exception);

type
  PListData = ^TListData;
  TListData = array[0..MaxListSize - 1] of Pointer;

  TSilList = class (
    // extends
    TSilInterfacedObject,
    // implements
    IList,
    IListItems,
    IItemHandler,
    ISortable,
    ISearchable,
    IEnumerable,
    IItems)
  private
    FCount: Integer;
    FCapacity: Integer;
  protected
    FTypeHandler: HandlerType;
    FTypeData: HandlerType;
    FList: PListData;
  private
    procedure DoQuickSort(L, R: Integer; const Comparator: IComparator);
  protected
    procedure Grow; virtual;
    function GetCapacity: Integer; virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetCount(NewCount: Integer); virtual;
  protected
    procedure FireAdd(Index: Integer); virtual;
    procedure FireInsert(Index: Integer); virtual;
    procedure FireDelete(Index: Integer); virtual;
  protected
    property Capacity: Integer read GetCapacity write SetCapacity;
    property List: PListData read FList;
    property Count: Integer read FCount;
  protected // IItemHandler
    function GetDataType: HandlerType;
    function GetDataInfo: Pointer;
  protected // IEnumerable
    function Enumerate(var Enum: IEnumerator; out Item): Boolean; virtual;
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; virtual;
  protected // IList
    function GetCount: Integer; virtual;
    function ItemAdd(const Item): Integer; virtual;
    procedure ItemInsert(Index: Integer; const Item); virtual;
    function ItemRemove(const Item): Integer; virtual;
    procedure ItemGet(Index: Integer; var Item); virtual;
    procedure ItemSet(Index: Integer; const Item); virtual;
    function ItemPtr(Index: Integer): Pointer; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Clear; virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function ValidIndex(Index: Integer): Boolean; virtual;
    procedure CheckIndex(Index: Integer);
  protected // ISortable
    function CustomSort(const Comparator: IComparator): Boolean;
    function Sort: Boolean; virtual;
  protected // ISearchable
    function CustomSearch(const Comparator: IComparable; var Index: Integer): Boolean;
    function Search(const Value: Variant; var Index: Integer): Boolean; virtual;
  public
    constructor Create(Locked: Boolean = false; CastPtr: HandlerType = nil; CastData: Pointer = nil); virtual;
    destructor Destroy; override;
  end;

implementation

uses
  SilBtError,
  SilLmLockedSection,
  SilLmListEnumerator;
           
{ TSilList }

constructor TSilList.Create(Locked: Boolean; CastPtr: HandlerType; CastData: Pointer);
begin
  inherited Create(Locked);

  FTypeHandler := BaseHandler.Check(CastPtr);
  FTypeData := HandlerType(CastData);
end;

destructor TSilList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSilList.CheckIndex(Index: Integer);
begin
  if not ValidIndex(Index) then Error.Throw(SListIndexError, [Index, FCount], ESilListError);
end;

function TSilList.ValidIndex(Index: Integer): Boolean;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    Result := (Index >= 0) and (Index < FCount);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    CheckIndex(Index1);
    CheckIndex(Index2);

    Item := FList^[Index1];
    FList^[Index1] := FList^[Index2];
    FList^[Index2] := Item;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
  if FCapacity > 8 then Delta := 16 else
  if FCapacity > 0 then Delta := 4 else Delta := 1;
  SetCapacity(FCapacity + Delta);
end;

procedure TSilList.SetCapacity(NewCapacity: Integer);
begin
  try
    if Lockable <> nil then Lockable.Lock;
    if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
      Error.Throw(SListCapacityError, [NewCapacity], ESilListError);
    if NewCapacity <> FCapacity then
    begin
      ReallocMem(FList, NewCapacity * SizeOf(Pointer));
      FCapacity := NewCapacity;
    end;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    if (NewCount < 0) or (NewCount > MaxListSize) then
      Error.Throw(SListCountError, [NewCount], ESilListError);
    if NewCount > FCapacity then
      SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
    else
      for I := FCount - 1 downto NewCount do Delete(I);
    FCount := NewCount;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TSilList.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TSilList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

function TSilList.ItemAdd(const Item): Integer;
var
  PValue: Pointer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    Result := FCount;
    if Result = FCapacity then Grow;
    Inc(FCount);
    FTypeHandler.ToPtr(Item, PValue, FTypeData);
    FList^[Result] := PValue;
    FireAdd(Result);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.ItemInsert(Index: Integer; const Item);
begin
  try
    if Lockable <> nil then Lockable.Lock;
    if FCount = FCapacity then Grow;

    if Index < FCount then
    begin
      System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(Pointer));
      FList^[Index] := nil;
    end;

    Inc(FCount);
    ItemSet(Index, Item);
    FireInsert(Index);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    CheckIndex(Index);
    Temp := FList^[Index];

    FireDelete(Index);
    Dec(FCount);
    if Temp <> nil then FTypeHandler.Free(Temp, FTypeData);

    if Index < FCount then
      System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(Pointer));

    FList^[FCount] := nil;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.ItemGet(Index: Integer; var Item);
var
  Temp: Pointer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    CheckIndex(Index);
    Temp := FList^[Index];
    FTypeHandler.ToObj(Temp, Item, FTypeData);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilList.ItemPtr(Index: Integer): Pointer;
begin
  Result := FList^[Index];
end;

procedure TSilList.ItemSet(Index: Integer; const Item);
var
  PValue: Pointer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    CheckIndex(Index);
    PValue := FList^[Index];
    if PValue <> nil then FTypeHandler.Free(PValue, FTypeData);
    FTypeHandler.ToPtr(Item, PValue, FTypeData);
    FList^[Index] := PValue;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilList.ItemRemove(const Item): Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    for Result := 0 to FCount - 1 do
      if FTypeHandler.Compare(FList^[Result], Item, FTypeData) = 0 then
      begin
        Delete(Result);
        Exit;
      end;

    Result := -1;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilList.DoQuickSort(L, R: Integer; const Comparator: IComparator);

  function DoCompare(P1, P2: Pointer): Integer;
  begin
    Result := Comparator.Compare(P1, P2);
  end;

var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while DoCompare(FList^[I], FList^[P]) < 0 do Inc(I);
      while DoCompare(FList^[J], FList^[P]) > 0 do Dec(J);
      if I <= J then
      begin
        Exchange(I, J);

        if P = I then P := J else
        if P = J then P := I;

        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then DoQuickSort(L, J, Comparator);
    L := I;
  until I >= R;
end;

function TSilList.CustomSort(const Comparator: IComparator): Boolean;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    Result := FCount > 1;
    if Result then DoQuickSort(0, FCount - 1, Comparator);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilList.Sort: Boolean;
begin
  Result := false;
end;

function TSilList.CustomSearch(const Comparator: IComparable; var Index: Integer): Boolean;

  function DoCompare(P1: Pointer): Integer;
  begin
    Result := Comparator.CompareTo(P1);
  end;

var
  L, H, I, C: Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    Result := False;
    L := 0;
    H := FCount - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := DoCompare(FList^[I]);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then Result := True;
      end;
    end;
    Index := L;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilList.Search(const Value: Variant; var Index: Integer): Boolean;
begin
  Result := false;
end;

function TSilList.Enumerate(var Enum: IEnumerator; out Item): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, Lockable <> nil) then
    Result := Enum.HasMore
  else
    Result := false;

  if Result then
    Enum.Get(Item)
  else
    Enum := nil;
end;

function TSilList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Count > 0;
  if Result then Enum := TListEnumerator.Create(Self, Locked, FTypeHandler, FTypeData);
end;

function TSilList.GetDataInfo: Pointer;
begin
  Result := FTypeData;
end;

function TSilList.GetDataType: HandlerType;
begin
  Result := FTypeHandler;
end;

procedure TSilList.FireAdd(Index: Integer);
var
  n: IEnumerator;
  l: IListEvents;
begin
  if Events <> nil then
    with Events do
      while Enumerate(n, l, IListEvents) do
        l.OnListAdd(Self, Index);
end;

procedure TSilList.FireDelete(Index: Integer);
var
  n: IEnumerator;
  l: IListEvents;
begin
  if Events <> nil then
    with Events do
      while Enumerate(n, l, IListEvents) do
        l.OnListDelete(Self, Index);
end;

procedure TSilList.FireInsert(Index: Integer);
var
  n: IEnumerator;
  l: IListEvents;
begin
  if Events <> nil then
    with Events do
      while Enumerate(n, l, IListEvents) do
        l.OnListInsert(Self, Index);
end;

end.
