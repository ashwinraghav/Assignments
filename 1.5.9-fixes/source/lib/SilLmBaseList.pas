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

unit SilLmBaseList;

{$I Defines.inc}

interface

uses
  SilLdBaseList;
  
const
  MaxListSize = High( integer ) div sizeof( Pointer );

type
  TCustomList = class;
  TBaseList = class;

  PListData = ^TListData;
  TListData = array[ 0..MaxListSize - 1 ] of Pointer;
  TListSortCompare = function (Item1, Item2: Pointer): Integer;
  TListNotification = (lnAdded, lnExtracted, lnDeleted);

  TCustomList = class(TObject)
  protected
    FList: PListData;
    FCount: Integer;
    FCapacity: Integer;
  protected
    function Get(Index: Integer): Pointer; virtual;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer); virtual;
    procedure Notify(Ptr: Pointer; Action: TListNotification); virtual;
    function GetCapacity: Integer; virtual;
    function GetCount: Integer; virtual;
    procedure SetCapacity(NewCapacity: Integer); virtual;
    procedure SetCount(NewCount: Integer); virtual;
  protected
    property Capacity: Integer read GetCapacity write SetCapacity;
    property List: PListData read FList;
  public
    destructor Destroy; override;
    function Add(Item: Pointer): Integer; virtual;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    class procedure Error(const Msg: string; const Data: array of const ); overload; virtual;
    class procedure Error(Msg: PResStringRec; const Data: array of const ); overload;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function Expand: TCustomList;
    function Extract(Item: Pointer): Pointer; virtual;
    function First: Pointer; virtual;
    function IndexOf(Item: Pointer): Integer; virtual;
    procedure Insert(Index: Integer; Item: Pointer); virtual;
    function Last: Pointer; virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    function Remove(Item: Pointer): Integer; virtual;
    procedure Pack; virtual;
    procedure Sort(Compare: TListSortCompare); virtual;
  public
    property Items[Index: Integer]: Pointer read Get write Put; default;
    property Count: Integer read GetCount write SetCount;
  end;

  TBaseList = class(TCustomList)
    property Capacity;
    property List;
  end;

implementation

uses
  SysUtils;

type  
  EListError = class(Exception);

  
{ TBaseList }

destructor TCustomList.Destroy;
begin
  Clear;
end;

function TCustomList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

procedure TCustomList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TCustomList.Delete(Index: Integer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, [Index, FCount]);
  Temp := FList^[Index];
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(Pointer));
  if Temp <> nil then
    Notify(Temp, lnDeleted);
end;

class procedure TCustomList.Error(const Msg: string; const Data: array of const );

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, Data) at ReturnAddr;
end;

class procedure TCustomList.Error(Msg: PResStringRec; const Data: array of const );
begin
  TCustomList.Error(LoadResString(Msg), Data);
end;

procedure TCustomList.Exchange(Index1, Index2: Integer);
var
  Item: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then
    Error(@SListIndexError, [Index1, FCount]);
  if (Index2 < 0) or (Index2 >= FCount) then
    Error(@SListIndexError, [Index2, FCount]);
  Item := FList^[Index1];
  FList^[Index1] := FList^[Index2];
  FList^[Index2] := Item;
end;

function TCustomList.Expand: TCustomList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TCustomList.First: Pointer;
begin
  Result := Get(0);
end;

function TCustomList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, [Index, FCount]);
  Result := FList^[Index];
end;

procedure TCustomList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TCustomList.IndexOf(Item: Pointer): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FList^[Result] <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TCustomList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then
    Error(@SListIndexError, [Index, FCount]);
  if FCount = FCapacity then
    Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(Pointer));
  FList^[Index] := Item;
  Inc(FCount);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TCustomList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TCustomList.Move(CurIndex, NewIndex: Integer);
var
  Item: Pointer;
begin
  if CurIndex <> NewIndex then
  begin
    if (NewIndex < 0) or (NewIndex >= FCount) then
      Error(@SListIndexError, [NewIndex, FCount]);
    Item := Get(CurIndex);
    FList^[CurIndex] := nil;
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex] := Item;
  end;
end;

procedure TCustomList.Put(Index: Integer; Item: Pointer);
var
  Temp: Pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    Error(@SListIndexError, [Index, FCount]);
  Temp := FList^[Index];
  FList^[Index] := Item;
  if Temp <> nil then
    Notify(Temp, lnDeleted);
  if Item <> nil then
    Notify(Item, lnAdded);
end;

function TCustomList.Remove(Item: Pointer): Integer;
begin
  Result := IndexOf(Item);
  if Result >= 0 then
    Delete(Result);
end;

procedure TCustomList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    if Items[I] = nil then
      Delete(I);
end;

function TCustomList.GetCapacity: Integer;
begin
  result := FCapacity;
end;

function TCustomList.GetCount: Integer;
begin
  result := FCount;
end;

procedure TCustomList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then
    Error(@SListCapacityError, [NewCapacity]);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(Pointer));
    FCapacity := NewCapacity;
  end;
end;

procedure TCustomList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then
    Error(@SListCountError, [NewCount]);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(Pointer), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure QuickSort(SortList: PListData; L, R: Integer;
  SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while SCompare(SortList^[I], P) < 0 do
        Inc(I);
      while SCompare(SortList^[J], P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TCustomList.Sort(Compare: TListSortCompare);
begin
  if (FList <> nil) and (Count > 0) then
    QuickSort(FList, 0, Count - 1, Compare);
end;

function TCustomList.Extract(Item: Pointer): Pointer;
var
  I: Integer;
begin
  Result := nil;
  I := IndexOf(Item);
  if I >= 0 then
  begin
    Result := Item;
    FList^[I] := nil;
    Delete(I);
    Notify(Result, lnExtracted);
  end;
end;

procedure TCustomList.Notify(Ptr: Pointer; Action: TListNotification);
begin
end;

end.

