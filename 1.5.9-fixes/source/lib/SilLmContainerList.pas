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

unit SilLmContainerList;

{$I Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilLiLock,
  SilLiContainerTypes,
  SilLiContainerBase,
  SilLiContainerList,
  SilLkContainerBase;

type
  TSilBaseList = class(
    TSilBaseContainer,
    ILockingSet,
    IBaseContainer,
    IBaseList )
  private
    FFirst: PListItem;
    FLast: PListItem;
  protected
    function GetILockingSet: ILockingSet;
  protected // IBaseContainer
    procedure SetCount(const Value: Integer);
    function GetFirstHandle: HItem;
    function GetLastHandle: HItem;
    function GetData(const Item: HItem): HData;
    function IsValid(const Item: HItem): Boolean;
    procedure Clear;
    function Add(Source: HData; Count: Integer; Return: PData = nil): HItem; overload;
    function Insert(Source: HData; Count: Integer; Before: HItem; Return: PData = nil): HItem; overload;
    procedure Delete(First, Last: HItem);
    procedure Exchange(Item1, Item2: HItem);
    procedure Move(Current, Target: HItem);
    function Find(Data: HData; Item: PItem = nil; const Comparator: ITypeComparator = nil; Param: Pointer = nil): Boolean;
    function GetNext(var Item: HItem; Delta: Integer = 1): Boolean;
  protected // IBaseList
    function GetFirst: PListItem;
    function GetLast: PListItem;
    function Alloc(Source: HData = nil): PListItem;
    procedure Dispose(var Rec: PListItem);
    procedure Remove(Rec: PListItem; FreeItem: Boolean);
    function Insert(Before, Rec: PListItem): HItem; overload;
    function Add(const Rec: PListItem): HItem; overload;
  protected
    property First: PListItem read FFirst;
    property Last: PListItem read FLast;
  end;

implementation

uses
  SilBtInt;

{ TSilBaseList }

function TSilBaseList.GetILockingSet: ILockingSet;
begin
  Result := Self;
end;

procedure TSilBaseList.SetCount(const Value: Integer);
begin
  Locked;

  if Value <> FCount then
    if Value > FCount then
      Add(nil, Value - FCount) else
      Delete(FCount - Value, HItem(FFirst));
end;

function TSilBaseList.GetFirstHandle: HItem;
begin
  Result := HItem(FFirst);
end;

function TSilBaseList.GetLastHandle: HItem;
begin
  Result := HItem(FLast);
end;

function TSilBaseList.GetData(const Item: HItem): HData;
begin
  Result := @PListItem(Item).Data;
end;

function TSilBaseList.IsValid(const Item: HItem): Boolean;
begin
  Result := (Item <> HNull) and Assigned(Pointer(Item)) and (PListItem(Item).Magic = CListMagic) and (PListItem(Item).Owner = Self);
end;

procedure TSilBaseList.Clear;
begin
  Delete(HItem(FFirst), HItem(FLast));
end;

function TSilBaseList.Add(Source: HData; Count: Integer; Return: PData): HItem;
var
  Item: HItem;
begin
  Locked;
  
  Result := 0;
  
  for Count := Count - 1 downto 0 do
  begin
    Item := Add(Alloc());
    
    if Assigned(Source) then
      Handler.Copy(@PListItem(Item).Data, Source);

    if Result = 0 then Result := Item;
  end;

  if Assigned(Return) then
    if (Result <> 0) then
      Return^ := @PListItem(Result).Data else
      Return^ := nil;
end;

function TSilBaseList.Insert(Source: HData; Count: Integer; Before: HItem; Return: PData): HItem;
var
  Item: HItem;
begin
  Locked;  
  Result := 0;
  
  for Count := Count - 1 downto 0 do
  begin
    Item := Insert(PListItem(Before), Alloc());

    if Assigned(Source) then
      Handler.Copy(@PListItem(Item).Data, Source);
      
    if Result = 0 then Result := Item;
  end;
  
  if Assigned(Return) then
    if (Result <> 0) then
      Return^ := @PListItem(Result).Data else
      Return^ := nil;
end;

procedure TSilBaseList.Delete(First, Last: HItem);
var
  Temp: PListItem;
begin
  Locked;
  
  while Assigned(PListItem(First)) do
  begin
    Temp := PListItem(First).Link[llNext];
    Remove(PListItem(First), True);
    if (PListItem(First) = PListItem(Last)) then Break; 
    PListItem(First) := Temp;
  end;
end;

procedure TSilBaseList.Exchange(Item1, Item2: HItem);
var
  Rec1, Rec2: PListItem;
  New1, New2: PListItem;
begin
  Locked;
  
  Rec1 := PListItem(Item1);
  Rec2 := PListItem(Item2);
  New1 := Rec1.Link[llNext];
  New2 := Rec2.Link[llNext];
  
  Remove(Rec1, False);
  Remove(Rec2, False);
  Insert(New2, Rec1);
  Insert(New1, Rec2);
end;

procedure TSilBaseList.Move(Current, Target: HItem);
var
  Rec1, Rec2: PListItem;
begin
  Locked;
  
  Rec1 := PListItem(Current);
  Rec2 := PListItem(Target);

  Remove(Rec1, False);
  Insert(Rec2, Rec1);
end;

function TSilBaseList.Find(Data: HData; Item: PItem; const Comparator: ITypeComparator; Param: Pointer): Boolean;
var
  Handler: ITypeComparator;
  Rec: PListItem;
begin
  Locked;
  Result := False;

  if Assigned(Comparator) then
    Handler := Comparator else
    Handler := Self.Handler;

  Rec := FFirst;

  while not Result and Assigned(Rec) do
  begin
    Result := Handler.Compare(Data, @Rec.Data, Param) = 0;
    if Result and Assigned(Item) then Item^ := HItem(Rec);
    Rec := Rec.Link[llNext];
  end;

end;

function TSilBaseList.GetNext(var Item: HItem; Delta: Integer = 1): Boolean;
var
  Link: TListLink;
  Lock: ILock;
begin
  Result := False;

  if Delta > 0 then
    Link := llNext else
    Link := llPrev;

  if Item = HNull then
  begin
    Lock := Locked();
    try
      Item := HItem(FFirst);
      Dec(Delta, Int.Sign(Delta));
      Result := IsValid(Item);
    finally
      Lock := nil;
    end;
  end;

  for Delta := Abs(Delta) - 1 downto 0 do
  begin
    Result := IsValid(Item);
    if not Result then Break;
    Item := HItem(PListItem(Item).Link[Link]);
  end;
end;

function TSilBaseList.GetFirst: PListItem;
begin
  Result := FFirst;
end;

function TSilBaseList.GetLast: PListItem;
begin
  Result := FLast;
end;

function TSilBaseList.Alloc(Source: HData): PListItem;
begin
  Locked;

  System.GetMem(Result, SizeOf(RListItem) + Size);
  Result.Magic := CListMagic;
  Result.Owner := Self;
  Result.Link[llNext] := nil;
  Result.Link[llPrev] := nil;
  Handler.Initialize(@Result.Data);
  if Assigned(Source) then Handler.Copy(@Result.Data, Source)
end;

procedure TSilBaseList.Dispose(var Rec: PListItem);
begin
  Locked;

  Handler.Finalize(@Rec.Data);
  System.FreeMem(Rec, SizeOf(RListItem) + Size);
end;

procedure TSilBaseList.Remove(Rec: PListItem; FreeItem: Boolean);
begin
  Locked;
  try
    if Rec = FFirst then
      FFirst := Rec.Link[llNext];

    if Rec = FLast then
      FLast := Rec.Link[llPrev];

    if Assigned(Rec.Link[llNext]) then
    begin
      Rec.Link[llNext].Link[llPrev] := Rec.Link[llPrev];
      Rec.Link[llNext] := nil;
    end;

    if Assigned(Rec.Link[llPrev]) then
    begin
      Rec.Link[llPrev].Link[llNext] := Rec.Link[llNext];
      Rec.Link[llPrev] := nil;
    end;
    
  finally
    Dec(FCount);
    if FreeItem then Dispose(Rec);
  end;
end;

function TSilBaseList.Insert(Before, Rec: PListItem): HItem;
begin
  Locked;
  
  if not Assigned(Before) then Before := FFirst;  

  Rec.Link[llNext] := Before;
  Rec.Link[llPrev] := Before.Link[llPrev];
  Before.Link[llPrev] := Rec;
  if Assigned(Rec.Link[llPrev]) then
    Rec.Link[llPrev].Link[llNext] := Rec;
  if Before = FFirst then
    FFirst := Rec;
  Inc(FCount);
  Result := HItem(Rec);
end;

function TSilBaseList.Add(const Rec: PListItem): HItem;
begin
  Locked;
  
  if FLast <> nil then
  begin
    FLast.Link[llNext] := Rec;
    Rec.Link[llPrev] := FLast;
    FLast := Rec;
  end else
  begin
    FFirst := Rec;
    FLast := Rec;
  end;
  Inc(FCount);
  Result := HItem(Rec);
end;

end.
