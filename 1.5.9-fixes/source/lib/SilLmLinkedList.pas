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

unit SilLmLinkedList;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilBkPtr,
  SilLiList,
  SilLiEnumerator,
  SilLiLinkedList,
  SilLkAbstractEnumerator;

type
  PListItem = ^RListItem;

  RListItem = record
    Next, Prev: PListItem;
    Ptr: Pointer;
  end;

  IPrivateLinkedList = interface (ILinkedList)
    ['{FBF8450C-08F1-4F94-97CD-6FCA68A9CA48}']
    function DoGetFirst: PListItem;
    function DoGetLast: PListItem;
    property First: PListItem read DoGetFirst;
    property Last: PListItem read DoGetLast;
  end;

  TSilLinkedList = class(
    TSilInterfacedObject,
    IEnumerable,
    IItemHandler,
    IItemization,
    ILinkedItems,
    IPrivateLinkedList,
    ILinkedList)
  private
    FFirst: PListItem;
    FLast: PListItem;
    FCount: Integer;
    FTypeHandler: HandlerType;
    FTypeData: Pointer;
  protected // IItemization
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  protected // IItemHandler
    function GetDataType: HandlerType;
    function GetDataInfo: Pointer;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected // ILinkedItems
    function GetCount: Integer;
    function Contains(const Item): Boolean;
  protected // ILinkedList
    procedure Clear;
    procedure Add(const Item);
    procedure Insert(const Before; const Item);
    procedure Remove(const Item);
    function GetFirst(var Item): Boolean;
    function GetLast(var Item): Boolean;
  protected
    function DoGetFirst: PListItem;
    function DoGetLast: PListItem;
  protected
    function DoAlloc(const Item): PListItem;
    procedure DoDispose(var Rec: PListItem);
    function DoEnumerate(var Enum: IEnumerator; out Rec: PListItem): Boolean;
    function DoFind(const Item; out Rec: PListItem): Boolean;
    procedure DoRemove(Rec: PListItem);
    procedure DoInsert(const Before, Rec: PListItem);
    procedure DoAdd(const Rec: PListItem);
    procedure DoClear;
  public
    constructor Create(const TypeHandler: HandlerType; Locked: Boolean = False; const TypeData: Pointer = nil);
    destructor Destroy; override;
  end;

  TLinkedEnumerator = class(TAbstractEnumerator)
  private
    FCurrent: PListItem;
    FNext: PListItem;
    procedure SetCurrent(const Value: PListItem);
  protected
    function DoHasMore: Boolean; override;
    function GetCurrent: Pointer; override;
    function Next: Boolean; override;
  public
    constructor Create(const Container: IUnknown; const Locked: Boolean = false; const TypeHandler: HandlerType = nil; const TypeData: Pointer = nil); override;
  end;


implementation

uses
  SilOtTool;

{ TSilLinkedList }

constructor TSilLinkedList.Create(const TypeHandler: HandlerType; Locked: Boolean; const TypeData: Pointer);
begin
  inherited Create(Locked);

  FTypeHandler := BaseHandler.Check(TypeHandler);
  FTypeData := TypeData;
end;

destructor TSilLinkedList.Destroy;
begin
  Clear;
  inherited;
end;

function TSilLinkedList.Enumerate(var Enum: IEnumerator; out Item): Boolean;
var
  Rec: PListItem;
begin
  Result := (FCount > 0) and DoEnumerate(Enum, Rec);
  if Result then FTypeHandler.ToObj(Rec.Ptr, Item, FTypeData);
end;

function TSilLinkedList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := FCount > 0;
  if Result then Enum := TLinkedEnumerator.Create(Self, Locked);
end;

procedure TSilLinkedList.Add(const Item);
var
  Rec: PListItem;
begin
  Rec := DoAlloc(Item);
  try
    Lock;
    try
      DoAdd(Rec);
    finally
      Unlock;
    end;
  except
    DoDispose(Rec);
    raise;
  end;
end;

procedure TSilLinkedList.Insert(const Before; const Item);
var
  ItemRec, BeforeRec: PListItem; 
begin
  Lock;
  try
    if DoFind(Before, BeforeRec) then
    begin
      ItemRec := DoAlloc(Item);
      try
        DoInsert(BeforeRec, ItemRec);
      except
        DoDispose(ItemRec);
        raise;
      end;
    end;
  finally
    Unlock;
  end;
end;

procedure TSilLinkedList.Remove(const Item);
var
  Rec: PListItem;
begin
  Lock;
  try
    if DoFind(Item, Rec) then
      DoRemove(Rec);
  finally
    Unlock;
  end;
end;

procedure TSilLinkedList.Clear;
begin
  Lock;
  try
    DoClear;
  finally
    Unlock;
  end;
end;

function TSilLinkedList.GetCount: Integer;
begin
  Result := FCount;
end;

function TSilLinkedList.Contains(const Item): Boolean;
var
  Rec: PListItem;
begin
  Result := DoFind(Item, Rec);
end;

function TSilLinkedList.DoEnumerate(var Enum: IEnumerator; out Rec: PListItem): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, HasLockable) then
    Result := Enum.HasMore
  else
    Result := false;

  if Result then
    Enum.Get(Rec)
  else
    Enum := nil;
end;

function TSilLinkedList.DoFind(const Item; out Rec: PListItem): Boolean;
var
  Enum: IEnumerator;
  PItem: Pointer;
begin
  Result := False;
  FTypeHandler.ToPtr(Item, PItem);

  try
    while DoEnumerate(Enum, Rec) do
      if FTypeHandler.Compare(PItem, Rec.Ptr, FTypeData) = 0 then
      begin
        Result := True;
        Exit;
      end;
  finally
    FTypeHandler.Free(PItem);
  end;
end;

procedure TSilLinkedList.DoRemove(Rec: PListItem);
begin
  try
    if Rec.Next <> nil then
      Rec.Next.Prev := Rec.Prev;

    if Rec.Prev <> nil then
      Rec.Prev.Next := Rec.Next;

    if Rec = FFirst then
      FFirst := Rec.Next;

    if Rec = FLast then
      FLast := Rec.Prev;
  finally
    Dec(FCount);
    DoDispose(Rec);
  end;
end;

function TSilLinkedList.DoAlloc(const Item): PListItem;
begin
  New(Result);
  Result.Next := nil;
  Result.Prev := nil;
  FTypeHandler.ToPtr(Item, Result.Ptr, FTypeData);
end;

procedure TSilLinkedList.DoDispose(var Rec: PListItem);
begin
  if Assigned(Rec) then
  try
    Rec.Next := nil;
    Rec.Prev := nil;
    FTypeHandler.Free(Rec.Ptr, FTypeData);
  finally
    Dispose(Rec);
    Rec := nil;
  end;
end;

procedure TSilLinkedList.DoClear;
begin
  while FFirst <> nil do
    DoRemove(FFirst);
end;

procedure TSilLinkedList.DoAdd(const Rec: PListItem);
begin
  if FLast <> nil then
  begin
    FLast.Next := Rec;
    Rec.Prev := FLast;
    FLast := Rec;
  end else
  begin
    FFirst := Rec;
    FLast := Rec;
  end;
  Inc(FCount);
end;

procedure TSilLinkedList.DoInsert(const Before, Rec: PListItem);
begin
  Rec.Next := Before;
  Rec.Prev := Before.Prev;
  Before.Prev := Rec;
  if Assigned(Rec.Prev) then
    Rec.Prev.Next := Rec;
  if Before = FFirst then
    FFirst := Rec;
  Inc(FCount);
end;

function TSilLinkedList.GetDataInfo: Pointer;
begin
  Result := FTypeData;
end;

function TSilLinkedList.GetDataType: HandlerType;
begin
  Result := FTypeHandler;
end;

function TSilLinkedList.DoGetFirst: PListItem;
begin
  Result := FFirst;
end;

function TSilLinkedList.DoGetLast: PListItem;
begin
  Result := FLast;
end;

function TSilLinkedList.GetFirst(var Item): Boolean;
begin
  FTypeHandler.ToObj(FFirst.Ptr, Item, FTypeData);
  Result := Assigned(Pointer(Item));
end;

function TSilLinkedList.GetLast(var Item): Boolean;
begin
  FTypeHandler.ToObj(FLast.Ptr, Item, FTypeData);
  Result := Assigned(Pointer(Item));
end;

{ TLinkedEnumerator }

constructor TLinkedEnumerator.Create(const Container: IUnknown; const Locked: Boolean; const TypeHandler: HandlerType; const TypeData: Pointer);
begin
  inherited;
  with Container as IPrivateLinkedList do SetCurrent(First);
end;

function TLinkedEnumerator.DoHasMore: Boolean;
begin
  Result := Assigned(FCurrent);
end;

procedure TLinkedEnumerator.SetCurrent(const Value: PListItem);
begin
  FCurrent := Value;
  if Assigned(Value) then
    FNext := FCurrent.Next else
    FNext := nil;
end;

function TLinkedEnumerator.GetCurrent: Pointer;
begin
  Result := FCurrent;
end;

function TLinkedEnumerator.Next: Boolean;
begin
  SetCurrent(FNext);

  Result := DoHasMore;

  if Result then
    Inc(FIteration);
end;

end.
