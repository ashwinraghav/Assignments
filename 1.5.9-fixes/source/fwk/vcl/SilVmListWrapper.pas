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

unit SilVmListWrapper;

{$I Defines.inc}

interface

uses
  Classes,
  Sil;

type
  TListWrapper = class (
  // extends
    TSilInterfacedObject,
  // implements
    IEnumerable,
    IList,
    IPointerList)
  private
    FList: TList;
    FFreeOnDestroy: Boolean;
    FAccessLock: ILockable;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  protected // IList
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer); 
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    procedure Move(CurIndex, NewIndex: Integer);
    function Locked: ILock; override; 
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
    function Get(Index: Integer): Pointer;
    procedure Clear;
    function ItemPtr(Index: Integer): Pointer;
  protected // IPointerList
    function Add(Item: Pointer): Integer;
    procedure AddList(const Source: IPointerList);
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    function Remove(Item: Pointer): Integer;
    function First: Pointer;
    function Last: Pointer;
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; Value: Pointer);
  protected
    property List: TList read FList;
  public
    constructor Create(List: TList; Locked: Boolean = false; FreeOnDestroy: Boolean = false);
    destructor Destroy; override;
  end;

implementation

uses
  SilLdContainer;
  
{ TListWrapper }

constructor TListWrapper.Create(List: TList; Locked: Boolean; FreeOnDestroy: Boolean);
begin
  inherited Create;

  if Locked then FAccessLock := Sil.Os.IPC.CriticalSection();

  if List = nil then
    begin
      List := TList.Create;
      FreeOnDestroy := True;
    end;

  FList := List;
  FFreeOnDestroy := FreeOnDestroy;
end;

destructor TListWrapper.Destroy;
begin
  if FFreeOnDestroy then FList.Free;
  FList := nil;
  FAccessLock := nil;
  inherited;
end;

function TListWrapper.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := FList.Count > 0;
  if Result then Enum := SIL.List.ListEnumerator(Self, Locked);
end;

function TListWrapper.Enumerate(var Enum: IEnumerator; out Item): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, FAccessLock <> nil) then
    Result := Enum.HasMore
  else
    Result := false;

  if not Result then
    begin
      if Enum <> nil then
        Enum.Detach;
      Enum := nil;
    end
  else
    Enum.Get(Item)
end;

function TListWrapper.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TListWrapper.Delete(Index: Integer);
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    FList.Delete(Index);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

procedure TListWrapper.Exchange(Index1, Index2: Integer);
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    FList.Exchange(Index1, Index2);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

procedure TListWrapper.Move(CurIndex, NewIndex: Integer);
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    FList.Move(CurIndex, NewIndex);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

function TListWrapper.Locked: ILock;
begin
  Result := SIL.Lock.Take(FAccessLock);
end;

function TListWrapper.ValidIndex(Index: Integer): Boolean;
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    Result := (Index >= 0) and (Index < FList.Count);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

procedure TListWrapper.CheckIndex(Index: Integer);
begin
  if not ValidIndex(Index) then
    Sil.Error.Throw(SListIndexError, [Index]);
end;

function TListWrapper.Get(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

function TListWrapper.Add(Item: Pointer): Integer;
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    Result := FList.Add(Item);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

procedure TListWrapper.AddList(const Source: IPointerList);
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    //..
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

function TListWrapper.First: Pointer;
begin
  Result := FList.First;
end;

function TListWrapper.GetItem(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

function TListWrapper.IndexOf(Item: Pointer): Integer;
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    Result := FList.IndexOf(Item);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

procedure TListWrapper.Insert(Index: Integer; Item: Pointer);
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    FList.Insert(Index, Item);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

function TListWrapper.Last: Pointer;
begin
  Result := FList.Last;
end;

function TListWrapper.Remove(Item: Pointer): Integer;
begin
  try
    if FAccessLock <> nil then FAccessLock.Lock;
    Result := FList.Remove(Item);
  finally
    if FAccessLock <> nil then FAccessLock.Unlock;
  end;
end;

procedure TListWrapper.SetItem(Index: Integer; Value: Pointer);
begin
  FList[Index] := Value;
end;

procedure TListWrapper.Clear;
begin
  FList.Clear;
end;

function TListWrapper.ItemPtr(Index: Integer): Pointer;
begin
  Result := FList[Index];
end;

procedure TListWrapper.SetCount(Value: Integer);
begin
  FList.Count := Value;
end;

function TListWrapper.GetCapacity: Integer;
begin
  Result := FList.Capacity;
end;

procedure TListWrapper.SetCapacity(NewCapacity: Integer);
begin
  FList.Capacity := NewCapacity;
end;

end.
