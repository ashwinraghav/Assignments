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

unit SilLmEventList;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilLiLinkedList,
  SilLiEnumerator,
  SilLiLock,
  SilLiConnection,
  SilLiReadWriteLock,
  SilLiEventList,
  SilLkAggregated;

type
  TEventList = class (
    // extends
    TSilAggregatedObject,
    // implements
    IConnectable,
    IEventList,
    IEvents)
  private
    FList: ILinkedList;
  private
    function DoFindRef(const Obj: IUnknown; out Ptr: PPointer): Boolean;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); reintroduce;
    procedure RemoveListener(const Listener: IUnknown); reintroduce;
  protected // IEventList
    function GetCount: Integer;
    function Enumerate(var Enum: IEnumerator; out Item; const IID: TGuid): Boolean;
    procedure AddList(const List: IEvents; KeepRef: Boolean);
    procedure Clear;
  protected // ICrossReference
    function DropRef(Ptr: PPointer): Boolean; override;
  public
    constructor Create(const Controller: IUnknown); reintroduce; 
    destructor Destroy; override;
  end;

implementation

uses
  SilLtTool,
  SilLtReference,
  SilBkPtr,
  SilLmLinkedList;

type
  TLinkedEventList = class (TSilLinkedList)
  private
    FLock: IReadWriteLock;
  protected
    function CreateLockable: ILockable; override;
  protected // ILockable
    procedure Lock; override;
    procedure Unlock; override;
  end;

{ TEventList }

constructor TEventList.Create(const Controller: IInterface);
begin
  inherited Create(Controller);
  FList := TLinkedEventList.Create(PointerHandler, true);
end;

destructor TEventList.Destroy;
begin
  Clear;
  FList := nil;

  inherited;
end;

function TEventList.GetCount: Integer;
begin
  if Assigned(FList) then
    Result := FList.Count else
    Result := 0;
end;

procedure TEventList.AddListener(const Listener: IInterface; KeepRef: Boolean);
var
  PItem: PPointer;
begin
  if not DoFindRef(Listener, PItem) then
  begin
    New(PItem);

    if not KeepRef then
    begin
      PItem^ := Pointer(Listener);
      MakeRef(Listener, PItem);
    end else
    begin
      PItem^ := nil;
      IUnknown(PItem^) := Listener;
    end;

    FList.Add(PItem^);
  end;
end;

procedure TEventList.RemoveListener(const Listener: IUnknown);
var
  PItem: PPointer;
begin
  if Assigned(FList) and DoFindRef(Listener, PItem) then
  begin
    FList.Remove(PItem^);

    if not DropRef(PItem) then
      IUnknown(PItem^) := nil;

    Dispose(PItem);
  end;
end;

function TEventList.Enumerate(var Enum: IEnumerator; out Item; const IID: TGuid): Boolean;
var
  PItem: PPointer;
begin
  repeat
    Result := Assigned(FList) and FList.Enumerate(Enum, PItem);
  until not Result or Reference.GetInterface(IUnknown(PItem^), IID, Item);
end;

function TEventList.DoFindRef(const Obj: IUnknown; out Ptr: PPointer): Boolean;
var
  Enum: IEnumerator;
  Item: PPointer;
begin
  while Assigned(FList) and FList.Enumerate(Enum, Item) do
    if Ref.SameObject(IUnknown(Item^), Obj) then
    begin
      Ptr := Item;
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TEventList.DropRef(Ptr: PPointer): Boolean;
begin
  RemoveListener(IUnknown(Ptr^));
  Result := inherited DropRef(Ptr);
end;

procedure TEventList.AddList(const List: IEvents; KeepRef: Boolean);
var
  Enum: IEnumerator;
  Item: IUnknown;
begin
  while Assigned(FList) and List.Enumerate(Enum, Item, IUnknown) do
    AddListener(Item, KeepRef);
end;

procedure TEventList.Clear;
var
  Enum: IEnumerator;
  Item: PPointer;
begin
  while Assigned(FList) and FList.Enumerate(Enum, Item) do
    DropRef(Item);
end;

{ TLinkedEventList }

function TLinkedEventList.CreateLockable: ILockable;
begin
  FLock := Tk.ReadWriteLock;
  Result := FLock as ILockable;
end;

procedure TLinkedEventList.Lock;
begin
  if FLock = nil then CreateLockable;
  FLock.BeginRead;
end;

procedure TLinkedEventList.Unlock;
begin
  if FLock <> nil then FLock.EndRead;
end;

end.
