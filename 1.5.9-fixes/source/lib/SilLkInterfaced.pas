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

unit SilLkInterfaced;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilLiLock,
  SilLiReference,
  SilLiConnection,
  SilLiEventList,
  SilLiCrossReference,
  SilLiPointerList,
  SilLiObject,
  SilLkObject;

type
  PCrossReference = ^RCrossReference;
  RCrossReference = record
    Field: PPointer;
    Obj: Pointer;
  end;

type
  TSilInterfacedObject = class(
    // extends
    TSilObject,
    // implements
    ILockable,
    IConnectable,
    ICrossReference,
    IConnections,

    IDispatchable,
    IReferenceable,
    ILockingSet,
    ISynchronizable,
    IObject )
  private
    FLockable: ILockable;
    FEvents: Pointer;
    FConnectable: IUnknown;
    FCrossRefs: IPointerList;
  private
    function GetConnectable: IConnectable;
    procedure DoCheckConnections;
    function DoRemoveCrossRef(const Obj: IInterface; out Item: PCrossReference): Boolean;
    function DoAddCrossRef(const Obj: IInterface; Ptr: PPointer): PCrossReference;
    procedure DoCheckCrossRefs;
    procedure DoDropAllCrossRefs;
  protected
    function HasLockable: Boolean;
  protected
    function CreateLockable: ILockable; virtual;
    function GetIsLockable: Boolean; override;
    function GetLockable: ILockable; override;
    function Locked: ILock; override;
  protected // ISynchronizable
    function ISynchronizable.Lock = Locked;
  protected
    function GetIConnections: IConnections;
  protected // ILockable
    procedure Lock; virtual;
    procedure Unlock; virtual;
  protected // IConnectable
    procedure AddListener(const Listener: IUnknown; KeepRef: Boolean); virtual;
    procedure RemoveListener(const Listener: IUnknown); virtual;
  protected // ICrossReference
    procedure MakeRef(const Obj: IUnknown; Ptr: PPointer); virtual;
    function DropRef(Ptr: PPointer): Boolean; virtual;
    procedure ChangeRef(const Obj: IUnknown; Ptr: PPointer); virtual;
  protected // IConnections
    function GetHasConnections: Boolean;
    function GetEventList: IEventList;
  protected
    property HasConnections: Boolean read GetHasConnections;
    property Connectable: IConnectable read GetConnectable;
    property Events: IEventList read GetEventList;
  protected 
    property Lockable: ILockable read FLockable;
  public
    constructor Create(Locked: Boolean; const Owner: IUnknown = nil; Param: Pointer = nil); overload;
    destructor Destroy; override;
  end;

  TInterfacedObject = class(TSilInterfacedObject) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

uses
  SilLdInterfaced,
  SilLiEnumerator,
  SilLtReference,
  SilAfLockedIncrement,
  SilLmPointerList,
  SilLmEventList,
  SilOsTypes,
  SilBtInt,
  SilBtError,
  SilOtTool;

{ TSilInterfacedObject }

constructor TSilInterfacedObject.Create(Locked: Boolean; const Owner: IUnknown; Param: Pointer);
begin
  CreateNew(Owner, Param);
  if Locked then FLockable := CreateLockable;
end;

destructor TSilInterfacedObject.Destroy;
begin
  FConnectable := nil;

  DoDropAllCrossRefs;
  FCrossRefs := nil;

  inherited;
  FLockable := nil;
  FEvents := nil;
end;

function TSilInterfacedObject.HasLockable: Boolean;
begin
  Result := FLockable <> nil;
end;

function TSilInterfacedObject.CreateLockable: ILockable;
begin
  Result := OS.IPC.CriticalSection;
end;

function TSilInterfacedObject.GetIsLockable: Boolean;
begin
  Result := HasLockable;
end;

function TSilInterfacedObject.GetLockable: ILockable;
begin
  Result := FLockable;
end;

function TSilInterfacedObject.Locked: ILock;
begin
  if FLockable = nil then FLockable := CreateLockable;
  Result := inherited Locked;
end;

function TSilInterfacedObject.GetIConnections: IConnections;
begin
  Result := Self;
end;

procedure TSilInterfacedObject.Lock;
begin
  if FLockable <> nil then FLockable.Lock;
end;

procedure TSilInterfacedObject.Unlock;
begin
  if FLockable <> nil then FLockable.Unlock;
end;

procedure TSilInterfacedObject.AddListener(const Listener: IInterface; KeepRef: Boolean);
begin
  DoCheckConnections;
  Connectable.AddListener(Listener, KeepRef);
end;

procedure TSilInterfacedObject.RemoveListener(const Listener: IUnknown);
begin
  if Assigned(FConnectable) then Connectable.RemoveListener(Listener);
end;

procedure TSilInterfacedObject.MakeRef(const Obj: IInterface; Ptr: PPointer);
var
  Item: PCrossReference;
  CrossRef: ICrossReference;
begin
  if Assigned(Obj) then
    if Ref.GetInterface(Obj, ICrossReference, CrossRef) then
    begin
      DoCheckCrossRefs;
      Item := DoAddCrossRef(Obj, Ptr);

      if Ptr <> nil then
      begin
        CrossRef.MakeRef(Self, nil);
        Ptr^ := Item.Obj;
      end;
    end else
      Ptr^ := Pointer(Obj);
end;

function TSilInterfacedObject.DropRef(Ptr: PPointer): Boolean;
var
  Item: PCrossReference;
  Obj: IUnknown;
  This: IUnknown;
  CrossRef: ICrossReference;
begin
  Result := false;

  if Assigned(FCrossRefs) and (Ptr^ <> nil) then
  begin
    Obj := IUnknown(Ptr^);

    if Ref.GetInterface(Obj, ICrossReference, CrossRef) then
    begin
      Result := DoRemoveCrossRef(Obj, Item);

      if Result then
      begin
        This := Self;
        CrossRef.DropRef(@This);
        if Item.Field <> nil then Item.Field^ := nil;
        Dispose(Item);
      end;
    end else
      Ptr^ := nil;
  end{ else
    Ptr^ := nil};
end;

procedure TSilInterfacedObject.ChangeRef(const Obj: IInterface; Ptr: PPointer);
begin
  if Assigned(Ptr^) then DropRef(Ptr);
  if Assigned(Obj) then MakeRef(Obj, Ptr);
end;

function TSilInterfacedObject.GetHasConnections: Boolean;
begin
  Result := (FEvents <> nil) and (Events.Count > 0);
end;

function TSilInterfacedObject.GetEventList: IEventList;
begin
  Result := IEventList(FEvents);
end;

function TSilInterfacedObject.GetConnectable: IConnectable;
begin
  DoCheckConnections;
  Result := FConnectable as IConnectable;
end;

procedure TSilInterfacedObject.DoCheckConnections;
begin
  if not Assigned(FConnectable) then
  begin
    FConnectable := TEventList.Create(Self);
    FEvents := Pointer(FConnectable as IEventList);
  end;
end;

function TSilInterfacedObject.DoRemoveCrossRef(const Obj: IInterface; out Item: PCrossReference): Boolean;
var
  Enum: IEnumerator;
begin
  while FCrossRefs.Enumerate(Enum, Item) do
    if Ref.SameObject(IUnknown(Item.Obj), Obj) then
    begin
      FCrossRefs.Delete(Enum.Iteration);
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TSilInterfacedObject.DoAddCrossRef(const Obj: IInterface; Ptr: PPointer): PCrossReference;
begin
  New(Result);
  Result.Field := Ptr;
  Result.Obj := Pointer(Obj);
  FCrossRefs.Add(Result);
end;

procedure TSilInterfacedObject.DoCheckCrossRefs;
begin
  if not Assigned(FCrossRefs) then
    FCrossRefs := TSilPointerList.Create(FLockable <> nil);
end;

procedure TSilInterfacedObject.DoDropAllCrossRefs;
var
  Enum: IEnumerator;
  Item: PCrossReference;
begin
  if Assigned(FCrossRefs) then
  begin
    FCrossRefs.Locked;

    while FCrossRefs.Enumerate(Enum, Item) do
      DropRef(@Item.Obj);

    FCrossRefs.Clear;
  end;
end;

end.
