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

unit SilLmArray;

{$I Defines.inc}

interface

uses
  SilLiCompare,
  SilLiContainer,
  SilLiArray,
  SilLkInterfaced;

// implementaciones

type
  TArray = class(
    TSilInterfacedObject,
    IContainer,
    IArray )
  private
    FSize: Integer;
    FBuffer: PChar;
    FCapacity: Integer;
    FCount: Integer;
    FEvents: IContainerEvents;
  private
    //- utility methods
    procedure Grow(Delta: Integer);
    function CalcDelta(ACount: Integer): Integer;
    //- internal properties
    function GetElement(AIndex: Integer): Pointer;
    //- internal methods
    //procedure InternalDelete(FromIndex, ToIndex: Integer);
  protected // IContainer
    function Add(const Item): IPointer;
    function Insert(const Item; const Before: IPointer): IPointer; overload;
    procedure Remove(const Ptr: IPointer); overload;
    procedure Remove(const First, Last: IPointer); overload;
    procedure Clear;
    procedure Swap(const Ptr1, Ptr2: IPointer);
    procedure Move(const Ptr1, Ptr2: IPointer);
    function Lookup(const Comparator: IComparator; const Item): IPointer;
    function First: IPointer; // puntero al primer elemento
    function Last: IPointer;  // puntero al último elemento más uno
  protected // IArray
    function Get(AIndex: Integer): IPointer;
    procedure Put(AIndex: Integer; const Item);
    procedure Pack(const Comparator: IComparator; const Item);
    function Insert(const Item; Index: Integer = -1): IPointer; overload;
    procedure Delete(FromIndex, ToIndex: Integer);
    //- property accessors
    function GetCount: Integer;
    procedure SetCount(NewCount: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function GetBuffer: Pointer;
  public
    constructor Create(ItemSize: Integer);
    destructor Destroy; override;
    property Element[AIndex: Integer]: Pointer read GetElement; default;
  end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TArrayPointer declaration

type
  TArrayPointer = class(
    TSilInterfacedObject,
    IPointer )
  private
    FArray: TArray;
    FItem: Integer;
  private
    function Container: IContainer;
    function Pointer: IPointer;
    procedure Get(out Item); overload;
    function Get: Pointer; overload;
    function Equals(const Other: IPointer): Boolean;
    function Has: Boolean;
    function Next: Boolean;
    function Clone: IPointer;
  public
    constructor Create(AOwner: TArray; AItem: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  SilAfLockedIncrement,
  SilLtReference;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TArray

constructor TArray.Create(ItemSize: Integer);
begin
  inherited Create;
  FSize := ItemSize;
end;

destructor TArray.Destroy;
begin
  Clear;
  inherited;
end;

function TArray.Add(const Item): IPointer;
  var Ptr: TArrayPointer;
begin
  Ptr := TArrayPointer.Create(Self, FCount);
  if Ptr.FItem = FCapacity then Grow(CalcDelta(Ptr.FItem));
  System.Move(Item, Element[Ptr.FItem]^, FSize);
  Inc(FCount);
  Result := Ptr;
  if Assigned(FEvents) then FEvents.OnAddition(Result);
end;

function TArray.Insert(const Item; const Before: IPointer): IPointer;
begin
end;
    
procedure TArray.Remove(const Ptr: IPointer);
begin
  Remove(Ptr, Ptr);
end;

procedure TArray.Remove(const First, Last: IPointer);
  var FirstPtr, LastPtr: TArrayPointer; 
begin
  FirstPtr := Reference.GetInstance(First);
  LastPtr := Reference.GetInstance(Last);
  Delete(FirstPtr.FItem, LastPtr.FItem);
end;

procedure TArray.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TArray.Swap(const Ptr1, Ptr2: IPointer);
begin
end;

procedure TArray.Move(const Ptr1, Ptr2: IPointer);
begin
end;

function TArray.Lookup(const Comparator: IComparator; const Item): IPointer;
var
   I: Integer;
begin
  for I := 0 to FCount - 1 do
  begin
    if Comparator.Compare(Item, Element[I]^) = 0 then
    begin
      Result := TArrayPointer.Create(Self, I);
      Exit;
    end;
  end;
  Result := nil;
end;

function TArray.First: IPointer;
begin
  Result := TArrayPointer.Create(Self, 0);
end;

function TArray.Last: IPointer;  
begin
  Result := TArrayPointer.Create(Self, FCount);
end;

function TArray.Get(AIndex: Integer): IPointer;
begin
  Result := TArrayPointer.Create(Self, AIndex);
end;

procedure TArray.Put(AIndex: Integer; const Item);
begin
  System.Move(Item, Element[AIndex]^, FSize);
end;

procedure TArray.Pack(const Comparator: IComparator; const Item);
begin
end;

function TArray.Insert(const Item; Index: Integer = -1): IPointer;
begin
end;

procedure TArray.Delete(FromIndex, ToIndex: Integer);
begin
  if Assigned(FEvents) then FEvents.OnDeletion(Get(FromIndex), Get(ToIndex));

  
end;

function TArray.GetCount: Integer;
begin
  Result := FCount;
end;

procedure TArray.SetCount(NewCount: Integer);
begin
  if NewCount <> FCount then
  begin
    if NewCount > FCapacity then SetCapacity(NewCount);
    if NewCount > FCount then
      FillChar(FBuffer[FCount * FSize], (NewCount - FCount) * FSize, 0) else
      Delete(NewCount, FCount - 1);
    FCount := NewCount;
  end;
end;

function TArray.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

procedure TArray.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FBuffer, NewCapacity * FSize);
    FCapacity := NewCapacity;
  end;
end;

function TArray.GetBuffer: Pointer;
begin
  Result := FBuffer;
end;

procedure TArray.Grow(Delta: Integer);
begin
  SetCapacity(FCapacity + Delta); 
end;

function TArray.CalcDelta(ACount: Integer): Integer;
  var Sink: IArrayEvents;
begin
  if Assigned(FEvents) and Reference.GetInterface(FEvents, IArrayEvents, Sink) then
    Result := Sink.OnCalcDelta(ACount) else
    Result := ACount + 1;
end;

function TArray.GetElement(AIndex: Integer): Pointer;
begin
  Result := @ FBuffer[ AIndex * FSize ];
end;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// TArrayPointer implementation


constructor TArrayPointer.Create(AOwner: TArray; AItem: Integer);
begin
  inherited Create;
  FArray := AOwner;
  FItem := AItem;
end;

destructor TArrayPointer.Destroy;
begin
  inherited;
end;

function TArrayPointer.Container: IContainer;
begin
  Result := FArray;
end;

function TArrayPointer.Pointer: IPointer;
begin
  Result := Self;
end;

procedure TArrayPointer.Get(out Item);
begin
  System.Move(FArray[FItem]^, Item, FArray.FSize);
end;

function TArrayPointer.Get: Pointer;
begin
  Result := FArray[FItem];
end;

function TArrayPointer.Equals(const Other: IPointer): Boolean;
  var OtherPtr: TArrayPointer;
begin
  OtherPtr := Reference.GetInstance(Other);
  Result := (OtherPtr.FArray = FArray) and (OtherPtr.FItem = FItem);
end;

function TArrayPointer.Has: Boolean;
begin
  Result := FItem < FArray.FCount;
end;

function TArrayPointer.Next: Boolean;
begin
  LockedInc(FItem);
  Result := Has;
end;

function TArrayPointer.Clone: IPointer;
begin
  Result := TArrayPointer.Create(FArray, FItem);
end;


end.
