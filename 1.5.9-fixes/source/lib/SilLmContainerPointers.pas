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

unit SilLmContainerPointers;

{$INCLUDE Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilLiEnumerator,
  SilLiLock,
  SilLiParameters,
  SilLiList,
  SilLiPointerList,
  SilLiContainerTypes,
  SilLiContainer,
  SilLiContainerVector,
  SilLkContainer;

type
  TSilContainerPointers = class (
    TSilContainer,
    IEnumerable,
    IPointerList )
  protected
    function GetIsLockable: Boolean; override;
    function GetLockable: ILockable; override;
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
  protected // IItemization
    function Enumerate(var Enum: IEnumerator; out Item): Boolean;
  protected // IList
    procedure SetCount(Value: Integer);
    function GetCapacity: Integer;
    procedure SetCapacity(NewCapacity: Integer);
    function ItemPtr(Index: Integer): Pointer;
    procedure Delete(Index: Integer);
    function HasLockable: Boolean;
    procedure Exchange(Index1, Index2: Integer);
    function ValidIndex(Index: Integer): Boolean;
    procedure CheckIndex(Index: Integer);
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
  public
    constructor Create(AClass: TSilBaseContainerClass; Locked: Boolean; const Comparer: ITypeComparator = nil);
    destructor Destroy; override; 
  end;

implementation

uses
  SilLtReference,
  SilLtList,
  SilLtContainer,
  SilLmContainerEnumerator,
  SilLmContainerTypes;

{ TSilContainerPointers }

constructor TSilContainerPointers.Create(AClass: TSilBaseContainerClass; Locked: Boolean; const Comparer: ITypeComparator);
var
  Options: IParameterList;
begin
  Options := ListTool.Parameters;
  Options['Locked'] := Locked;
  inherited Create(AClass, SilLtContainer.Handler.Create(SizeOf(Pointer), TSilPointerComparator.Create(Comparer)), Options);
end;

destructor TSilContainerPointers.Destroy;
begin
  inherited;
end;

function TSilContainerPointers.GetIsLockable: Boolean;
begin
  Result := Base.Locking.IsLockable;
end;

function TSilContainerPointers.GetLockable: ILockable;
begin
  Result := Base.Locking.Lockable;
end;

function TSilContainerPointers.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := GetCount > 0;
  if Result then Enum := TSilContainerEnumerator.Create(Self, Sequence.Create(Self), Locked);
end;

function TSilContainerPointers.Enumerate(var Enum: IEnumerator; out Item): Boolean;
var
  Data: PPointer;
begin
  if not Assigned(Enum) then
    Result := Sequence.Start(Self, Enum, HData(Data), IsLockable) else
    Result := Sequence.Next(Enum, HData(Data));
    
  if Result then
    Pointer(Item) := Data^ else
    Enum := nil;
end;

procedure TSilContainerPointers.SetCount(Value: Integer);
begin
  inherited SetCount(Value);
end;

function TSilContainerPointers.GetCapacity: Integer;
var
  Vector: IBaseVector;
begin
  if Ref.Get(Base, IBaseVector, Vector) then
    Result := Vector.Capacity else
    Result := GetCount;
end;

procedure TSilContainerPointers.SetCapacity(NewCapacity: Integer);
var
  Vector: IBaseVector;
begin
  if Ref.Get(Base, IBaseVector, Vector) then
    Vector.Capacity := NewCapacity;
end;

function TSilContainerPointers.ItemPtr(Index: Integer): Pointer;
begin
  Result := Pointer(HandleGet(Index));
end;

procedure TSilContainerPointers.Delete(Index: Integer);
begin
  inherited Delete(Index, Index);
end;

function TSilContainerPointers.HasLockable: Boolean;
begin
  Result := IsLockable;
end;

procedure TSilContainerPointers.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
end;

function TSilContainerPointers.ValidIndex(Index: Integer): Boolean;
begin
  Result := inherited IsValid(Index);
end;

procedure TSilContainerPointers.CheckIndex(Index: Integer);
begin
  inherited Check(Index);
end;

function TSilContainerPointers.Add(Item: Pointer): Integer;
begin
  Result := inherited Add(@Item);
end;

procedure TSilContainerPointers.AddList(const Source: IPointerList);
var
  i: Integer;
begin
  Locked;
  if Source.HasLockable then Source.Locked;

  for i := 0 to Source.Count - 1 do
    Add(Source[i]);
end;

function TSilContainerPointers.IndexOf(Item: Pointer): Integer;
var
  Return: HItem absolute Result;
begin
  if not inherited Find(@Item, Return) then
    Result := -1;
end;

procedure TSilContainerPointers.Insert(Index: Integer; Item: Pointer);
begin
  inherited Insert(Index, @Item);
end;

function TSilContainerPointers.Remove(Item: Pointer): Integer;
begin
  Result := inherited Remove(@Item, nil);
end;

function TSilContainerPointers.First: Pointer;
begin
  Result := GetItem(GetFirstItem());
end;

function TSilContainerPointers.Last: Pointer;
begin
  Result := GetItem(GetLastItem());
end;

function TSilContainerPointers.GetItem(Index: Integer): Pointer;
begin
  Result := Pointer(HandleGet(Index)^);
end;

procedure TSilContainerPointers.SetItem(Index: Integer; Value: Pointer);
begin
  inherited HandlePut(Index, @Value);
end;

end.
