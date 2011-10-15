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

unit SilLmContainerInterfaces;

{$INCLUDE Defines.inc}
{$STACKFRAMES OFF}
{$OPTIMIZATION ON}

interface

uses
  SilLiEnumerator,
  SilLiLock,
  SilLiParameters,
  SilLiInterfaceList,
  SilLiContainerTypes,
  SilLiContainer,
  SilLiContainerVector,
  SilLkContainer;

type
  TSilInterfaceListContainer = class (
    TSilContainer,
    IEnumerable,
    IInterfaceList)
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
  protected // IInterfaceList
    function Add(const Item: IUnknown): Integer;
    procedure AddList(const Source: IInterfaceList);
    function IndexOf(const Item: IUnknown): Integer;
    procedure Insert(Index: Integer; const Item: IUnknown);
    function Remove(const Item: IUnknown): Integer;
    function First: IUnknown;
    function Last: IUnknown;
    function GetItem(Index: Integer): IUnknown;
    procedure SetItem(Index: Integer; const Value: IUnknown);
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

{ TSilInterfaceListContainer }

constructor TSilInterfaceListContainer.Create(AClass: TSilBaseContainerClass; Locked: Boolean; const Comparer: ITypeComparator);
var
  Options: IParameterList;
begin
  Options := ListTool.Parameters;
  Options['Locked'] := Locked;
  inherited Create(AClass, SilLtContainer.Handler.Create(TypeInfo(IUnknown), TSilInterfaceComparator.Create(Comparer)), Options);
end;

destructor TSilInterfaceListContainer.Destroy;
begin
  inherited;
end;

function TSilInterfaceListContainer.GetIsLockable: Boolean;
begin
  Result := Base.Locking.IsLockable;
end;

function TSilInterfaceListContainer.GetLockable: ILockable;
begin
  Result := Base.Locking.Lockable;
end;

function TSilInterfaceListContainer.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := GetCount > 0;
  if Result then Enum := TSilContainerEnumerator.Create(Self, Sequence.Create(Self), Locked);
end;

function TSilInterfaceListContainer.Enumerate(var Enum: IEnumerator; out Item): Boolean;
var
  Data: PUnknown;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if GetEnumerator(Enum, IsLockable) then
    Result := Enum.HasMore
  else
    Result := False;

  if not Result then
    Enum := nil
  else if Enum.Get(Data) then
    IUnknown(Item) := Data^;
end;

procedure TSilInterfaceListContainer.SetCount(Value: Integer);
begin
  inherited SetCount(Value);
end;

function TSilInterfaceListContainer.GetCapacity: Integer;
var
  Vector: IBaseVector;
begin
  if Ref.GetInterface(Base, IBaseVector, Vector) then
    Result := Vector.Capacity else
    Result := GetCount;
end;

procedure TSilInterfaceListContainer.SetCapacity(NewCapacity: Integer);
var
  Vector: IBaseVector;
begin
  if Ref.GetInterface(Base, IBaseVector, Vector) then
    Vector.Capacity := NewCapacity;
end;

function TSilInterfaceListContainer.ItemPtr(Index: Integer): Pointer;
begin
  Result := Pointer(HandleGet(Index));
end;

procedure TSilInterfaceListContainer.Delete(Index: Integer);
begin
  inherited Delete(Index, Index);
end;

function TSilInterfaceListContainer.HasLockable: Boolean;
begin
  Result := IsLockable;
end;

procedure TSilInterfaceListContainer.Exchange(Index1, Index2: Integer);
begin
  inherited Exchange(Index1, Index2);
end;

function TSilInterfaceListContainer.ValidIndex(Index: Integer): Boolean;
begin
  Result := inherited IsValid(Index);
end;

procedure TSilInterfaceListContainer.CheckIndex(Index: Integer);
begin
  inherited Check(Index);
end;

function TSilInterfaceListContainer.Add(const Item: IInterface): Integer;
begin
  Result := inherited Add(@Item);
end;

procedure TSilInterfaceListContainer.AddList(const Source: IInterfaceList);
var
  i: Integer;
begin
  Locked;
  if Source.HasLockable then Source.Locked;

  for i := 0 to Source.Count - 1 do
    Add(Source[i]);
end;

function TSilInterfaceListContainer.IndexOf(const Item: IInterface): Integer;
var
  Return: HItem absolute Result;
begin
  if not inherited Find(@Item, Return) then
    Result := -1;
end;

procedure TSilInterfaceListContainer.Insert(Index: Integer; const Item: IInterface);
begin
  inherited Insert(Index, @Item);
end;

function TSilInterfaceListContainer.Remove(const Item: IInterface): Integer;
begin
  Result := inherited Remove(@Item, nil);
end;

function TSilInterfaceListContainer.First: IUnknown;
begin
  Result := GetItem(0);
end;

function TSilInterfaceListContainer.Last: IUnknown;
begin
  Result := GetItem(GetCount - 1);
end;

function TSilInterfaceListContainer.GetItem(Index: Integer): IUnknown;
begin
  Result := PUnknown(HandleGet(Index))^;
end;

procedure TSilInterfaceListContainer.SetItem(Index: Integer; const Value: IInterface);
begin
  inherited HandlePut(Index, @Value);
end;

end.
