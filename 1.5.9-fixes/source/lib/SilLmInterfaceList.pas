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

unit SilLmInterfaceList;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilLiReference,
  SilLiEnumerator,
  SilLiInterfaceList,
  SilLiList,
  SilLkList,
  SilLmListEnumerator;

type

{ TInterfaceList }

  TSilInterfaceList = class (
    // extends
    TSilList,
    // implements
    IUnknown,
    IInterfaceList)
  protected // IInterfaceList
    function Add(const Item: IUnknown): Integer; virtual;
    procedure AddList(const Source: IInterfaceList); virtual;
    function IndexOf(const Item: IUnknown): Integer; virtual;
    procedure Insert(Index: Integer; const Item: IUnknown); virtual;
    function Remove(const Item: IUnknown): Integer; virtual;
    function First: IUnknown;
    function Last: IUnknown;
    function GetItem(Index: Integer): IUnknown;
    procedure SetItem(Index: Integer; const Value: IUnknown);
  public
    constructor Create(Locked: Boolean = false; TypeData: Pointer = nil); reintroduce;
  end;

  TInterfaceList = class(TSilInterfaceList) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

  
implementation

uses
  SilLdContainer,
  SilBtInterfacePtr,
  SilLtReference;

{ TSilInterfaceList }

constructor TSilInterfaceList.Create(Locked: Boolean; TypeData: Pointer);
begin
  inherited Create(Locked, InterfaceHandler, TypeData);
end;

function TSilInterfaceList.Add(const Item: IUnknown): Integer;
begin
  Result := ItemAdd(Item);
end;

function TSilInterfaceList.First: IUnknown;
begin
  Result := GetItem(0);
end;

function TSilInterfaceList.GetItem(Index: Integer): IUnknown;
begin
  ItemGet(Index, Result);
end;

function TSilInterfaceList.IndexOf(const Item: IUnknown): Integer;
var
  Other: IUnknown;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    for Result := 0 to Count - 1 do
    begin
      Other := GetItem(Result);
      if (Pointer(Item) = Pointer(Other)) or Reference.SameObject(Other, Item) then
        Exit;
    end;
    Result := -1;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilInterfaceList.Insert(Index: Integer; const Item: IUnknown);
begin
  ItemInsert(Index, Item);
end;

function TSilInterfaceList.Last: IUnknown;
begin
  Result := GetItem(Count - 1);
end;

procedure TSilInterfaceList.SetItem(Index: Integer; const Value: IUnknown);
begin
  ItemSet(Index, Value);
end;

function TSilInterfaceList.Remove(const Item: IUnknown): Integer;
begin
  Result := ItemRemove(Item);
end;

procedure TSilInterfaceList.AddList(const Source: IInterfaceList);
var
  i: Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;
    Source.Locked;

    for i := 0 to Source.Count - 1 do
      Add(Source.Items[i]);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

end.
