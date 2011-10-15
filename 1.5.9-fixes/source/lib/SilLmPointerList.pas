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

unit SilLmPointerList;

{$I Defines.inc}

interface

uses
  SilLkList,
  SilLiPointerList,
  SilLmListEnumerator;

type
  TSilPointerList = class (
    // extends
    TSilList,
    // implements
    IPointerList)
  protected
    function GetItem(Index: Integer): Pointer;
    procedure SetItem(Index: Integer; Value: Pointer);
  protected // IPointerList
    function Add(Item: Pointer): Integer; virtual;
    procedure AddList(const Source: IPointerList);
    function IndexOf(Item: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer); virtual;
    function Remove(Item: Pointer): Integer; virtual;
    function First: Pointer;
    function Last: Pointer;
  end;

  TPointerList = class(TSilPointerList) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

{ TSilPointerList }

function TSilPointerList.Add(Item: Pointer): Integer;
begin
  Result := ItemAdd(Item^);
end;

function TSilPointerList.IndexOf(Item: Pointer): Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;

    for Result := 0 to Count - 1 do
      if GetItem(Result) = Item then
        Exit;

    Result := -1;
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

procedure TSilPointerList.Insert(Index: Integer; Item: Pointer);
begin
  ItemInsert(Index, Item^);
end;

function TSilPointerList.First: Pointer;
begin
  Result := GetItem(0);
end;

function TSilPointerList.GetItem(Index: Integer): Pointer;
begin
  Result := ItemPtr(Index);
end;

function TSilPointerList.Last: Pointer;
begin
  Result := GetItem(Count - 1);
end;

procedure TSilPointerList.SetItem(Index: Integer; Value: Pointer);
begin
  ItemSet(Index, Value^);
end;

procedure TSilPointerList.AddList(const Source: IPointerList);
var
  i: Integer;
begin
  try
    if Lockable <> nil then Lockable.Lock;

    for i := 0 to Source.Count - 1 do
      Add(Source.Items[i]);
  finally
    if Lockable <> nil then Lockable.Unlock;
  end;
end;

function TSilPointerList.Remove(Item: Pointer): Integer;
begin
  Result := ItemRemove(Item);
end;

end.
