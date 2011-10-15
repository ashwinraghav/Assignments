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

unit SilLkNamedItems;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilLkAbstractEnumerator,
  SilLiKey,
  SilLiLock,
  SilLiEnumerator;

type
  TSilNamedItems = class (
    TSilInterfacedObject,
    IEnumerable,
    INamedItems)
  protected // IEnumerable
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; 
  protected // INamedItems
    function Remove(const Name: String): Boolean; 
    function Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
  protected
    function DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; virtual; abstract;
    function DoRemove(const Name: String): Boolean; virtual; abstract;
  end;

  TSilNamedItemsEnumerator = class(TAbstractEnumerator)
  private
    FCurrent: String;
  protected // TAbstractEnumerator
    procedure DoReset; override;
    function GetCurrent: Pointer; override;
    function Next: Boolean; override;
    function Get(out Item): Boolean; override;
  protected
    function DoGetItem: String; virtual; abstract;
  protected
    property Current: String read FCurrent;
  end;

implementation

function TSilNamedItems.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := DoGetEnumerator(Enum, Locked);
end;

function TSilNamedItems.Enumerate(var Enum: IEnumerator; out Name: string): Boolean;
begin
  if Enum <> nil then
    Result := Enum.Next
  else if DoGetEnumerator(Enum, Self.HasLockable) then
    Result := Enum.HasMore
  else
    Result := False;

  if Result then
    Enum.Get(Name)
  else
    Enum := nil;
end;

function TSilNamedItems.Remove(const Name: String): Boolean;
begin
  Result := DoRemove(Name);
end;

{ TRegistryEnumerator }

procedure TSilNamedItemsEnumerator.DoReset;
begin
  FCurrent := DoGetItem;
end;

function TSilNamedItemsEnumerator.GetCurrent: Pointer;
begin
  Result := PChar(FCurrent);
end;

function TSilNamedItemsEnumerator.Get(out Item): Boolean;
begin
  Result := HasMore;
  if Result then String(Item) := FCurrent;
end;

function TSilNamedItemsEnumerator.Next: Boolean;
var
  sItem: String;
begin
  sItem := DoGetItem;

  if not HasMore or (sItem = FCurrent) then
    begin
      Inc(FIteration);
      Result := HasMore;
      if Result then
        FCurrent := DoGetItem else
        Detach;
    end
  else
    begin
      FCurrent := sItem;
      Result := true;
    end;
end;

end.
