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

unit SilOmHandleList;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,


  SilLiEnumerator,
  SilOiHandle,

  SilLmPointerList;

type
  TSilHandleList = class(TSilPointerList, IHandleList)
  protected
    {IEnumerable}
    function GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; override;
    {IHandleList}
    function First: IHandle;
    function Last: IHandle;
    function GetItem(Index: Integer): IHandle;
    procedure SetItem(Index: Integer; const Value: IHandle);
    procedure AddList(const Source: IHandleList);
  end;

implementation

uses
  SilBtInterfacePtr,
  SilLmListEnumerator;

{ TSilHandleList }

procedure TSilHandleList.AddList(const Source: IHandleList);
var
  i: Integer;
begin
  for i := 0 to Source.Count - 1 do Add(Pointer(Source.Items[i]));
end;

function TSilHandleList.First: IHandle;
begin
  Result := GetItem(0);
end;

function TSilHandleList.GetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
begin
  Result := Count > 0;
  if Result then Enum := TListEnumerator.Create(Self, Locked, InterfaceHandler);
end;

function TSilHandleList.GetItem(Index: Integer): IHandle;
begin
  Result := IHandle(inherited GetItem(Index));
end;

function TSilHandleList.Last: IHandle;
begin
  Result := GetItem(Count - 1);
end;

procedure TSilHandleList.SetItem(Index: Integer; const Value: IHandle);
begin
  inherited SetItem(Index, Pointer(Value));
end;

end.
 