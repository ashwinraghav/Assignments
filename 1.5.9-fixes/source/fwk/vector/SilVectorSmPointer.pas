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

unit SilVectorSmPointer;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector,
  SilVectorSkVector;

type
  TSilVectorPointer = class (TSilVector, IVectorPointer)
  protected // IVectorPointer
    function GetItem(Index: integer): pointer;
    procedure SetItem(Index: integer; Value: pointer);
    function Add(Item: Pointer): integer;
    function Remove(Value: pointer): integer;
    procedure Insert(Index: integer; Value: pointer);
    function IndexOf(item: pointer): integer;
    function First: pointer;
    function Last: pointer;
    function Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorPointerEnumerator;
  end;

implementation

uses
  SilVectorSmPointerEnumerator;

{ TSilVectorPointer }

function TSilVectorPointer.Add(Item: Pointer): integer;
begin
  Result := ItemAdd;
  FList^[Result] := item;
end;

function TSilVectorPointer.Remove(Value: pointer): integer;
begin
  Result := IndexOf(value);
  if Result >= 0 then
    Delete(Result);
end;

function TSilVectorPointer.Enumerator(Step, IndexBegin, IndexEnd: integer): IVectorPointerEnumerator;
begin
  Result := TSilVectorPointerEnumerator.Create(self, step, indexbegin, indexend);
end;

function TSilVectorPointer.IndexOf(item: pointer): integer;
var
  i: integer;
begin
  for i := 0 to fcount - 1 do
    if item = flist^[i] then
    begin
      Result := i;
      exit;
    end;

  Result := -1;
end;

function TSilVectorPointer.First: pointer;
begin
  if (FCount < 1) then
    IndexError(0);

  Result := GetItem(0);
end;

function TSilVectorPointer.GetItem(Index: integer): pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  Result := FList^[Index];
end;

procedure TSilVectorPointer.Insert(Index: integer; Value: pointer);
begin
  ItemInsert(index);
  FList^[Index] := Value;
end;

function TSilVectorPointer.Last: pointer;
begin
  Result := GetItem(FCount - 1);
end;

procedure TSilVectorPointer.SetItem(Index: integer; Value: pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  FList^[Index] := Value;
end;

end.
