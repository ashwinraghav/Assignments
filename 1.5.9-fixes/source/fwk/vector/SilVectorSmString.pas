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

unit SilVectorSmString;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector,
  SilVectorSkVector;

type
  PStringItem = ^RStringItem;
  RStringItem = record
    Buffer: String;
    Ptr: Pointer;
  end;

  TSilVectorString = class (TSilVector, IVectorString, IVectorComparator, IVectorComparable)
  protected
    fIgnoreCase: boolean;
    procedure DisposeItem(Index: Integer); override;
  protected // IVectorPointer
    function GetIgnoreCase: boolean;
    procedure SetIgnoreCase(Value: boolean);
    function GetItem(Index: integer): string;
    procedure SetItem(Index: integer; const Value: string);
    function GetPtr(Index: integer): pointer;
    procedure SetPtr(Index: integer; Value: pointer);
    function Add(const Value: string; Ptr: pointer = nil): integer;
    function Remove(const Value: string): integer;
    procedure Insert(Index: integer; const Value: string; Ptr: pointer = nil);
    function IndexOf(const Value: string): integer;
    function IndexOfPtr(Value: pointer): integer;
    function First: string;
    function Last: string;
    function Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorStringEnumerator;
  protected // IVectorComparator
    function CompareItems(const Vector: IVector; index1, index2: integer): integer;
  protected // IVectorComparable
    function CompareValue(const Vector: IVector; index: integer; const value): integer;
  end;

implementation

uses
  SilVectorSmStringEnumerator;

{ TSilVectorString }

function TSilVectorString.Add(const Value: string; Ptr: pointer): integer;
var
  item: PStringItem;
begin
  new(item);
  item.Buffer := value;
  item.Ptr := ptr;

  Result := ItemAdd;
  FList^[Result] := item;
end;

function TSilVectorString.Remove(const Value: string): integer;
begin
  Result := IndexOf(value);
  if Result >= 0 then
    Delete(Result);
end;

function TSilVectorString.Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorStringEnumerator;
begin
  Result := TSilVectorStringEnumerator.Create(self, step, indexbegin, indexend);
end;

function TSilVectorString.IndexOf(const Value: string): integer;
var
  i: integer;
  item: PStringItem;
begin
  for i := 0 to fcount - 1 do
  begin
    item := PStringItem(flist^[i]);

    if (not fIgnoreCase and (value = item.Buffer)) or
      (fIgnoreCase and Sil.Text.IsEqual(value, item.Buffer)) then
    begin
      Result := i;
      exit;
    end;
  end;

  Result := -1;
end;

function TSilVectorString.IndexOfPtr(Value: pointer): integer;
var
  i: integer;
  item: PStringItem;
begin
  for i := 0 to fcount - 1 do
  begin
    item := PStringItem(flist^[i]);

    if value = item.Ptr then
    begin
      Result := i;
      exit;
    end;
  end;

  Result := -1;
end;

function TSilVectorString.First: string;
begin
  if (FCount < 1) then
    IndexError(0);

  Result := GetItem(0);
end;

function TSilVectorString.GetItem(Index: integer): string;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  Result := PStringItem(FList^[Index]).Buffer;
end;

procedure TSilVectorString.Insert(Index: integer; const Value: string; Ptr: pointer);
var
  item: PStringItem;
begin
  new(item);
  item.Buffer := value;
  item.Ptr := ptr;

  ItemInsert(index);
  FList^[Index] := item;
end;

function TSilVectorString.Last: string;
begin
  Result := GetItem(FCount - 1);
end;

procedure TSilVectorString.SetItem(Index: integer; const Value: string);
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  PStringItem(FList^[Index]).Buffer := value;
end;

procedure TSilVectorString.DisposeItem(Index: Integer);
var
  item: PStringItem;
begin
  item := FList^[Index];
  dispose(item);
end;

function TSilVectorString.GetPtr(Index: integer): pointer;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  Result := PStringItem(FList^[Index]).Ptr;
end;

procedure TSilVectorString.SetPtr(Index: integer; Value: pointer);
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  PStringItem(FList^[Index]).Ptr := value;
end;

function TSilVectorString.GetIgnoreCase: boolean;
begin
  Result := fIgnoreCase;
end;

procedure TSilVectorString.SetIgnoreCase(Value: boolean);
begin
  fIgnoreCase := value;
end;

function TSilVectorString.CompareItems(const Vector: IVector; index1, index2: integer): integer;
begin
  if fignorecase then
    result := Sil.Text.Compare(GetItem(index1), GetItem(index2))
  else
    result := Sil.Str.Compare(GetItem(index1), GetItem(index2));
end;

function TSilVectorString.CompareValue(const Vector: IVector; index: integer; const value): integer;
begin
  if fignorecase then
    result := Sil.Text.Compare(GetItem(index), string(value))
  else
    result := Sil.Str.Compare(GetItem(index), string(value));
end;

end.
