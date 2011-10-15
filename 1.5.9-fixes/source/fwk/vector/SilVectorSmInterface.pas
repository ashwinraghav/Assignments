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

unit SilVectorSmInterface;

interface

{$include Sil.inc}

uses
  Sil,
  SilVectorSiVector,
  SilVectorSkVector;

type
  TSilVectorInterface = class (TSilVector, IVectorInterface)
  protected
    procedure DisposeItem(Index: Integer); override;
  protected // IVectorPointer
    function GetItem(Index: integer): IInterface;
    procedure SetItem(Index: integer; const Value: IInterface);
    function Add(const Item: IInterface): integer;
    function Remove(const Value: IInterface): integer;
    procedure Insert(Index: integer; const Value: IInterface);
    function IndexOf(const Value: IInterface): integer;
    function First: IInterface;
    function Last: IInterface;
    function Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorInterfaceEnumerator;
  end;

implementation

uses
  SilVectorSmInterfaceEnumerator;

{ TSilVectorInterface }

function TSilVectorInterface.Add(const Item: IInterface): integer;
var
  ptr: pointer;
begin
  Result := ItemAdd;
  ptr := nil;
  IInterface(ptr) := item;
  FList^[Result] := ptr;
end;

function TSilVectorInterface.Remove(const Value: IInterface): integer;
begin
  Result := IndexOf(value);
  if Result >= 0 then
    Delete(Result);
end;

function TSilVectorInterface.Enumerator(Step: integer = 1; IndexBegin: integer = 0; IndexEnd: integer = -1): IVectorInterfaceEnumerator;
begin
  Result := TSilVectorInterfaceEnumerator.Create(self, step, indexbegin, indexend);
end;

function TSilVectorInterface.IndexOf(const Value: IInterface): integer;
var
  i: integer;
begin
  for i := 0 to fcount - 1 do
    if Sil.Ref.SameObject(value, IInterface(flist^[i])) then
    begin
      Result := i;
      exit;
    end;

  Result := -1;
end;

function TSilVectorInterface.First: IInterface;
begin
  if (FCount < 1) then
    IndexError(0);

  Result := GetItem(0);
end;

function TSilVectorInterface.GetItem(Index: integer): IInterface;
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  Result := IInterface(FList^[Index]);
end;

procedure TSilVectorInterface.Insert(Index: integer; const Value: IInterface);
begin
  ItemInsert(index);
  IInterface(FList^[Index]) := value;
end;

function TSilVectorInterface.Last: IInterface;
begin
  Result := GetItem(FCount - 1);
end;

procedure TSilVectorInterface.SetItem(Index: integer; const Value: IInterface);
begin
  if (Index < 0) or (Index >= FCount) then
    IndexError(Index);

  IInterface(FList^[Index]) := value;
end;

procedure TSilVectorInterface.DisposeItem(Index: Integer);
begin
  IInterface(FList^[Index]) := nil;
end;

end.
