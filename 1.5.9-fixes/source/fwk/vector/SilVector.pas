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

unit SilVector;

interface

{$include Sil.inc}

(*)
  all:
    streaming
(*)

uses
  Sil,
  SilVectorSiVector;

type
  IVector                     = SilVectorSiVector.IVector;
  IVectorEnumerator           = SilVectorSiVector.IVectorEnumerator;

  IVectorPointer              = SilVectorSiVector.IVectorPointer;
  IVectorPointerEnumerator    = SilVectorSiVector.IVectorPointerEnumerator;

  IVectorString               = SilVectorSiVector.IVectorString;
  IVectorStringEnumerator     = SilVectorSiVector.IVectorStringEnumerator;

  IVectorInterface            = SilVectorSiVector.IVectorInterface;
  IVectorInterfaceEnumerator  = SilVectorSiVector.IVectorInterfaceEnumerator;

{ lists }

function PointerList(locked: boolean = false): IVectorPointer;
function StringList(locked: boolean = false): IVectorString;
function InterfaceList(locked: boolean = false): IVectorInterface;
function ParameterList(locked: boolean = false): IParameterList;

function Enumerate(var enum: IEnumerator; const vector: IVectorString; out value: string): boolean; overload;
function Enumerate(var enum: IEnumerator; const vector: IVectorInterface; out value): boolean; overload;
function Enumerate(var enum: IEnumerator; const vector: IVectorPointer; out value): boolean; overload;

{ tools }

implementation

uses
  SilVectorSmPointer,
  SilVectorSmInterface,
  SilVectorSmString,
  SilVectorSmParameters,

  SilVectorSmStringEnumerator,
  SilVectorSmPointerEnumerator,
  SilVectorSmInterfaceEnumerator;

function PointerList(locked: boolean): IVectorPointer;
begin
  Result := TSilVectorPointer.Create(locked);
end;

function StringList(locked: boolean): IVectorString;
begin
  Result := TSilVectorString.Create(locked);
end;

function InterfaceList(locked: boolean): IVectorInterface;
begin
  Result := TSilVectorInterface.Create(locked);
end;

function ParameterList(locked: boolean): IParameterList;
begin
  Result := TSilVectorParameter.Create(locked);
end;

function Enumerate(var enum: IEnumerator; const vector: IVectorString; out value: string): boolean;
begin
  if enum <> nil then
    Result := enum.Next
  else if vector.count > 0 then
  begin
    Enum := TSilVectorStringEnumerator.Create(vector, 1, 0, vector.count - 1);
    Result := enum.HasMore;
  end else
    Result := false;

  if Result then
    enum.Get(value)
  else
    enum := nil;
end;

function Enumerate(var enum: IEnumerator; const vector: IVectorInterface; out value): boolean;
begin
  if enum <> nil then
    Result := enum.Next
  else if vector.count > 0 then
  begin
    Enum := TSilVectorInterfaceEnumerator.Create(vector, 1, 0, vector.count - 1);
    Result := enum.HasMore;
  end else
    Result := false;

  if Result then
    enum.Get(value)
  else
    enum := nil;
end;

function Enumerate(var enum: IEnumerator; const vector: IVectorPointer; out value): boolean;
begin
  if enum <> nil then
    Result := enum.Next
  else if vector.count > 0 then
  begin
    Enum := TSilVectorPointerEnumerator.Create(vector, 1, 0, vector.count - 1);
    Result := enum.HasMore;
  end else
    Result := false;

  if Result then
    enum.Get(value)
  else
    enum := nil;
end;

end.
