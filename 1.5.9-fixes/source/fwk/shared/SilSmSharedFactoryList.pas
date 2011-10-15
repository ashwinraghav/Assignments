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

unit SilSmSharedFactoryList;

{$INCLUDE Defines.inc}

interface

uses
  SilLiEnumerator,
  SilLmInterfaceList,
  SilSiSharedObject;

type
  TSilSharedFactoryList = class(
    TSilInterfaceList,
    ISharedFactoryList,
    ISharedRegistry  )
  protected // ISharedFactoryList
    function Get(const ClassID: TGUID): ISharedFactory; overload;
    function Get(const ProgID: string): ISharedFactory; overload;
    function Find(const ClassID: TGUID; out Instance: ISharedFactory): Boolean; overload;
    function Find(const ProgID: string; out Instance: ISharedFactory): Boolean; overload;
    function Enumerate(var Enum: IEnumerator; out Item: ISharedFactory): Boolean; reintroduce; overload; 
    function Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean; reintroduce; overload; 
  protected // ISharedRegistry
    function Add(const Factory: ISharedFactory): Integer; reintroduce;
    function Remove(const Factory: ISharedFactory): Integer; reintroduce; 
  public
    constructor Create;
    destructor Destroy; override; 
  end;

implementation

uses
  Sil;

{ TSilSharedFactoryList }

constructor TSilSharedFactoryList.Create;
begin
  inherited Create(True);
end;

destructor TSilSharedFactoryList.Destroy;
begin
  inherited;
end;

function TSilSharedFactoryList.Enumerate(var Enum: IEnumerator; out Item: ISharedFactory): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilSharedFactoryList.Enumerate(var Enum: IEnumerator; const IID: TGUID; out Item): Boolean;
var
  Instance: IUnknown;
begin
  repeat
    Result := inherited Enumerate(Enum, Item);
  until not Result or (Instance.QueryInterface(IID, Item) = 0);
end;

function TSilSharedFactoryList.Find(const ClassID: TGUID; out Instance: ISharedFactory): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Instance) do
    Result := Sil.GUID.IsEqual(ClassID, Instance.ClassID);
end;

function TSilSharedFactoryList.Find(const ProgID: string; out Instance: ISharedFactory): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Instance) do
    Result := Sil.Text.Compare(ProgID, Instance.ProgID) = 0;
end;

function TSilSharedFactoryList.Get(const ClassID: TGUID): ISharedFactory;
begin
  if not Find(ClassID, Result) then
    raise Sil.Error.Create('No hay un factory para el ClassID: %s', [GUID.ToStr(ClassID)]);
end;

function TSilSharedFactoryList.Get(const ProgID: string): ISharedFactory;
begin
  if not Find(ProgID, Result) then
    raise Sil.Error.Create('No hay un factory para el ProgID: %s', [ProgID]);
end;

function TSilSharedFactoryList.Add(const Factory: ISharedFactory): Integer;
begin
  Result := inherited Add(Factory);
end;

function TSilSharedFactoryList.Remove(const Factory: ISharedFactory): Integer;
begin
  Result := inherited Remove(Factory);
end;

end.
