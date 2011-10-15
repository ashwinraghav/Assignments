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

unit SilSmSharedList;

{$INCLUDE Defines.inc}

interface

uses
  Sil,

  SilLmInterfaceList,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  TSilSharedList = class(
    TSilInterfaceList,
    ISharedObjectList )
  private
    FManager: Pointer;
  private 
    function DoGetManager: ISharedObjectManager;
  protected  // ISharedObjectList 
    function Enumerate(var Enum: IEnumerator; out Item: ISharedObjectContainer): Boolean; reintroduce;
    function Find(const ID: ISharedID; out Container: ISharedObjectContainer): Boolean; overload;
    function Find(const Provider: ISharedObjectProvider; out Container: ISharedObjectContainer): Boolean; overload;
    function Add(const Provider: ISharedObjectProvider): ISharedObjectContainer; reintroduce; 
    function Remove(const Container: ISharedObjectContainer): Integer; reintroduce; overload; 
    function Remove(const Provider: ISharedObjectProvider): Integer; reintroduce; overload;  
  public 
    constructor Create(const Manager: ISharedObjectManager);
    destructor Destroy; override;
  public 
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation

uses
  SilSmSharedContainer;

{ TSilSharedList }

constructor TSilSharedList.Create(const Manager: ISharedObjectManager);
begin
  inherited Create(True);
  FManager := Pointer(Manager);
end;

destructor TSilSharedList.Destroy;
begin
  FManager := nil;
  inherited;
end;

function TSilSharedList.Enumerate(var Enum: IEnumerator; out Item: ISharedObjectContainer): Boolean;
begin
  Result := inherited Enumerate(Enum, Item);
end;

function TSilSharedList.Find(const ID: ISharedID; out Container: ISharedObjectContainer): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Container) do
    Result := Container.Provider.Provides(ID);
end;

function TSilSharedList.Find(const Provider: ISharedObjectProvider; out Container: ISharedObjectContainer): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  while not Result and Enumerate(Enum, Container) do
    Result := Sil.Text.Compare(Container.Provider.Module.FullName, Provider.Module.FullName) = 0; 
end;

function TSilSharedList.Add(const Provider: ISharedObjectProvider): ISharedObjectContainer; 
begin
  Result := TSilSharedContainer.Create(Provider);
  inherited Add(Result);
end;

function TSilSharedList.Remove(const Container: ISharedObjectContainer): Integer;
begin
  Result := inherited Remove(Container);
end;

function TSilSharedList.Remove(const Provider: ISharedObjectProvider): Integer;
var
  Container: ISharedObjectContainer;
begin
  if Find(Provider, Container) then
    Result := Remove(Container) else
    Result := -1;
end;

function TSilSharedList.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

end.
