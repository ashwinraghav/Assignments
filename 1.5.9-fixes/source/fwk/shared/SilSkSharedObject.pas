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

unit SilSkSharedObject;

{$INCLUDE Defines.inc}

interface

uses
  SilLkAggregated,
  SilOiModule,
  SilSiSharedObject;

type
  TSilSharedObjectClass = class of TSilSharedObject;

  TSilSharedObject = class(
    TSilAggregatedObject,
    ISharedObject,
    ISharedProgID )
  private
    FFactory: ISharedFactory;
    FOwner: Pointer;
  protected // ISharedObject
    function GetOwner: IUnknown;
    function GetClassID: TGUID; virtual;
    function GetModule: IModule2; virtual;
  protected // ISharedProgID
    function GetProgID: string; virtual;
  public
    constructor CreateShared(const Factory: ISharedFactory; const Owner: IUnknown = nil; const Controller: IUnknown = nil; Param: Pointer = nil); virtual;
    destructor Destroy; override;
  public
    property Owner: IUnknown read GetOwner;
    property ClassID: TGUID read GetClassID;
    property Module: IModule2 read GetModule;
  end;

implementation

uses
  Sil;

{ TSilSharedObject }

constructor TSilSharedObject.CreateShared(const Factory: ISharedFactory; const Owner, Controller: IInterface; Param: Pointer);
begin
  inherited Create(Controller);
  MakeRef(Owner, @FOwner);
  FFactory := Factory;
end;

destructor TSilSharedObject.Destroy;
begin
  FFactory := nil;
  FOwner := nil;
  inherited;
end;

function TSilSharedObject.GetOwner: IUnknown;
begin
  Result := IUnknown(FOwner);
end;

function TSilSharedObject.GetClassID: TGUID;
begin
  if Assigned(FFactory) then
    Result := FFactory.ClassID else
    Result := Sil.GUID.Null;
end;

function TSilSharedObject.GetModule: IModule2;
begin
  if Assigned(FFactory) then
    Result := FFactory.Module else
    Result := Sil.Os.Module.Get(ClassType) as IModule2;
end;

function TSilSharedObject.GetProgID: string;
begin
  if Assigned(FFactory) then
    Result := FFactory.ProgID else
    Result := '(' + Sil.Os.Filesystem.ChangeFileExt(Self.Module.Info.Name, '') + '.' + Self.ClassName + ')';
end;

end.
