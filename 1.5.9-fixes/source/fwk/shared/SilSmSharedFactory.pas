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

unit SilSmSharedFactory;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  ESilFactoryError = class(ESilSharedObjectError);

  TSilSharedFactory = class(
    TSilObject,
    ISharedObjectFactory )
  private
    FManager: Pointer;
  private
    function DoGetManager: ISharedObjectManager;
    function DoCreateInstance(const Provider: ISharedObjectProvider; const ID: ISharedID; Param: Pointer; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function DoCreateInstance(const Factory: ISharedFactory; Param: Pointer; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function DoGetProvider(const ID: ISharedID): ISharedObjectProvider;
    function DoGetContainer(const Provider: ISharedObjectProvider): ISharedObjectContainer; overload;
    function DoFindLocal(const ID: ISharedID; out Factory: ISharedFactory): Boolean;
    function DoFindReference(Ptr: Pointer; out Container: ISharedObjectContainer): Boolean;
    procedure DoReleaseUnknown(var Obj: IUnknown);
    procedure DoReleasePtr(const Ptr: Pointer);
  protected // ISharedObjectFactory
    function CreateInstance(const ClassID: TGuid; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const FileName: String; const ClassID: TGuid; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const FileName: String; const ProgID: string; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const ProgID: string; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateInstance(const ID: ISharedID; Param: Pointer = nil; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; overload;
    function CreateTool(const FileName: String; const ClassID: TGuid): TClass; overload;
    function CreateTool(const ClassID: TGuid): TClass; overload;
    function CreateTool(const ProgID: String): TClass; overload;
    procedure ReleaseInstance(var Obj);
    procedure ReleaseTool(var Obj);
  public
    constructor Create(const Manager: ISharedObjectManager);
    destructor Destroy; override;
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation

uses
  SilSmSharedID,
  SilSmSharedProviderV2,
  SilSfSharedFactoryList,
  SilSmSharedObjectProxy;

{ TSilSharedFactory }

constructor TSilSharedFactory.Create(const Manager: ISharedObjectManager);
begin
  inherited Create;
  FManager := Pointer(Manager);
end;

destructor TSilSharedFactory.Destroy;
begin
  FManager := nil;
  inherited;
end;

function TSilSharedFactory.CreateInstance(const ProgID: string; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  Result := CreateInstance(TSilSharedID.Create(ProgID), Param, Owner, Controller);
end;

function TSilSharedFactory.CreateInstance(const ClassID: TGuid; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  Result := CreateInstance(TSilSharedID.Create(ClassID), Param, Owner, Controller);
end;

function TSilSharedFactory.CreateInstance(const ID: ISharedID; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
var
  Provider: ISharedObjectProvider;
begin
  Provider := DoGetProvider(ID);
  try
    Result := DoCreateInstance(Provider, ID, Param, Owner, Controller);
  finally
    Provider := nil;
  end;
end;

function TSilSharedFactory.CreateInstance(const FileName: String; const ClassID: TGuid; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  Result := DoCreateInstance(DoGetProvider(TSilSharedID.Create(idModule, FileName)), TSilSharedID.Create(ClassID), Param, Owner, Controller);
end;

function TSilSharedFactory.CreateInstance(const FileName, ProgID: string; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  Result := DoCreateInstance(DoGetProvider(TSilSharedID.Create(idModule, FileName)), TSilSharedID.Create(ProgID), Param, Owner, Controller);
end;

function TSilSharedFactory.CreateTool(const ProgID: String): TClass;
begin
  { TODO : !falta implementar! }
  Result := nil;
end;

function TSilSharedFactory.CreateTool(const ClassID: TGuid): TClass;
begin
  { TODO : !falta implementar! }
  Result := nil;
end;

function TSilSharedFactory.CreateTool(const FileName: String; const ClassID: TGuid): TClass;
begin
  { TODO : !falta implementar! }
  Result := nil;
end;

procedure TSilSharedFactory.ReleaseInstance(var Obj);
var
  Unk: IUnknown;
begin
  Unk := IUnknown(Obj) as IUnknown;
  IUnknown(Obj) := nil;
  DoReleaseUnknown(Unk);
end;

procedure TSilSharedFactory.ReleaseTool(var Obj);
var
  Ptr: Pointer;
begin
  Ptr := Pointer(Obj);
  Pointer(Obj) := nil;
  DoReleasePtr(Ptr);
end;

function TSilSharedFactory.DoCreateInstance(const Provider: ISharedObjectProvider; const ID: ISharedID; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
var
  Container: ISharedObjectContainer;
  Proxy: ISharedObjectProxy;
  Instance: IUnknown; 
begin
  Container := DoGetContainer(Provider);
  try
    Proxy := TSilSharedObjectProxy.Create(Manager, Container, Controller);
    try
      Instance := DoCreateInstance(Container.Provider.Get(ID), Param, Owner, Proxy);
      Proxy.Instance := Instance;
      Result := Proxy;
    except
      Result := nil;
    end;
  finally
    Container := nil;  
  end;
end;

function TSilSharedFactory.DoCreateInstance(const Factory: ISharedFactory; Param: Pointer; const Owner, Controller: IUnknown): IUnknown;
begin
  if not Factory.CreateObject(Owner, IUnknown, Result, Controller, Param) then
    raise Sil.Error.Create('EL MODULO NO SOPORTA LA CREACION DE INSTANCIAS DE LA CLASE SOLICITADA'); { TODO : @!@ }
end;

function TSilSharedFactory.DoFindLocal(const ID: ISharedID; out Factory: ISharedFactory): Boolean;
begin
  with FactoryList do
    case ID.Kind of
      idName:   Result := Find(ID.AsName.ProgID, Factory);
      idClass:  Result := Find(ID.AsClass.ClassID, Factory);
      else      Result := False;
    end;
end;

function TSilSharedFactory.DoGetProvider(const ID: ISharedID): ISharedObjectProvider;
var
  Dummy: ISharedFactory;
begin
  if DoFindLocal(ID, Dummy) then
    Result := TSilObjectProviderV2.CreateNew(Sil.Local.Services)
  else if not Manager.Cache.Lookup(ID, Result) then
    Result := Manager.Loader.Get(ID);
end;

function TSilSharedFactory.DoGetContainer(const Provider: ISharedObjectProvider): ISharedObjectContainer;
begin
  if not Manager.List.Find(Provider, Result) then
    Result := Manager.List.Add(Provider);
end;

procedure TSilSharedFactory.DoReleaseUnknown(var Obj: IUnknown);
var
  Ptr: Pointer;
  i: Integer;
  ModuleName: String;
  Container: ISharedObjectContainer;
begin
  Ptr := Pointer(Obj);

  try
    i := Obj._Release;
    
    if i > 0 then
    begin
      if DoFindReference(Ptr, Container) then
        ModuleName := Container.Provider.Module.Info.FullName else
        ModuleName := '???';

      raise Error.Create('EL MODULO %s TIENE %d REFERENCIA(S), NO SE PUEDE DESCARGAR!!!', [ModuleName, i]);
    end;
  finally
    Pointer(Obj) := nil;
  end;

  DoReleasePtr(Ptr);
end;

procedure TSilSharedFactory.DoReleasePtr(const Ptr: Pointer);
var
  Item: ISharedObjectContainer;
begin
  Manager.Locked;
  if DoFindReference(Ptr, Item)
    and Item.References.DropRef(Ptr)
    and (Item.References.Count = 0) then
    Manager.List.Remove(Item);
end;

function TSilSharedFactory.DoFindReference(Ptr: Pointer; out Container: ISharedObjectContainer): Boolean;
var
  Enum: IEnumerator;
begin
  Result := False;
  with Manager.List do
    while not Result and Enumerate(Enum, Container) do
      Result := Container.References.Contains(Ptr);
end;

function TSilSharedFactory.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

end.
