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

unit SilSmSharedObjectProxy;

{$INCLUDE Defines.inc}

interface

uses
  Sil,
  SilSiSharedObject,
  SilShSharedObject,
  SilShSharedManager;

type
  TSilSharedObjectProxy = class(
    TSilAggregatedObject,
    ISharedObjectProxy )
  private
    FManager: Pointer;
    FContainer: ISharedObjectContainer;
    FInstance: IUnknown;
  private
    function DoMakeRef(const Container: ISharedObjectContainer; Instance: PUnknown; const Value: IUnknown): Boolean;
    function DoDropRef(const Container: ISharedObjectContainer; Instance: PUnknown): Boolean;
    function DoGetManager: ISharedObjectManager;
  protected // ISharedObjectProxy
    function GetContainer: ISharedObjectContainer;
    function GetInstance: IUnknown;
    procedure SetInstance(const Value: IUnknown);
  public
    constructor Create(const Manager: ISharedObjectManager; const Container: ISharedObjectContainer; const Controller: IUnknown); reintroduce;
    destructor Destroy; override;
  public
    function ObjQueryInterface(const IID: TGUID; out Obj): HResult; override;
  public
    property Manager: ISharedObjectManager read DoGetManager;
  end;

implementation

uses SilLiObject;

type
  TSilSharedDropAction = class(
    TSilObject,
    IAction )
  private
    FManager: ISharedObjectManager;
    FContainer: ISharedObjectContainer;
  protected // IAction
    procedure Execute(const Controller: IUnknown; const Item: Pointer; const Args: Pointer = nil); register;
  public
    constructor Create(const Manager: ISharedObjectManager; const Container: ISharedObjectContainer);
    destructor Destroy; override;
  end;

{ TSilSharedObjectProxy }

constructor TSilSharedObjectProxy.Create(const Manager: ISharedObjectManager; const Container: ISharedObjectContainer; const Controller: IUnknown);
begin                
  inherited Create(Controller);
  FManager := Pointer(Manager);
  FContainer := Container;
end;

destructor TSilSharedObjectProxy.Destroy;
begin
  DoDropRef(FContainer, @FInstance);
  FContainer := nil;
  FManager := nil;
  inherited;
end;

function TSilSharedObjectProxy.ObjQueryInterface(const IID: TGUID; out Obj): HResult;
begin
  Result := E_NOINTERFACE;  
  if not Sil.GUID.IsEqual(IUnknown, IID) and Assigned(FInstance) then Result := FInstance.QueryInterface(IID, Obj);
  if Result <> 0 then Result := inherited ObjQueryInterface(IID, Obj);
end;

function TSilSharedObjectProxy.GetContainer: ISharedObjectContainer;
begin
  Result := FContainer;
end;

function TSilSharedObjectProxy.GetInstance: IUnknown;
begin
  Result := FInstance;
end;

procedure TSilSharedObjectProxy.SetInstance(const Value: IInterface);
begin
  DoDropRef(FContainer, @FInstance);
  DoMakeRef(FContainer, @Finstance, Value);  
end;

function TSilSharedObjectProxy.DoGetManager: ISharedObjectManager;
begin
  Result := ISharedObjectManager(FManager);
end;

function TSilSharedObjectProxy.DoMakeRef(const Container: ISharedObjectContainer; Instance: PUnknown; const Value: IUnknown): Boolean;
begin
  FContainer := Container;
  Instance^ := Value;
  Result := Assigned(Value);
  if Result then FContainer.References.AddRef(Instance);
end;

function TSilSharedObjectProxy.DoDropRef(const Container: ISharedObjectContainer; Instance: PUnknown): Boolean;
begin
  Result := Assigned(Instance^);
  if Result then
  try
    if FContainer.References.DropRef(Instance) and (FContainer.References.Count = 0) then
      Manager.Watchdog.Post(TSilSharedDropAction.Create(Manager, FContainer));
  finally
    Instance^ := nil;
  end;
end;

{ TSilSharedDropAction }

constructor TSilSharedDropAction.Create(const Manager: ISharedObjectManager; const Container: ISharedObjectContainer);
begin
  inherited Create;
  FManager := Manager;
  FContainer := Container;
end;

destructor TSilSharedDropAction.Destroy;
begin
  FContainer := nil;
  FManager := nil;
  inherited;
end;

procedure TSilSharedDropAction.Execute(const Controller: IInterface; const Item, Args: Pointer);
begin
  Sil.Os.Wait.Sleep(30);
  FManager.List.Remove(FContainer);
end;

end.
