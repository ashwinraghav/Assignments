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

unit SilLkAbstractEnumerator;

{$I Defines.inc}

interface

uses
  SilBkPtr,
  SilLiEnumerator,
  SilLiLock,
  SilLiList,
  SilLkInterfaced;

type

{  TAbstractEnumerator }

  TAbstractEnumerator = class(
    TSilInterfacedObject,
    IItemHandler,
    IEnumerator )
  protected
    FContainer: IUnknown;
    FLock: ILockable;
    FItem: Integer;
    FIteration: Integer;
    FTypeHandler: HandlerType;
    FTypeData: Pointer;        
    FDetached: Boolean;
    procedure DoDetach; virtual;
    procedure DoReset; virtual;
    procedure DoClear(var Item);
  protected // IItemHandler
    function GetDataType: HandlerType;
    function GetDataInfo: Pointer;
  protected // IEnumerator
    function HasMore: Boolean;
    function DoHasMore: Boolean; virtual; abstract;
    function Get(out Item): Boolean; virtual;
    function GetCurrent: Pointer; virtual; abstract;
    function Next: Boolean; virtual; abstract;
    function GetIteration: Integer;
    procedure Detach;
    procedure Reset;
  public
    constructor Create(const Container: IUnknown; const Locked: Boolean = false; const TypeHandler: HandlerType = nil; const TypeData: Pointer = nil); virtual;
    destructor Destroy; override;
    procedure AfterConstruction; override;
  end;

implementation

uses
  SilLtReference;

{ TAbstractEnumerator }

constructor TAbstractEnumerator.Create(const Container: IUnknown; const Locked: Boolean; const TypeHandler: HandlerType; const TypeData: Pointer);
begin
  inherited Create;
  FContainer := Container;
  FTypeHandler := BaseHandler.Check(TypeHandler);
  FTypeData := TypeData;
  if Locked and Reference.GetInterface(Container, ILockable, FLock) then FLock.Lock;
end;

destructor TAbstractEnumerator.Destroy;
begin
  Detach;
  inherited;
end;

procedure TAbstractEnumerator.AfterConstruction;
begin
  Reset;
  inherited;
end;

function TAbstractEnumerator.Get(out Item): Boolean;
var
  Ptr: Pointer;
begin
  Ptr := GetCurrent();
  Result := Ptr <> nil;
  if Result then
    FTypeHandler.ToObj(Ptr, Item, FTypeData);
end;

function TAbstractEnumerator.GetIteration: Integer;
begin
  Result := FIteration;
end;

function TAbstractEnumerator.HasMore: Boolean;
begin
  if FDetached then
    Result := false else
    Result := DoHasMore;
end;

procedure TAbstractEnumerator.Detach;
begin
  if not FDetached then
  try
    if FLock <> nil then
      FLock.Unlock;
    DoDetach;
  finally
    FContainer := nil;
    FLock := nil;
    FDetached := True;
  end;
end;

procedure TAbstractEnumerator.Reset;
begin
  FIteration := 0;
  DoReset;
end;

function TAbstractEnumerator.GetDataInfo: Pointer;
begin
  Result := FTypeData;
end;

function TAbstractEnumerator.GetDataType: HandlerType;
begin
  Result := FTypeHandler;
end;

procedure TAbstractEnumerator.DoDetach;
begin
end;

procedure TAbstractEnumerator.DoReset;
begin
end;

procedure TAbstractEnumerator.DoClear(var Item);
begin
  FTypeHandler.Clear(Item);
end;


end.
