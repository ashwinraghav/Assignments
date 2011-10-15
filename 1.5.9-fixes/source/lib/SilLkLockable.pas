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

unit SilLkLockable;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilLiObject,
  SilLkAggregable;

type
  TSilLockableExtension = class(
    TSilAggregableObject,
    ILockable )
  private
    FLockable: ILockable;
  protected // ILockable
    procedure Lock; reintroduce;
    procedure Unlock; reintroduce; 
  public
    constructor Create(const Controller: IUnknown = nil; const Owner: IUnknown = nil; Param: Pointer = nil); override; 
    destructor Destroy; override;
  (*)public
    class procedure Extend(const Instance: IObject);
    class procedure Check(const Instance: IObject; Locked: Boolean);(*)
  end;

implementation

uses
  SilLmLockedSection,
  SilOtTool;

{ TSilLockableExtension }

(*)class procedure TSilLockableExtension.Check(const Instance: IObject; Locked: Boolean);
begin
  if Locked then Extend(Instance);
end;

class procedure TSilLockableExtension.Extend(const Instance: IObject);
begin
  if Assigned(Instance) then
    Instance.Extension := Self.Create(Instance.MainInterface, Instance);
end;(*)

constructor TSilLockableExtension.Create(const Controller, Owner: IInterface; Param: Pointer);
begin
  inherited;
  FLockable := Os.Ipc.CriticalSection();
end;

destructor TSilLockableExtension.Destroy;
begin
  FLockable := nil;
  inherited;
end;

procedure TSilLockableExtension.Lock;
begin
  FLockable.Lock;
end;

procedure TSilLockableExtension.Unlock;
begin
  FLockable.Unlock;
end;

end.
