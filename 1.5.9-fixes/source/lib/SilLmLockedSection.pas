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

unit SilLmLockedSection;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilLkObject;

type
  TSilLockedSection = class(
    TSilObject,
    ILock)
  private
    FLockable: ILockable;
    FReleased: Boolean;
  public
    constructor Create(const Lockable: ILockable; const AlreadyLocked: Boolean = False);
    destructor Destroy; override;
    procedure Release; reintroduce;
  end;

  TLockedSection = class(TSilLockedSection) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF}; 

implementation

{ TSilLockedSection }

constructor TSilLockedSection.Create(const Lockable: ILockable; const AlreadyLocked: Boolean);
begin
  inherited Create;
  FReleased := false;
  FLockable := Lockable;
  if Assigned(FLockable) and not AlreadyLocked then
    FLockable.Lock;
end;

destructor TSilLockedSection.Destroy;
begin
  Release;
  inherited Destroy;
end;

procedure TSilLockedSection.Release;
begin
  if Assigned(FLockable) and not FReleased then
  begin
    FLockable.Unlock;
    FReleased := true;
  end;
end;

end.
 