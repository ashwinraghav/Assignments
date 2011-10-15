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

unit SilLtLock;

{$I Defines.inc}

interface

uses
  SilLiLock,
  SilBkTool;

type
  ILock = SilLiLock.ILock;

type
  Lock = class(Tool)
    class function Take(const Instance: IUnknown; Exclusive: Boolean; const AContext: Pointer = nil): ILock; overload;
    class function Take(const Lockable: ILockable): ILock; overload;
    class function Take(const Lockable: ILockable; Exclusive: Boolean; const AContext: Pointer = nil): ILock; overload;
    class function Take(const Locker: ILocker; Exclusive: Boolean = False; const AContext: Pointer = nil): ILock; overload;
    class function AsLocker(const ALockable: IUnknown; Counted: Boolean = False): ILocker; overload;
    class function AsLocker(var Field: Boolean; LockedValue: Boolean = True; Counted: Boolean = False): ILocker; overload;
    class function AsLocker(Target: Pointer = nil; Counted: Boolean = False): ILocker; overload;
    class function IsLocked(const Locker: ILocker): Boolean;
  end;

implementation

uses
  SilLtReference,
  SilLkLockers,
  SilLkAbstractLocker,
  SilLmLockedSection;

class function Lock.Take(const Lockable: ILockable): ILock;
begin
  Result := TSilLockedSection.Create(Lockable);
end;

class function Lock.Take(const Instance: IUnknown; Exclusive: Boolean; const AContext: Pointer): ILock;
var
  Locker: ILocker;
  Lockable: ILockable;
begin
  if Reference.GetInterface(Instance, ILockable, Lockable) then
    Result := Take(Lockable, Exclusive, AContext)
  else if Reference.GetInterface(Instance, ILocker, Locker) then
    Result := Take(Locker, Exclusive, AContext)
  else
    Result := nil;
end;

class function Lock.Take(const Locker: ILocker; Exclusive: Boolean; const AContext: Pointer): ILock;
var
  Lockable: ILockable;
  Extended: IExtendedLocker;
begin
  Result := nil;
  if not Exclusive then
    Result := Locker.Lock(AContext)
  else if Ref.GetInterface(Locker, IExtendedLocker, Extended) then
    begin
      Ref.GetInterface(Extended, ILockable, Lockable);
      if Assigned(Lockable) then Lockable.Lock;
      try
        if not Extended.IsLocked then
          Result := Extended.Lock(AContext) else
          Result := nil;
      finally
        if Assigned(Lockable) then Lockable.Unlock;
      end;
    end;
end;

class function Lock.Take(const Lockable: ILockable; Exclusive: Boolean; const AContext: Pointer): ILock;
begin
  if not Exclusive then
    Result := Take(Lockable) else
    Result := Take(AsLocker(Lockable), Exclusive, AContext);
end;

class function Lock.AsLocker(const ALockable: IUnknown; Counted: Boolean): ILocker;
begin
  Result := TLockableLocker.Create(ALockable);
  if Counted then Result := TCountedLocker.Create(Result as IExtendedLocker);
end;

class function Lock.AsLocker(var Field: Boolean; LockedValue: Boolean; Counted: Boolean): ILocker;
begin
  Result := TBooleanLocker.Create(Field, LockedValue);
  if Counted then Result := TCountedLocker.Create(Result as IExtendedLocker);
end;

class function Lock.AsLocker(Target: Pointer; Counted: Boolean): ILocker;
begin
  Result := TExtendedLocker.Create(Target);
  if Counted then Result := TCountedLocker.Create(Result as IExtendedLocker);
end;

class function Lock.IsLocked(const Locker: ILocker): Boolean;
var
  ExtLock: IExtendedLocker;
begin
  Result := Reference.GetInterface(Locker, IExtendedLocker, ExtLock) and ExtLock.IsLocked;
end;

end.
