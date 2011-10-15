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

unit SilOtSynchronization;

{$INCLUDE Defines.inc}

{$IFDEF USE_DEPRECATED}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

interface

uses
  SilLiLock,
  SilBkTool,

  SilOiIpc;

type
  OSEventClass = class of Event;

  Event = class(Tool)
    class function Create(ManualReset: Boolean = true; InitialState: Boolean = false; Name: PChar = nil): IEvent;
    class function Open(Name: PChar): IEvent;
  end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

  OSMutexClass = class of Mutex;

  Mutex = class(Tool)
    class function Create(InitialOwner: Boolean = false; const Name: PChar = nil): IMutex;
    class function Open(const Name: PChar): IMutex;
  end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

  OSSemaphoreClass = class of Semaphore;

  Semaphore = class(Tool)
    class function Create(InitialCount: Integer = 0; MaxCount: Integer = 0; Name: PChar = nil): ISemaphore;
    class function Open(Name: PChar): ISemaphore;
  end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

  OSCriticalSectionClass = class of CriticalSection;

  CriticalSection = class(Tool)
    class function Create: ILockable;
  end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

uses
  SilOtTool;

{ Event }

class function Event.Create(ManualReset, InitialState: Boolean; Name: PChar): IEvent;
begin
  Result := Os.Ipc.Event(ManualReset, InitialState, Name);
end;

class function Event.Open(Name: PChar): IEvent;
begin
  Result := Os.Ipc.Event(Name);
end;

{ Mutex }

class function Mutex.Create(InitialOwner: Boolean; const Name: PChar): IMutex;
begin
  Result := Os.Ipc.Mutex(InitialOwner, Name);
end;

class function Mutex.Open(const Name: PChar): IMutex;
begin
  Result := Os.Ipc.Mutex(Name);
end;

{ Semaphore }

class function Semaphore.Create(InitialCount, MaxCount: Integer; Name: PChar): ISemaphore;
begin
  Result := Os.Ipc.Semaphore(InitialCount, MaxCount, Name);
end;

class function Semaphore.Open(Name: PChar): ISemaphore;
begin
  Result := Os.Ipc.Semaphore(Name);
end;

{ CriticalSection }

class function CriticalSection.Create: ILockable;
begin
  Result := Os.Ipc.CriticalSection() as ILockable;
end;

end.

