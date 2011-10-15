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

unit SilOtIpc;

{$I Defines.inc}

interface

uses
  SilBkTool,

  SilOiIpc,
  SilOiSharedMemory,
  
  SilOjIpc;

type
  LinuxIpcTool = class(SilIpcTool)
    class function Event(ManualReset: Boolean = true; InitialState: Boolean = false; const Name: PChar = nil): IEvent; override;
    class function Event(const Name: PChar): IEvent; override;
    class function Mutex(InitialOwner: Boolean = false; const Name: PChar = nil): IMutex; override;
    class function Mutex(const Name: PChar): IMutex; override;
    class function Semaphore(InitialCount: Integer = 0; MaxCount: Integer = 0; const Name: PChar = nil): ISemaphore; override;
    class function Semaphore(const Name: PChar): ISemaphore; override;
    class function CriticalSection: ICriticalSection; override;
    class function SharedMemory(const Size: LongWord; const Name: PChar = nil): ISharedMemory; override;
    class function SharedMemory(const Name: PChar): ISharedMemory; override;
  end;

implementation

uses
  SilOsClasses, SilOkSharedMemory;

{ LinuxIpcTool }

class function LinuxIpcTool.CriticalSection: ICriticalSection;
begin
  Result := TSilOsCriticalSection.Create;
end;

class function LinuxIpcTool.Event(const Name: PChar): IEvent;
begin
  Result := TSilOsEvent.Create(Name);
end;

class function LinuxIpcTool.Event(ManualReset, InitialState: Boolean; const Name: PChar): IEvent;
begin
  Result := TSilOsEvent.Create(ManualReset, InitialState, Name);
end;

class function LinuxIpcTool.Mutex(InitialOwner: Boolean; const Name: PChar): IMutex;
begin
  Result := TSilOsMutex.Create(Name, InitialOwner);
end;

class function LinuxIpcTool.Mutex(const Name: PChar): IMutex;
begin
  Result := TSilOsMutex.Create(Name);
end;

class function LinuxIpcTool.Semaphore(InitialCount, MaxCount: Integer; const Name: PChar): ISemaphore;
begin
  if MaxCount = 0 then MaxCount := System.MaxInt;
  Result := TSilOsSemaphore.Create(Name, InitialCount, MaxCount);
end;

class function LinuxIpcTool.Semaphore(const Name: PChar): ISemaphore;
begin
  Result := TSilOsSemaphore.Create(Name);
end;

class function LinuxIpcTool.SharedMemory(const Size: LongWord; const Name: PChar): ISharedMemory;
begin
  Result := TSilOsSharedMemory.Create(Name, Size);
end;

class function LinuxIpcTool.SharedMemory(const Name: PChar): ISharedMemory;
begin
  Result := TSilOsSharedMemory.Create(Name);
end;

end.
