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

unit SilOjIpc;

{$I Defines.inc}

interface

uses
  SilBkTool,
  
  SilOiIpc,
  SilOiSharedMemory;

type
  SilIpcTool = class(Tool)
    class function CriticalSection: ICriticalSection; virtual; abstract;
    class function Event(ManualReset: Boolean = true; InitialState: Boolean = false; const Name: PChar = nil): IEvent; overload; virtual; abstract;
    class function Event(const Name: PChar): IEvent; overload; virtual; abstract;
    class function Mutex(InitialOwner: Boolean = false; const Name: PChar = nil): IMutex; overload; virtual; abstract;
    class function Mutex(const Name: PChar): IMutex; overload; virtual; abstract;
    class function Semaphore(InitialCount: Integer = 0; MaxCount: Integer = 0; const Name: PChar = nil): ISemaphore; overload; virtual; abstract;
    class function Semaphore(const Name: PChar): ISemaphore; overload; virtual; abstract;
    class function SharedMemory(const Size: LongWord; const Name: PChar = nil): ISharedMemory; overload; virtual; abstract;
    class function SharedMemory(const Name: PChar): ISharedMemory; overload; virtual; abstract;
  end;

implementation
end.
