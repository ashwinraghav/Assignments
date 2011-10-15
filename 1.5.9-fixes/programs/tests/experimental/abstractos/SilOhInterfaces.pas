{********************************************************************************
 *                  Standard Interface Library (SIL)                            *
 *                                                                              *
 *       General purpose library whose design is based in STRONG                *
 *   use of interfaces.                                                         *
 *                                                                              *
 *                                                                              *
 *     Copyright (C) 2000 Mariano Podestá    marianop@intercom.com.ar           *
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

unit SilOhInterfaces;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilOiHandle,
  SilOiWait,
  SilOiThread;

type
  IOsThreadCallbacks = interface;
  IOsThreadInstance = interface;
  IOsSharedLibraryInstance = interface;

  IOsWaitable = interface
    ['{97F5E586-66B9-46C6-B3A6-A7E6AE849B09}']
    function WaitFor(const Timeout: LongWord; AvoidMsgLock: Boolean = false): TSyncWaitResult;
  end;

  IOsThreadCallbacks = interface
    ['{0075635B-5B93-4908-8542-5627E285DA4A}']
    procedure Initialize(const Sender: IOsThreadInstance; Instance: Pointer);
    procedure Finalize(const Sender: IOsThreadInstance; Instance: Pointer);
    function Execute(const Sender: IOsThreadInstance; Instance: Pointer): Integer;
  end;

  IOsThreadInstance = interface (IHandle)
    ['{2E179783-D89E-4730-93FB-C5AC4AF266E3}']
    function GetThreadID: LongWord;
    function GetPriority: TThreadPriority;
    procedure SetPriority(Value: TThreadPriority);
    function Resume: Boolean;
    function Suspend: Boolean;
    property ID: LongWord read GetThreadID;
    property Priority: TThreadPriority read GetPriority write SetPriority;
  end;

  IOsSharedLibraryInstance = interface (IHandle)
    ['{99CE308C-D4BA-4891-A2F6-DF5D3C78E2EB}']
    function Get(const Name: String): Pointer;
  end;

  POsInterface = ^ROsInterface;
  POsThreadFactory = ^ROsThreadFactory;
  POsSharedLibraryFactory = ^ROsSharedLibraryFactory;

  ROsThreadFactory = record
    GetCurrent: function: IOsThreadInstance; stdcall;
    Create: function(const Callbacks: IOsThreadCallbacks; CreateSuspended: Boolean; Arguments: Pointer = nil): IOsThreadInstance; stdcall;
  end;

  ROsSharedLibraryFactory = record
    Create: function(const Name: String): IOsSharedLibraryInstance; stdcall;
  end;

  ROsInterface = record
    SharedLibrary: POsSharedLibraryFactory;
    Thread: POsThreadFactory;
  end;

implementation
end.
