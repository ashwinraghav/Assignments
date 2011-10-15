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

unit SilOiIpc;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOsTypes,

  SilLiLock,
  SilOiWait,
  SilOiHandle;

type
  ICriticalSection = interface (ILockable)
    ['{63AF56E9-99B4-4FAD-AE1B-2A08FE918974}']
    function GetEntered: Boolean;
    function Locked: ILock;
    function TryLock: ILock;
    property Entered: Boolean read GetEntered;
  end;

  IIpcObject = interface(IWaitable)
    ['{C7F96C20-2C74-4528-BCA2-F042BA559F44}']
    function GetSignaled: Boolean;
    property IsSignaled: Boolean read GetSignaled;
  end;

  IEvent = interface(IIpcObject)
    ['{FD203304-0B70-11D4-9155-00C0261013CD}']
    procedure Signal;
    procedure Reset;
    procedure Pulse;
  end;

  IMutex = interface(IIpcObject)
    ['{FD203304-0B70-11D4-9155-00C0261013CD}']
    procedure Release;
  end;

  ISemaphore = interface(IIpcObject)
    ['{7AAC488C-5353-4EA0-85E1-D6E10A6B5E51}']
    function Release(Count: Integer = 1): Integer;
  end;

implementation
end.
