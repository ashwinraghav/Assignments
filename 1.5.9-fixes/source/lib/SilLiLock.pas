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

unit SilLiLock;

{$I Defines.inc}

interface

type
  ILockable = interface;
  ILock = interface;
  ILocker = interface;
  ISynchronizable = interface;
  ILockingSet = interface;

  ILockable = interface
    ['{51A30912-774C-11D3-9853-00104B0FA1EF}']
    procedure Lock;
    procedure Unlock;
  end;

  ILock = interface
    ['{07E518B6-C09F-11D3-9C64-00C0DFE46337}']
    procedure Release;
  end;

  ILocker = interface
    ['{07E518B7-C09F-11D3-9C64-00C0DFE46337}']
    function Lock(const AContext: Pointer = nil): ILock;
  end;
  
  IExtendedLocker = interface(ILocker)
    ['{783527E1-59AA-11D4-AD8D-00902794F778}']
    function GetLocked: Boolean;
    function GetTarget: Pointer;
    property IsLocked: Boolean read GetLocked;
    property Target: Pointer read GetTarget;
  end;

  ISynchronizable = interface
    ['{6ABC1992-C404-428B-A207-3D9D70FB874E}']
    function Lock: ILock;
  end;

  ILockingSet = interface
    ['{EB9D8312-5CC6-42C9-8899-20F2F1254614}']
    function GetIsLockable: Boolean;
    procedure SetIsLockable(const Value: Boolean);
    function GetLockable: ILockable;
    procedure SetLockable(const Value: ILockable);
    property IsLockable: Boolean read GetIsLockable write SetIsLockable;
    property Lockable: ILockable read GetLockable write SetLockable;
  end;

implementation
end.
