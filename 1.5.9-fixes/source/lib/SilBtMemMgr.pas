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

unit SilBtMemMgr;

{$I Defines.inc}

interface

uses
  SilBtSystemMM,
  SilBkMemMgr;

procedure InitManager;
procedure DoneManager;
function GetManager: MemoryManagerType;
procedure SetManager(const Mgr: MemoryManagerType);

type
  DefaultMemoryManager = class(SystemMemoryManager);

implementation

function HookGetMem(Size: Integer): Pointer; forward;
function HookFreeMem(P: Pointer): Integer; forward;
function HookReallocMem(P: Pointer; Size: Integer): Pointer; forward;

var
  MManager: MemoryManagerType = DefaultMemoryManager;
  MHookMgr: System.TMemoryManager =
    ( GetMem: HookGetMem;
      FreeMem: HookFreeMem;
      ReallocMem: HookReallocMem );
  MOldMgr: System.TMemoryManager;

{ exports }

procedure InitManager;
begin
  System.GetMemoryManager(MOldMgr);
  System.SetMemoryManager(MHookMgr);
end;

procedure DoneManager;
begin
  System.SetMemoryManager(MOldMgr);
end;

function GetManager: MemoryManagerType;
begin
  Result := MManager;
end;

procedure SetManager(const Mgr: MemoryManagerType);
begin
  if (Mgr <> MManager) then
  begin
    if (MManager <> DefaultMemoryManager) then
      InitManager
    else if (Mgr = DefaultMemoryManager) then
      DoneManager;
    MManager := Mgr;
  end;
end;

{ hooks }

function HookGetMem(Size: Integer): Pointer;
begin
  Result := MManager.Get(Size);
end;

function HookFreeMem(P: Pointer): Integer;
begin
  Result := MManager.Free(P);
end;

function HookReallocMem(P: Pointer; Size: Integer): Pointer; 
begin
  Result := MManager.Realloc(P, Size);
end;

end.
