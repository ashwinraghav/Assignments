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

unit SilBtSystemMM;

interface

{$INCLUDE Defines.inc}

uses
  SilBkMemMgr,
  SilBeMemMgr;

type
  SystemMemoryManager = class(MemoryManager)
    class function Get(Size: Integer): Pointer; override;
    class function Free(P: Pointer): Integer; override;
    class function Realloc(P: Pointer; Size: Integer): Pointer; override;
    class function Info: RMemoryInfo; override;
  end;

implementation

uses
  SilBtMem,
  SilLfMem;

{ SystemMemoryManager }

class function SystemMemoryManager.Get(Size: Integer): Pointer;
begin
  Result := System.SysGetMem(Size);
end;

class function SystemMemoryManager.Free(P: Pointer): Integer;
begin
  Result := System.SysFreeMem(P);
end;

class function SystemMemoryManager.Realloc(P: Pointer; Size: Integer): Pointer;
begin
  Result := System.SysReallocMem(P, Size);
end;

class function SystemMemoryManager.Info: RMemoryInfo;
begin
  Result := HeapToInfo(SilLfMem.GetHeapStatus);
end;

end.
