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

unit SilOmSharedMemory;

{$INCLUDE Defines.inc}

interface

uses
  SilOsTypes,
  
  SilOiHandle,
  SilOkSharedMemory;

type
	TSilWindowsSharedMemory = class(TSilSharedMemory)
  protected // TSilSharedMemory
    function DoCreate(const Name: PChar; Size: LongWord): IHandle; override;
    function DoOpen(const Name: PChar; out Size: LongWord): IHandle; override;
    function DoMap(const Start, Size: LongWord): Pointer; override;
    procedure DoUnmap(const Value: Pointer); override;
	end;

  TSharedMemory = class(TSilWindowsSharedMemory) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

uses
  Windows,
  SilOsError;

{ TSilWindowsSharedMemory }

function TSilWindowsSharedMemory.DoCreate(const Name: PChar; Size: LongWord): IHandle;
begin
  Result := DoCreateHandle(Windows.CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size, Name));
  OsError.Check(OsState(Result.IsValid), 'TSilWindowsSharedMemory.DoCreate [Windows.CreateFileMapping]');
end;

function TSilWindowsSharedMemory.DoOpen(const Name: PChar; out Size: LongWord): IHandle;
begin
  Result := DoCreateHandle(Windows.OpenFileMapping(PAGE_READWRITE, false, Name));
	OsError.Check(Windows.GetFileSize(Result.Value, @Size), 'TSilWindowsSharedMemory.DoOpen [Windows.GetFileSize]');
end;

function TSilWindowsSharedMemory.DoMap(const Start, Size: LongWord): Pointer;
begin
  Result := Windows.MapViewOfFile(Self.Handle.Value, FILE_MAP_WRITE, 0, 0, Size);
  OsError.Check(OsState(Result <> nil), 'TSilWindowsSharedMemory.DoMap [Windows.MapViewOfFile]');
end;

procedure TSilWindowsSharedMemory.DoUnmap(const Value: Pointer);
begin
  Windows.UnmapViewOfFile(Value);  
end;

end.
