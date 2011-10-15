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
  TSilLinuxSharedMemory = class(TSilSharedMemory)
  protected // TSilSharedMemory
    function DoCreate(const Name: PChar; Size: LongWord): IHandle; override;
    function DoOpen(const Name: PChar; out Size: LongWord): IHandle; override;
    function DoMap(const Start, Size: LongWord): Pointer; override;
    procedure DoUnmap(const Value: Pointer); override;
  end;

  TSharedMemory = class(TSilLinuxSharedMemory) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

uses
  SysUtils,
  SilOsError;

{ TSilLinuxSharedMemory }

function TSilLinuxSharedMemory.DoCreate(const Name: PChar; Size: LongWord): IHandle;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxSharedMemory.DoCreate']);
(*)
  Result := DoCreateHandle(Linux.CreateFileMapping($FFFFFFFF, nil, PAGE_READWRITE, 0, Size, Name));
  OsError.Check(OsState(Result.IsValid), 'TSilLinuxSharedMemory.DoCreate [Linux.CreateFileMapping]');
(*)end;

function TSilLinuxSharedMemory.DoOpen(const Name: PChar; out Size: LongWord): IHandle;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxSharedMemory.DoOpen']);
(*)
  Result := DoCreateHandle(Linux.OpenFileMapping(PAGE_READWRITE, false, Name));
  OsError.Check(Linux.GetFileSize(Result.Value, @Size), 'TSilLinuxSharedMemory.DoOpen [Linux.GetFileSize]');
(*)end;

function TSilLinuxSharedMemory.DoMap(const Start, Size: LongWord): Pointer;
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxSharedMemory.DoMap']);
(*)
  Result := Linux.MapViewOfFile(Self.Handle.Value, FILE_MAP_WRITE, 0, 0, Size);
  OsError.Check(OsState(Result <> nil), 'TSilLinuxSharedMemory.DoMap [Linux.MapViewOfFile]');
(*)end;

procedure TSilLinuxSharedMemory.DoUnmap(const Value: Pointer);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxSharedMemory.DoUnmap']);
(*)
  Linux.UnmapViewOfFile(Value);
(*)end;

end.
