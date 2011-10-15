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

unit SilOfResource;

{$INCLUDE Defines.inc}

interface

uses
  SilOsTypes;

function DoFind(Module: THandle; const Kind, ID: WideString; out Info: THandle; Lang: Word = 0): Boolean;
function DoGet(Module: THandle; const Kind, ID: WideString; Lang: Word = 0): THandle;
function DoLoad(Module: THandle; Info: THandle): THandle;
function DoRead(Handle: THandle): Pointer;
function DoGetSize(Module: THandle; Info: THandle): LongWord;

implementation

uses
  SilOsError,
  Windows;

function DoFind(Module: THandle; const Kind, ID: WideString; out Info: THandle; Lang: Word = 0): Boolean;
begin
  Info := Windows.FindResourceExW(Module, PWideChar(Kind), PWideChar(ID), Lang);
  Result := Info <> 0;
end;

function DoGet(Module: THandle; const Kind, ID: WideString; Lang: Word = 0): THandle;
begin
  if not DoFind(Module, Kind, ID, Result, Lang) then
    raise OsError.Create(GetLastError, 'Windows.FindResourceExW');
end;

function DoLoad(Module: THandle; Info: THandle): THandle;
begin
  Result := Windows.LoadResource(Module, Info);
  if Result = 0 then raise OsError.Create(GetLastError, 'Windows.LoadResource');
end;

function DoGetSize(Module: THandle; Info: THandle): LongWord;
begin
  Result := Windows.SizeofResource(Module, Info);
  if Result = 0 then raise OsError.Create(GetLastError, 'Windows.SizeofResource');
end;

function DoRead(Handle: THandle): Pointer;
begin
  Result := Windows.LockResource(Handle);
  if Result = nil then raise OsError.Create(GetLastError, 'Windows.LockResource');
end;

end.
