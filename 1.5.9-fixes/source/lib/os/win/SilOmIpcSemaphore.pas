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

unit SilOmIpcSemaphore;

{$I Defines.inc}

interface

uses
  SilOiHandle,
  SilOsTypes,

  SilOkIpcSemaphore;

type
	TSilWindowsSemaphore = class(TSilSemaphore)
	protected // TSilSemaphore
    function DoCreate(const Name: PChar; const InitialCount, MaxCount: Integer): IHandle; override; 
    function DoOpen(const Name: PChar): IHandle; override;
    function DoRelease(Count: Integer): Integer; override;
	end;

implementation

uses
  Windows;

{ TSilWindowsSemaphore }

function TSilWindowsSemaphore.DoCreate(const Name: PChar; const InitialCount, MaxCount: Integer): IHandle;
begin
  Result := DoCreateHandle(Windows.CreateSemaphore(nil, InitialCount, MaxCount, Name));
end;

function TSilWindowsSemaphore.DoOpen(const Name: PChar): IHandle;
begin
  Result := DoCreateHandle(Windows.OpenSemaphore(0, false, Name));
end;

function TSilWindowsSemaphore.DoRelease(Count: Integer): Integer;
begin
  Windows.ReleaseSemaphore(Self.Handle.Value, Count, @Result)
end;

end.
