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

unit SilOkIpcSemaphore;

{$I Defines.inc}

interface

uses
  SilOsTypes,

  SilLiLock,
  SilOiHandle,
  SilOiIpc,

  SilOsIpc;

type
	TSilSemaphore = class(
  //- extends
    TSilOsIpcObject,
  //- implements
    ISemaphore )
	protected // ISemaphore
    function ISemaphore.Release = DoRelease;
	protected
    function DoCreate(const Name: PChar; const InitialCount, MaxCount: Integer): IHandle; virtual; abstract;
    function DoOpen(const Name: PChar): IHandle; reintroduce; virtual; abstract;
    function DoRelease(Count: Integer): Integer; virtual; abstract;
	public
		constructor Create(const Name: PChar; const InitialCount, MaxCount: Integer); overload;
		constructor Create(const Name: PChar); overload; 
	end;

implementation

uses
  SilOsError;

{ TSilSemaphore }

constructor TSilSemaphore.Create(const Name: PChar; const InitialCount, MaxCount: Integer);
begin
	inherited Create(DoCreate(Name, InitialCount, MaxCount));
end;

constructor TSilSemaphore.Create(const Name: PChar);
begin
	inherited Create(DoOpen(Name));
  if not Handle.IsValid then
    OsError.Check(False, 'TSilSemaphore.Create[Open]');
end;

end.
