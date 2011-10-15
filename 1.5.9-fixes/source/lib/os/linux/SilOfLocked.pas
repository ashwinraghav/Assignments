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

unit SilOfLocked;

{$INCLUDE Defines.inc}

interface

function InterlockedIncrement(var Addend: Integer): Integer; {$IFNDEF FP20} stdcall; {$ENDIF}
function InterlockedDecrement(var Addend: Integer): Integer; {$IFNDEF FP20} stdcall; {$ENDIF}
function InterlockedExchange(var Target: Integer; Value: Integer): Integer; {$IFNDEF FP20} stdcall; {$ENDIF}
function InterlockedCompareExchange(var Destination: Pointer; Exchange: Pointer; Comperand: Pointer): Pointer; {$IFNDEF FP20} stdcall; {$ENDIF}
function InterlockedExchangeAdd(Addend: PLongint; Value: Longint): Longint; {$IFNDEF FP20} stdcall; {$ENDIF} overload;
function InterlockedExchangeAdd(var Addend: Longint; Value: Longint): Longint; {$IFNDEF FP20} stdcall; {$ENDIF} overload;

implementation

uses
  SysUtils,
  SilAfLockedIncrement;

function InterlockedIncrement(var Addend: Integer): Integer;
begin
  Result := LockedInc(Addend);
end;

function InterlockedDecrement(var Addend: Integer): Integer;
begin
  Result := LockedDec(Addend);
end;

function InterlockedExchange(var Target: Integer; Value: Integer): Integer;
begin
  Result := LockedExc(Target, Value);
end;

function InterlockedCompareExchange;
begin
  raise Exception.CreateFmt('%s: not implemented', ['InterlockedCompareExchange']);
end;

function InterlockedExchangeAdd(Addend: PLongint; Value: Longint): Longint;
begin
  raise Exception.CreateFmt('%s: not implemented', ['InterlockedExchangeAdd']);
end;

function InterlockedExchangeAdd(var Addend: Longint; Value: Longint): Longint;
begin
  raise Exception.CreateFmt('%s: not implemented', ['InterlockedExchangeAdd']);
end;

end.
