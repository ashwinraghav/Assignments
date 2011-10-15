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

unit SilAfLockedIncrement;

{$I Defines.inc}

interface

function LockedAdd(Delta: Integer; var P): Integer;        assembler;
function LockedSub(Delta: Integer; var P): Integer;        assembler;
function LockedInc(var P): Integer;                        assembler;
function LockedDec(var P): Integer;                        assembler;
function LockedExc(var P; const Value: Integer): Integer;  assembler;
function LockedGet(var P): Integer;                        assembler;

implementation

function LockedInc(var P): Integer;
asm
           MOV     ECX, EAX
           MOV     EAX, +1
           NOP
  LOCK     XADD   [ECX], EAX
           INC     EAX
end;

function LockedDec(var P): Integer;
asm
           MOV     ECX, EAX
           MOV     EAX, -1
           NOP
  LOCK     XADD   [ECX], EAX
           DEC     EAX
end;

function LockedAdd(Delta: Integer; var P): Integer;
asm
           MOV     ECX , EAX
  LOCK     XADD   [EDX], EAX
           ADD     EAX,  ECX
end;

function LockedExc(var P; const Value: Integer): Integer;
asm
  LOCK     XCHG    [EAX], EDX
           MOV     EAX, EDX
end;

function LockedGet(var P): Integer;
asm
  LOCK     MOV     EAX, [EAX]
end;

function LockedSub(Delta: Integer; var P): Integer;
asm
           NEG     EAX
           JMP     LockedAdd
end;


end.

