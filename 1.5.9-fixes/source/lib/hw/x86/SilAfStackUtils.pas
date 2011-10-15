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

unit SilAfStackUtils;

{$INCLUDE Defines.inc}

interface

function StackGetTop: Pointer; register;
procedure StackSetTop(Value: Pointer); register;
function StackGetFrame: Pointer; register;
function StackAlloc(Size: LongWord): Pointer; register;
function StackPullup(Frame: Pointer): Pointer; register;
procedure StackReturn(Value, Frame: Pointer); register;

implementation

{$W-,R-,Q-}

function StackGetTop: Pointer; register;
asm
    MOV   EAX, ESP
end;

function StackGetFrame: Pointer; register;
asm
    MOV   EAX, EBP
end;

procedure StackSetTop(Value: Pointer); register;
asm
    MOV   ESP, EAX
end;

function StackAlloc(Size: LongWord): Pointer; register;
asm
    POP   EDX
    MOV   ECX,  EAX
    ADD   ECX,  3
    AND   ECX,  NOT 3
    SUB   ESP,  ECX
    SHR   ECX,  1
    SHR   ECX,  1
    MOV   EAX,  ESP
    PUSH  EDI
    MOV   EDI,  EAX
    XOR   EAX,  EAX
    CLD
    REP   STOSD
    POP   EDI
    MOV   EAX,  ESP
    JMP   EDX
end;

function StackPullup(Frame: Pointer): Pointer; register;
asm
    pop   [Frame]
    pop   eax
end;

procedure StackReturn(Value, Frame: Pointer); register;
asm
    mov   ebp, Frame
    jmp   eax
end;

end.
 