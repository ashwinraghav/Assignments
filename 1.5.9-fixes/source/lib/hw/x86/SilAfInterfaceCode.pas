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

unit SilAfInterfaceCode;

{$INCLUDE Sil.inc}

interface

const
  opNOP     = $90;
  opJUMP    = $E9;
  opCALL    = $E8;
  opPOP_ECX = $59;

type
  RMethodHeader = packed record
    Nop: array[0 .. 1] of Byte;
    EntryPoint: record end;
    Code: array[0 .. 1] of byte;
    Offset: LongInt;
  end;

  RMethodCode = packed record
    Nop: array[0 .. 2] of Byte;
    EntryPoint: record end;
    Call: byte; // $E8
    Offset: LongInt;
  end;

function CalcOffset(Src, Dest: Pointer): Longint;
procedure MakeHeader(var Header: RMethodHeader; const Dest: Pointer); 
procedure MakeCode(var Code: RMethodCode; const Dest: Pointer);

implementation

function CalcOffset(Src, Dest: Pointer): Longint;
begin
  Result := Longint(Dest) - (Longint(Src) + 5);
end;

procedure MakeHeader(var Header: RMethodHeader; const Dest: Pointer);
begin
  Header.Nop[0] := opNOP;       // NOP
  Header.Nop[1] := opNOP;       // NOP
  Header.Code[0] := opPOP_ECX;  // POP ECX  ; -> contiene la direccion del metodo invocado
  Header.Code[1] := opJUMP;     // Opcode de JMP <OFFSET 32 bits>
  Header.Offset := CalcOffset(@Header.Code[1], Dest);
end;

procedure MakeCode(var Code: RMethodCode; const Dest: Pointer);
begin
  Code.Nop[0] := opNOP;
  Code.Nop[1] := opNOP;  
  Code.Nop[2] := opNOP;
  Code.Call := opCALL; // Opcode de CALL <OFFSET 32 bits>
  Code.Offset := CalcOffset(@Code.Call, Dest);
end;

end.
