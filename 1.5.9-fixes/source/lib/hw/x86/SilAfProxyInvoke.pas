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

unit SilAfProxyInvoke;

{$INCLUDE Sil.inc}

interface

procedure DoProxyStdcallInvoke; stdcall;

implementation

uses
  SilBeInterfaceProxy,
  SilBhInterfaceProxy;

{$STACKFRAMES OFF}

procedure DoProxyStdcallInvoke; assembler;
asm
        PUSH    EBP                               // Armo el Stack Frame
        MOV     EBP, ESP                          // Se usa EBP para acceder a los parametros en el stack
        PUSH    EBX                               // voy a usar EBX para acceder al metodo
        MOV     EBX, ECX                          // ECX trae el puntero al RMethodInfo dentro del Entry
        LEA     EAX, [EBP + 12]                   // -> Argumentos de la funcion
        PUSH    EAX                               // último argumento de la funcion a invocar: Parameters
        PUSH    RMethodInfo[EBX].Info                 // primer argumento: Method
        MOV     EAX, [EBP + 8]                    // -> Instance pointer
        PUSH    RInterfaceProxy[EAX].Instance     // Este es el SELF para el metodo a llamar
        CALL    RMethodInfo[EBX].Address.Code         // Invoco a la rutina especificada
        MOV     ECX, RMethodInfo[EBX].StackSize       // Hay que limpiar el Stack: tomo el tamaño de parametros
        POP     EBX                               // Restauro el EBX salvado arriba
        POP     EBP                               // Recupero el EBP salvado arriba
        POP     EDX                               // Obtengo la dirección de retorno: ahi debo volver
        ADD     ESP, ECX                          // Elimino los parametros de la llamada original
        JMP     EDX                               // En lugar de un RET hago un JMP :)
end;

end.
 