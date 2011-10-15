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

unit SilSgDateMapper;

interface

uses
  SilScDateMapper,
  SilSeDateMapper;

const
  GMappingEntries: TEntriesTable =
    (
      (
        Size:  KUnaHora;
        Delta: 5 * KUnMinuto;
        Flags: [dfExtremos, dfMediaHora];
      ), //  0

      (
        Size:  2 * KUnaHora;
        Delta: 5 * KUnMinuto;
        Flags: [dfExtremos, dfUnaHora, dfMediaHora];
      ), //  0

      (
        Size: 5 * KUnaHora;
        Delta: 15 * KUnMinuto;
        Flags: [dfExtremos, dfUnaHora];
      ), //  1
      
      (
        Size: 12 * KUnaHora;
        Delta: 30 * KUnMinuto;
        Flags: [dfExtremos, dfTresHoras];
      ), //  1
      
      (
        Size: KUnDia;
        Delta: KUnaHora;
        Flags: [dfExtremos, dfSeisHoras];
      ), //  2

      (
        Size: 7 * KUnDia;
        Delta: 12 * KUnaHora;
        Flags: [dfUnDia];
      ), //  3

      (
        Size: 30 * KUnDia;
        Delta: KUnDia;
        Flags: [dfSieteDias];
      ), //  4

      (
        Size: 100 * KUnDia;
        Delta: 2 * KUnDia;
        Flags: [dfMes, dfDosDias];
      ), //  5

      (
        Size: 214 * KUnDia;
        Delta: 2 * KUnDia;
        Flags: [dfMes, dfDiezDias];
      ), //  6

      (
        Size: 300 * KUnDia;
        Delta: 2 * KUnDia;
        Flags: [dfMes, dfQuinceDias];
      ), //  7

      (
        Size: 1800 * KUnDia;
        Delta: 2 * KUnDia;
        Flags: [dfMes, dfSeisMeses];
      ), //  8

      (
        Size: 3000 * KUnDia;
        Delta: 2 * KUnDia;
        Flags: [dfAnual, dfSeisMeses];
      ) //  9
    );

implementation
end.
