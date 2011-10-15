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

unit SilOhSocket;

{$I Defines.inc}

interface

uses
  Windows;

const
  CIPHlpApi = 'iphlpapi.dll';

type
  PMIB_IPADDRROW = ^MIB_IPADDRROW;
  MIB_IPADDRROW = packed record
    dwAddr: DWORD;              // IP address
    dwIndex: DWORD;             // interface index
    dwMask: DWORD;              // subnet mask
    dwBCastAddr: DWORD;         // broadcast address
    dwReasmSize: DWORD;         // rassembly size
    unused1: Word;              // not currently used
    unused2: Word;              // not currently used
  end;

  PMIB_IPADDRTABLE = ^MIB_IPADDRTABLE;
  MIB_IPADDRTABLE = packed record
    dwNumEntries: DWORD;                        // number of entries in the table
    table: packed array[0..0] of MIB_IPADDRROW; // array of IP address entries
  end;

  TGetIpAddrTable = function(
    pIpAddrTable: PMIB_IPADDRTABLE;   // buffer for mapping table
    pdwSize: PULONG;                  // size of buffer
    bOrder: BOOL                      // sort the table
    ): DWORD; stdcall; 

implementation

end.

