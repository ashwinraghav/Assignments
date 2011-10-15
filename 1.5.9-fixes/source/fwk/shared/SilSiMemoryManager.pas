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

unit SilSiMemoryManager;

interface

const   
  GsMemoryManager: TGUID = '{F783DFDC-B314-4270-8828-209F2FDF8841}';

type
  ISharedMemoryManager = interface
    ['{39A8D618-C400-438A-A0B9-A7089753CAA0}']
    function GetMemoryStatus: THeapStatus; 
    function GetInstance: LongWord;
    function GetIsAssigned: Boolean;
    function GetManager: TMemoryManager;
    property Instance: LongWord read GetInstance;
    property IsAssigned: Boolean read GetIsAssigned;
    property Manager: TMemoryManager read GetManager;
    property MemoryStatus: THeapStatus read GetMemoryStatus; 
  end;

implementation
end.
 