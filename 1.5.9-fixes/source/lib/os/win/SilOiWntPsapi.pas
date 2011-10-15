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

unit SilOiWntPsapi;

{$I Defines.inc}

interface

uses
  SilOhWntPsapi;

type
  IPsapi = interface
    ['{77D25D69-A0E6-4591-B9B1-FC8B9301F312}']
    function DoEnumProcesses: TEnumProcesses;
    function DoEnumProcessModules: TEnumProcessModules;
    function DoGetModuleFileNameExA: TGetModuleFileNameExA;
    function DoGetModuleFileNameExW: TGetModuleFileNameExW;
    function DoGetModuleBaseNameA: TGetModuleBaseNameA;
    function DoGetModuleBaseNameW: TGetModuleBaseNameW;
    function DoGetProcessImageFileNameA: TGetProcessImageFileNameA;
    function DoGetProcessImageFileNameW: TGetProcessImageFileNameW;
    property EnumProcesses: TEnumProcesses read DoEnumProcesses;
    property EnumProcessModules: TEnumProcessModules read DoEnumProcessModules;
    property GetModuleFileNameExA: TGetModuleFileNameExA read DoGetModuleFileNameExA;
    property GetModuleFileNameExW: TGetModuleFileNameExW read DoGetModuleFileNameExW;
    property GetModuleBaseNameA: TGetModuleBaseNameA read DoGetModuleBaseNameA;
    property GetModuleBaseNameW: TGetModuleBaseNameW read DoGetModuleBaseNameW;
    property GetProcessImageFileNameA: TGetProcessImageFileNameA read DoGetProcessImageFileNameA;
    property GetProcessImageFileNameW: TGetProcessImageFileNameW read DoGetProcessImageFileNameW;
  end;

implementation
end.
 