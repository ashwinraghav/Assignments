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

unit SilExport;

{$INCLUDE Defines.inc}

interface

uses
  SilModule,
  SilLiGlobalServices;

function GetGlobalServicesV1: IGlobalServicesV1; stdcall;
function GetGlobalServicesV2: IGlobalServiceListV2; stdcall;

{$IFDEF USE_EXPORTS}
exports
  SysGetMem name 'SilGetMem',
  SysFreeMem name 'SilFreeMem',
  SysReallocMem name 'SilReallocMem',
{$IFDEF USE_PLATFORM}
{$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  GetHeapStatus name 'SilHeapStatus',
{$IFDEF USE_PLATFORM}
{$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
  GetGlobalServicesV1 name 'SilGlobalServices',
  GetGlobalServicesV2 name 'SilGlobalServiceList';
{$ENDIF}

implementation

uses
  SilLfGlobalServices;

function GetGlobalServicesV1: IGlobalServicesV1;
begin
  Result := SilLfGlobalServices.Global as IGlobalServicesV1;
end;

function GetGlobalServicesV2: IGlobalServiceListV2;
begin
  Result := SilLfGlobalServices.Global as IGlobalServiceListV2;
end;
  
end.
 