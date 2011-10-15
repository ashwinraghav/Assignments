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

unit SilOmWntPsapi;

{$I Defines.inc}

interface

uses
  SilLkInterfaced,
  SilOiSharedLibrary,
  SilOhWntPsapi,
  SilOiWntPsapi;

type
  TSilWntPsapiLibrary = class (
    // extends
    TSilInterfacedObject,
    // implements
    IPsApi)
  private
    FLibrary: ISharedLibrary;
  protected // IPsApi
    function DoEnumProcesses: TEnumProcesses;
    function DoEnumProcessModules: TEnumProcessModules;
    function DoGetModuleFileNameExA: TGetModuleFileNameExA;
    function DoGetModuleFileNameExW: TGetModuleFileNameExW;
    function DoGetModuleBaseNameA: TGetModuleBaseNameA;
    function DoGetModuleBaseNameW: TGetModuleBaseNameW;
    function DoGetProcessImageFileNameA: TGetProcessImageFileNameA;
    function DoGetProcessImageFileNameW: TGetProcessImageFileNameW;
  public
    constructor Create(const FileName: String = CPsapi);
  end;

implementation

{ TSilWntPsapiLibrary }

uses
  SilOtTool;

constructor TSilWntPsapiLibrary.Create(const FileName: String);
begin
  inherited Create;
  FLibrary := OS.SharedLibrary.Load(FileName);
end;

function TSilWntPsapiLibrary.DoEnumProcesses: TEnumProcesses;
begin
  FLibrary.Bind('EnumProcesses', 0, Result, true);
end;

function TSilWntPsapiLibrary.DoEnumProcessModules: TEnumProcessModules;
begin
  FLibrary.Bind('EnumProcessModules', 1, Result, true);
end;

function TSilWntPsapiLibrary.DoGetModuleFileNameExA: TGetModuleFileNameExA;
begin
  FLibrary.Bind('GetModuleFileNameExA', 2, Result, true);
end;

function TSilWntPsapiLibrary.DoGetModuleFileNameExW: TGetModuleFileNameExW;
begin
  FLibrary.Bind('GetModuleFileNameExW', 3, Result, true);
end;

function TSilWntPsapiLibrary.DoGetModuleBaseNameA: TGetModuleBaseNameA;
begin
  FLibrary.Bind('GetModuleBaseNameA', 4, Result, true);
end;

function TSilWntPsapiLibrary.DoGetModuleBaseNameW: TGetModuleBaseNameW;
begin
  FLibrary.Bind('GetModuleBaseNameW', 5, Result, true);
end;

function TSilWntPsapiLibrary.DoGetProcessImageFileNameA: TGetProcessImageFileNameA;
begin
  FLibrary.Bind('GetProcessImageFileNameA', 6, Result, true);
end;

function TSilWntPsapiLibrary.DoGetProcessImageFileNameW: TGetProcessImageFileNameW;
begin
  FLibrary.Bind('GetProcessImageFileNameW', 7, Result, true);
end;

end.
