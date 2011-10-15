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

unit SilOmWntIpHlpApi;

{$I Defines.inc}

interface

uses
  SilLkObject,
  SilOiSharedLibrary,
  SilOhWntIpHlpApi,
  SilOiWntIpHlpApi;

type
  TIPHelper = class (
    // extends
    TSilObject,
    // implements
    IIPHelper)
  private
    FLib: ISharedLibrary;
  protected // IIPHelper
    function DoGetIpAddrTable: TGetIpAddrTable;
    function DoGetIpNetTable: TGetIpNetTable;
  public
    constructor Create(const FileName: String);
  end;

implementation

uses
  Windows,
  SilOtTool;

function LocalGetIpAddrTable(
  pIpAddrTable:   PMIB_IPADDRTABLE;
  pdwSize:        PULONG;
  bOrder:         BOOL): DWORD; stdcall;
begin
  Result := ERROR_NOT_SUPPORTED;
end;

function LocalGetIpNetTable(
  pIpAddrTable:   PMIB_IPNETTABLE;        
  pdwSize:        PULONG;                 
  bOrder:         BOOL): DWORD; stdcall;
begin
  Result := ERROR_NOT_SUPPORTED;
end;
  
{ TIPHelper }

constructor TIPHelper.Create(const FileName: String);
begin
  inherited Create;
  FLib := OS.SharedLibrary.Load(FileName);
end;

function TIPHelper.DoGetIpAddrTable: TGetIpAddrTable;
begin
  if not FLib.Bind('GetIpAddrTable', 0, Result) then
    Result := LocalGetIpAddrTable;
end;

function TIPHelper.DoGetIpNetTable: TGetIpNetTable;
begin
  if not FLib.Bind('GetIpNetTable', 1, Result) then
    Result := LocalGetIpNetTable;
end;

end.
 