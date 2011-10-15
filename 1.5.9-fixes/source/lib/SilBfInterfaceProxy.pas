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

unit SilBfInterfaceProxy;

{$INCLUDE Sil.inc}

interface

uses
  SilBeInterfaceProxy,
  SilBhInterfaceProxy;

procedure DoFreeInterface(Proxy: PInterfaceProxy);
function DoQueryInterface(const Instance: IUnknown; const IID: TGUID; out Obj): Integer; stdcall;
function DoAddRef(const Instance: IUnknown): Integer; stdcall;
function DoRelease(const Instance: IUnknown): Integer; stdcall;

implementation

uses
  SysUtils,
  SilAfInterfaceCode,
  SilAfProxyInvoke,
  SilOfLocked;

procedure DoFreeInterface(Proxy: PInterfaceProxy);
begin
  Proxy.Controller := nil;
  Proxy.VmtProxy := nil;
  Proxy.Methods.List := nil;
  Dispose(Proxy);
end;

function DoQueryInterface(const Instance: IUnknown; const IID: TGUID; out Obj): Integer; stdcall;
var
  Proxy: PInterfaceProxy absolute Instance;
begin
  if SysUtils.IsEqualGUID(IID, IUnknown) or SysUtils.IsEqualGUID(IID, Proxy.IID) then
  begin
    IUnknown(Obj) := Instance;
    Result := S_OK;
  end else
    Result := Proxy.Controller.QueryInterface(IID, Obj);
end;

function DoAddRef(const Instance: IUnknown): Integer; stdcall;
var
  Proxy: PInterfaceProxy absolute Instance;
begin
  Result := InterlockedIncrement(Proxy.RefCount);
end;

function DoRelease(const Instance: IUnknown): Integer; stdcall;
var
  Proxy: PInterfaceProxy absolute Instance;
begin
  Result := InterlockedDecrement(Proxy.RefCount);
  if Result = 0 then DoFreeInterface(Proxy); 
end;

end.
