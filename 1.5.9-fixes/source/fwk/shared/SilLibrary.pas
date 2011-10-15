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

unit SilLibrary;

{$include Defines.inc}

interface

uses
  SilDLL,
  SilSiSharedObject;

function SilCreateObject(const ClassID: TGuid; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; stdcall;
function SilClassQuery(const ClassID: TGuid): Boolean; stdcall;
function SilGetClassObject(const ClassID: TGuid; const IID: TGUID; out Obj): Integer; stdcall;
function SilRegister: Integer; stdcall;
function SilUnregister: Integer; stdcall;

{$IFDEF USE_EXPORTS}
exports
  SilCreateObject,
  SilClassQuery,
  SilGetClassObject,
  SilRegister,
  SilUnregister;
{$ENDIF}

implementation

uses
  SilOsTypes,
  SilLfGlobalServices,
  SilSfSharedSignature,
  SilSfSharedFactoryList;
  
function SilCreateObject(const ClassID: TGuid; const Owner: IUnknown = nil; const Controller: IUnknown = nil): IUnknown; stdcall;
begin
  with FactoryList do Get(ClassID).CreateObject(Owner, IUnknown, Result, Controller);
end;

function SilClassQuery(const ClassID: TGuid): Boolean; stdcall;
var
  Dummy: ISharedFactory;
begin
  with FactoryList do Result := Find(ClassID, Dummy);
end;

function SilGetClassObject(const ClassID: TGuid; const IID: TGUID; out Obj): Integer; stdcall;
begin
  with FactoryList do
  try
    Result := Get(ClassID).QueryInterface(IID, Obj);
  except
    Result := REGDB_E_CLASSNOTREG;
  end;
end;

function SilRegister: Integer; stdcall;
begin
  Result := E_NOTIMPL;
end;

function SilUnregister: Integer; stdcall;
begin
  Result := E_NOTIMPL;
end;

end.
 