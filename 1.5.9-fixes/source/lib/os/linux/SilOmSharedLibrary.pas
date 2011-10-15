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

unit SilOmSharedLibrary;

{$I Defines.inc}

interface

uses
  SysUtils,
  SilOsTypes,
  SilOiHandle,
  SilOkSharedLibrary,
  SilOmHandle;

type
  TSilLinuxSharedLibrary = class(TSilSharedLibrary)
  protected
    function DoCreateHandle(const Value: THandle; const MustFree: Boolean = True): IHandle; override;
  protected // ILibrary
    function DoGet(const Name: String): Pointer; override;
    function DoLoad(const Name: String): IHandle; override;
  end;

  TSilLibraryHandle = class(TSilHandle, IHandle)
  protected
    procedure HandleIsValid(var Result: Boolean); override;
    procedure HandleClose; override;
  end;

  TLibrary = class(TSilLinuxSharedLibrary) end {$IFDEF USE_DEPRECATED} deprecated {$ENDIF};

implementation

uses
  dl,
  Types;

{ TSilLinuxSharedLibrary }

function TSilLinuxSharedLibrary.DoCreateHandle(const Value: THandle; const MustFree: Boolean): IHandle;
begin
  Result := TSilLibraryHandle.Create(Value, MustFree);
end;

function TSilLinuxSharedLibrary.DoGet(const Name: String): Pointer;
begin
  Result := dl.dlsym(Pointer(Handle.Value), PChar(Name));
end;

function TSilLinuxSharedLibrary.DoLoad(const Name: String): IHandle;
begin
  Result := DoCreateHandle(THandle(dl.dlOpen(PChar(Name), RTLD_LAZY)));
end;

{ TSilLibraryHandle }

procedure TSilLibraryHandle.HandleClose;
begin
  dl.dlclose(Pointer(Handle));
  inherited;
end;

procedure TSilLibraryHandle.HandleIsValid(var Result: Boolean);
begin
  inherited;
  Result := Result and Assigned(Pointer(Handle));
end;

end.
