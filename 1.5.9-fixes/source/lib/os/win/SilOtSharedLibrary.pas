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

unit SilOtSharedLibrary;

{$I Defines.inc}

interface

uses
  SilOsTypes,
  SilOjSharedLibrary,
  SilOiSharedLibrary,
  SilOiLibraryBinding;

type
{$WARNINGS OFF}
  WindowsSharedLibrary = class(SilSharedLibrary)
    class function Load(const FileName: String; RaiseOnError: Boolean = true): ISharedLibrary; override;
    class function CreateBinding(const FileName: String): ILibraryBinding; override; {$IFDEF USE_DEPRECATED} deprecated; {$ENDIF}
    class function GetAddress(const Name: string; out Address; const Handle: THandle = 0): Boolean; override;
  end;
{$WARNINGS ON}

implementation

uses
  Windows,
  SilOmLibraryBinding,
  SilOsClasses;

{ WindowsSharedLibrary }

{$WARNINGS OFF}
class function WindowsSharedLibrary.CreateBinding(const FileName: String): ILibraryBinding;
begin
  Result := TSilLibraryBinding.Create(FileName);
end;
{$WARNINGS ON}

class function WindowsSharedLibrary.Load(const FileName: String; RaiseOnError: Boolean): ISharedLibrary;
begin
  Result := TSilOsSharedLibrary.Create;
  Result.Load(FileName, RaiseOnError);
end;

class function WindowsSharedLibrary.GetAddress(const Name: string; out Address; const Handle: THandle): Boolean;
begin
  Pointer(Address) := Windows.GetProcAddress(Handle, PChar(Name));
  Result := Assigned(Pointer(Address));
end;

end.
 