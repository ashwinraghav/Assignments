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

unit SilBtInterfacePtr;

{$I Defines.inc}

interface

uses
  SilBkPtr;

type
  InterfaceHandler = class(PointerHandler)
    class procedure ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer = nil); override;
    class procedure ToObj(const Ptr: Pointer; var Obj; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class procedure Free(var Ptr: Pointer; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

implementation

uses
  SilLtReference;

{ InterfaceHandler }

class procedure InterfaceHandler.ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer);
begin
  Pointer(Ptr) := nil; 
  IUnknown(Ptr) := IUnknown(Obj);
end;

class procedure InterfaceHandler.ToObj(const Ptr: Pointer; var Obj; const Data: Pointer);
begin
  IUnknown(Obj) := IUnknown(Ptr);
end;

class procedure InterfaceHandler.Clear(var Obj; const Data: Pointer);
begin
  IUnknown(Obj) := nil; 
end;

class procedure InterfaceHandler.Free(var Ptr: Pointer; const Data: Pointer);
begin
  IUnknown(Ptr) := nil;
end;

class function InterfaceHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := Ref.Compare(IUnknown(Value1), IUnknown(Value2), Data);
end;

end.
