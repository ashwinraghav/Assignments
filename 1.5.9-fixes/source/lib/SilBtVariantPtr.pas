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

unit SilBtVariantPtr;

{$I Defines.inc}

interface

uses
  SilBkPtr;

type
  VariantHandler = class(DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

implementation

uses
  SilBtVart;

{ VariantHandler }

class procedure VariantHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PVariant(Result));
end;

class procedure VariantHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PVariant(Value));
end;

class procedure VariantHandler.Clear(var Obj; const Data: Pointer);
begin
  Vart.Clear(PVariant(@Obj)^);
end;

class procedure VariantHandler.Copy(const Source, Dest, Data: Pointer);
begin
  Vart.Copy(PVariant(Dest)^, PVariant(Source)^);
end;

class function VariantHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := Vart.Compare(PVariant(Value1)^, PVariant(Value2)^);
end;

end.
