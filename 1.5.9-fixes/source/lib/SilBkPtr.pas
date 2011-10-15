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

unit SilBkPtr;

{$I Defines.inc}

interface

uses
  SilBkTool;

type
  HandlerType = class of BaseHandler;
  BaseHandler = class(Tool)
    class function Check(Value: HandlerType): HandlerType;
    class procedure ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer = nil); virtual; abstract;
    class procedure ToObj(const Ptr: Pointer; var Obj; const Data: Pointer = nil); virtual; abstract;
    class procedure Clear(var Obj; const Data: Pointer = nil); virtual; abstract;
    class procedure Free(var Ptr: Pointer; const Data: Pointer = nil); virtual; abstract;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; virtual; abstract;
  end;

  PointerType = class of PointerHandler;
  PointerHandler = class(BaseHandler)
    class procedure ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer = nil); override;
    class procedure ToObj(const Ptr: Pointer; var Obj; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class procedure Free(var Ptr: Pointer; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

  ObjectType = class of DataHandler;
  DataHandler = class(BaseHandler)
    class procedure ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer = nil); override;
    class procedure ToObj(const Ptr: Pointer; var Obj; const Data: Pointer = nil); override;
    class procedure Free(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); virtual; abstract;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); virtual; abstract;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); virtual; abstract; // copia el contenido
    class procedure New(const Value: Pointer; var Result: Pointer; const Data: Pointer = nil); virtual; // aloca un puntero y le copia el contenido desde Value
  end;

implementation

uses
  SilBtVoidPtr;

{ BaseHandler }

class function BaseHandler.Check(Value: HandlerType): HandlerType;
begin
  Result := Value;
  if Result = nil then
    Result := VoidHandler;
end;

{ DataHandler }

class procedure DataHandler.ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer);
begin
  New(@Obj, Ptr, Data);
end;

class procedure DataHandler.ToObj(const Ptr: Pointer; var Obj; const Data: Pointer);
begin
  Copy(Ptr, @Obj, Data);
end;

class procedure DataHandler.New(const Value: Pointer; var Result: Pointer; const Data: Pointer);
begin
  Alloc(Result, Data);
  Copy(Value, Result, Data);
end;

class procedure DataHandler.Free(var Value: Pointer; const Data: Pointer);
begin
  if Value <> nil then
  try
    Clear(Value^, Data);
  finally
    Dispose(Value, Data);
    Value := nil;
  end;
end;

{ PointerHandler }

class procedure PointerHandler.ToPtr(const Obj; out Ptr: Pointer; const Data: Pointer);
begin
  Ptr := @Obj;
end;

class procedure PointerHandler.ToObj(const Ptr: Pointer; var Obj; const Data: Pointer);
begin
  Pointer(Obj) := Ptr;
end;

class procedure PointerHandler.Clear(var Obj; const Data: Pointer);
begin
  Pointer(Obj) := nil;
end;

class procedure PointerHandler.Free(var Ptr: Pointer; const Data: Pointer);
begin
  Ptr := nil;
end;

class function PointerHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  Result := Integer(Value1) - Integer(Value2);
end;

end.
