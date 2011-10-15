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

unit SilBtStringPtr;

{$I Defines.inc}

interface

uses
  SilBkPtr;

type
  StringHandler = class(DataHandler)
    class procedure Alloc(var Result: Pointer; const Data: Pointer = nil); override;
    class procedure Dispose(var Value: Pointer; const Data: Pointer = nil); override;
    class procedure Copy(const Source: Pointer; const Dest: Pointer; const Data: Pointer = nil); override;
    class procedure Clear(var Obj; const Data: Pointer = nil); override;
    class function Compare(const Value1; const Value2; Data: Pointer = nil): Integer; override;
  end;

implementation

uses
  SilBtStr,
  SilBtText;

{ StringHandler }

class procedure StringHandler.Alloc(var Result: Pointer; const Data: Pointer);
begin
  System.New(PString(Result));
end;

class procedure StringHandler.Dispose(var Value: Pointer; const Data: Pointer);
begin
  System.Dispose(PString(Value));
end;

class procedure StringHandler.Copy(const Source, Dest, Data: Pointer);
begin
  PString(Dest)^ := PString(Source)^;
end;

class procedure StringHandler.Clear(var Obj; const Data: Pointer);
begin
  SetLength(PString(@Obj)^, 0);
end;

class function StringHandler.Compare(const Value1; const Value2; Data: Pointer): Integer;
begin
  if (Data = nil) or not Boolean(Data^) then  //... more fruit ...
    Result := Str.Compare(PString(Value1)^, PString(Value2)^) else
    Result := Text.Compare(PString(Value1)^, PString(Value2)^);
end;

end.
