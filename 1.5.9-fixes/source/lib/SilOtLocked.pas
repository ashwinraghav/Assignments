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

unit SilOtLocked;

{$INCLUDE Defines.inc}

interface

uses
  SilBeTypes,
  SilBkTool,
  SilOsTypes;

type
  SilLockedTool = class(Tool)
    class function Increment(var Ref: Integer): Integer; overload;
    class function Increment(var Ref: Integer; Delta: Integer; Preincrement: Boolean = True): Integer; overload;
    class function Increment(Ref: PInteger): Integer; overload;
    class function Increment(Ref: PInteger; Delta: Integer; Preincrement: Boolean = True): Integer; overload;
    class function Decrement(var Ref: Integer): Integer; overload;
    class function Decrement(var Ref: Integer; Delta: Integer; Predecrement: Boolean = True): Integer; overload;
    class function Decrement(Ref: PInteger): Integer; overload;
    class function Decrement(Ref: PInteger; Delta: Integer; Predecrement: Boolean = True): Integer; overload;
    class function Exchange(var Ref: Integer; Value: Integer): Integer; overload;
    class function Exchange(Ref: PInteger; Value: Integer): Integer; overload;
  end;

implementation

uses
  SilBtInt, 
  SilOfLocked;

{ SilLockedTool }

class function SilLockedTool.Increment(var Ref: Integer): Integer;
begin
  Result := SilOfLocked.InterlockedIncrement(Ref);
end;

class function SilLockedTool.Increment(var Ref: Integer; Delta: Integer; Preincrement: Boolean): Integer;
begin
  Result := SilOfLocked.InterlockedExchangeAdd(Ref, Delta);
  if Preincrement then Inc(Result, Delta);
end;

class function SilLockedTool.Increment(Ref: PInteger): Integer;
begin
  Result := SilOfLocked.InterlockedIncrement(Ref^);
end;

class function SilLockedTool.Increment(Ref: PInteger; Delta: Integer; Preincrement: Boolean): Integer;
begin
  Result := SilOfLocked.InterlockedExchangeAdd(Ref^, Delta);
  if Preincrement then Inc(Result, Delta);
end;

class function SilLockedTool.Decrement(var Ref: Integer): Integer;
begin
  Result := SilOfLocked.InterlockedDecrement(Ref);
end;

class function SilLockedTool.Decrement(var Ref: Integer; Delta: Integer; Predecrement: Boolean): Integer;
begin
  Result := SilOfLocked.InterlockedExchangeAdd(Ref, -Delta);
  if Predecrement then Inc(Result, -Delta);
end;

class function SilLockedTool.Decrement(Ref: PInteger): Integer;
begin
  Result := SilOfLocked.InterlockedDecrement(Ref^);
end;

class function SilLockedTool.Decrement(Ref: PInteger; Delta: Integer; Predecrement: Boolean): Integer;
begin
  Result := SilOfLocked.InterlockedExchangeAdd(Ref^, -Delta);
  if Predecrement then Inc(Result, -Delta);
end;

class function SilLockedTool.Exchange(var Ref: Integer; Value: Integer): Integer;
begin
  Result := SilOfLocked.InterlockedExchange(Ref, Value);
end;

class function SilLockedTool.Exchange(Ref: PInteger; Value: Integer): Integer;
begin
  Result := SilOfLocked.InterlockedExchange(Ref^, Value);
end;

end.
