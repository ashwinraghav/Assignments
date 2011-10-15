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

unit SilBtPoint;

{$I Defines.inc}

interface

uses
  SilOeTypes,
  SilBkTool;

type
  Point = class(Tool)
    class function Make(X, Y: Integer): TPoint;
    class function Add(const P1, P2: TPoint): TPoint;
    class function Negate(const P: TPoint): TPoint;
    class function Multiply(const P: TPoint; const Num, Den: TPoint): TPoint;
    class function Equals(const  P1, P2: TPoint): Boolean;
    class function Max(const P1, P2: TPoint): TPoint;
    class function Min(const P1, P2: TPoint): TPoint;
  end;

implementation

uses
  SilBtInt;

{ Point }

class function Point.Make(X, Y: Integer): TPoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

class function Point.Add(const P1, P2: TPoint): TPoint;
begin
  Result := Make(P1.X + P2.X, P1.Y + P2.Y);
end;

class function Point.Negate(const P: TPoint): TPoint;
begin
  Result := Make(-P.X, -P.Y);
end;

class function Point.Multiply(const P: TPoint; const Num, Den: TPoint): TPoint;
begin
  Result := Make(P.X * Num.X div Den.X, P.Y * Num.Y div Den.Y);
end;

class function Point.Equals(const  P1, P2: TPoint): Boolean;
begin
  Result := (P1.X = P2.X) and (P1.Y = P2.Y);
end;

class function Point.Max(const P1, P2: TPoint): TPoint;
begin
  Result.X := Int.Max(P1.X, P2.X);
  Result.Y := Int.Max(P1.Y, P2.Y);
end;

class function Point.Min(const P1, P2: TPoint): TPoint;
begin
  Result.X := Int.Min(P1.X, P2.X);
  Result.Y := Int.Min(P1.Y, P2.Y);
end;

end.
