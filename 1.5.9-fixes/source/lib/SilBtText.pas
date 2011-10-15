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

unit SilBtText;

interface

{$include Defines.inc}

uses
  SilBkTool;

type
  Text = class (Tool)
    class function IsEqual(const a, b: String): Boolean;
    class function Compare(const a, b: String): Integer; overload;
    class function Compare(const a, b: String; Count: LongWord): Integer; overload;
    class function Pos(const Substr, Buffer: String; const Init: Integer = 1): Integer;
    class function StartsWith(const substr, buffer: string): boolean;
  end;

implementation

uses
  SysUtils,
  SilBtStr;

{ Text }

class function Text.Compare(const a, b: String): Integer;
begin
  Result := SysUtils.CompareText(a, b);
end;

class function Text.Compare(const a, b: String; Count: LongWord): Integer;
begin
  Result := Sysutils.StrLIComp(PChar(a), PChar(b), Count);
end;

class function Text.IsEqual(const a, b: String): Boolean;
begin
  Result := SysUtils.CompareText(a, b) = 0;
end;

class function Text.Pos(const Substr, Buffer: String; const Init: Integer): Integer;
begin
  Result := Str.Pos(Str.ToLower(SubStr), Str.ToLower(Buffer), Init);
end;

class function Text.StartsWith(const substr, buffer: string): boolean;
begin
  Result := Compare(substr, buffer, length(substr)) = 0;
end;

end.
