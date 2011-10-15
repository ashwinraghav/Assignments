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

unit SilBtChr;

{$I Defines.inc}

interface

uses
  SilBcChr,
  SilBkTool;

type
  Chr = class(Tool)
    class function Upper(C: Char): Char;
    class function Lower(C: Char): Char;
    class function ToByte(C: Char): Byte;
    class function Name(C: Char): string;
    class function IsAlpha(C: Char): Boolean;
    class function IsNumber(C: Char): Boolean;
  end;

implementation

uses
  SilAfChr,
  SysUtils;

class function Chr.Upper(C: Char): Char;
begin
  Result := ChrUpper(C);
end;

class function Chr.Lower(C: Char): Char;
begin
  Result := ChrLower(C);
end;

class function Chr.ToByte(C: Char): Byte;
begin
  Result := Byte(C);
end;

class function Chr.Name(C: Char): string;
begin
  if Ord(C) in [Low(AsciiChars) .. High(AsciiChars)] then
    Result := AsciiChars[Byte(C)] else
    Result := '#' + IntToStr(Ord(C));
end;

class function Chr.IsAlpha(C: Char): Boolean;
begin
  Result := ChrUpper(C) in ['A'..'Z'];
end;

class function Chr.IsNumber(C: Char): Boolean;
begin
  Result := C in ['0'..'9'];
end;

end.
 