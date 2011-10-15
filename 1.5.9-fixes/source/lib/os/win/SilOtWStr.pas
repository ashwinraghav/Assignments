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

unit SilOtWStr;

{$I Defines.inc}

interface

uses
  SilBeTypes,
  SilOjWStr;

type
  WindowsWStr = class(SilWStr) 
    class function Len(const Buffer: WideString): Integer; overload; override;
    class function Len(const Buffer: PWideChar; MaxLen: integer = -1): Integer; overload; override;
    class function Compare(const Buffer, Compare: WideString; IgnoreCase: Boolean = True): Integer; override;
    class function CompareText(const Buffer, Compare: WideString; Complete: Boolean {$IFDEF USE_DEFPARAMS} = false {$ENDIF}): Integer; override; 
    class function Compare(const Buffer, Compare: PWideChar; IgnoreCase: Boolean = True; Dummy: Integer = 0): Integer; override;
    class function CompareText(const Buffer, Compare: PWideChar; Complete: Boolean {$IFDEF USE_DEFPARAMS} = false {$ENDIF}; Dummy: Integer = 0): Integer; override;
  end;

implementation

uses
  Windows;

{ WindowsWStr }

class function WindowsWStr.Len(const Buffer: WideString): Integer;
begin
  Result := Length(Buffer);
end;

class function WindowsWStr.Len(const Buffer: PWideChar; MaxLen: integer): Integer;
begin
  Result := lstrlenW(Buffer);
  if (MaxLen > 0) and (Result > MaxLen) then Result := MaxLen;
end;

class function WindowsWStr.Compare(const Buffer, Compare: WideString; IgnoreCase: Boolean): Integer;
var
  Flags: Cardinal; 
begin
  if IgnoreCase then
    Flags := NORM_IGNORECASE
  else
    Flags := 0;

  Result := CompareStringW(LOCALE_USER_DEFAULT, Flags, PWideChar(Buffer), Len(Buffer), PWideChar(Compare), Len(Compare)) - 2;
end;

class function WindowsWStr.CompareText(const Buffer, Compare: WideString; Complete: Boolean): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PWideChar(Buffer), Len(Buffer), PWideChar(Compare), Len(Compare)) - 2;
end;

class function WindowsWStr.Compare(const Buffer, Compare: PWideChar; IgnoreCase: Boolean; Dummy: Integer): Integer;
var
  Flags: Cardinal;
begin
  if IgnoreCase then
    Flags := NORM_IGNORECASE
  else
    Flags := 0;

  Result := CompareStringW(LOCALE_USER_DEFAULT, Flags, Buffer, Len(Buffer), Compare, Len(Compare)) - 2;
end;

class function WindowsWStr.CompareText(const Buffer, Compare: PWideChar; Complete: Boolean; Dummy: Integer): Integer;
begin
  Result := CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE, Buffer, Len(Buffer), Compare, Len(Compare)) - 2;
end;

end.
