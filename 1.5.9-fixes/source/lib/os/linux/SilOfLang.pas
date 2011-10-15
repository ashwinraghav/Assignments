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

unit SilOfLang;

{$I Defines.inc}

interface

uses
  SilOeLang;

function LocaleGetCurrent(const Level: TLocaleLevel): Locale; 
function LocaleIsValid(const Value: Locale; const Check: TLocaleCheck): Boolean;

implementation

uses
  SysUtils;

(*)const
  CheckMap: array[TLocaleCheck] of LongWord = (LCID_INSTALLED, LCID_SUPPORTED);(*)

function LocaleGetCurrent(const Level: TLocaleLevel): Locale;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LocaleGetCurrent']);

(*)  case Level of
    SystemLevel:
      Result := Windows.GetSystemDefaultLCID();
    UserLevel:
      Result := Windows.GetUserDefaultLCID();
  else
      Result := Windows.GetThreadLocale();
  end;(*)
end;

function LocaleIsValid(const Value: Locale; const Check: TLocaleCheck): Boolean;
begin
  raise Exception.CreateFmt('%s: not implemented', ['LocaleIsValid']);
(*)  Result := Windows.IsValidLocale(Value, CheckMap[Check]);(*)
end;

end.
