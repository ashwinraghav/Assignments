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

unit SilSfFirebirdClient;

{$INCLUDE Defines.inc}

interface

uses
  SilSeFirebirdClient,
  SilSjFirebirdClient;

function fb: FirebirdClientType;
procedure Initialize(var Rec: TCTimeStructure); overload;
function XSQLDA_LENGTH(n: Long; version: Integer = SQLDA_VERSION1): Long;
(*)function getb(p: PBSTREAM): Char;
function putb(x: Char; p: PBSTREAM): Int;
function putbx(x: Char; p: PBSTREAM): Int;(*)

implementation

uses
  SilStFirebirdClient;

function fb: FirebirdClientType;
begin
  Result := SilStFirebirdClient.Firebird;
end;
  
procedure Initialize(var Rec: TCTimeStructure);
begin
  with Rec do
  begin
    tm_sec    := 0;
    tm_min    := 0;
    tm_hour   := 0;
    tm_mday   := 0;
    tm_mon    := 0;
    tm_year   := 0;
    tm_wday   := 0;
    tm_yday   := 0;
    tm_isdst  := 0;
  end;
end;

function XSQLDA_LENGTH(n: Long; version: Integer): Long;
begin
  Result := 0;
  case version of
    SQLDA_VERSION2: Result := SizeOf(TXSQLDA_V2) + ((n - 1) * SizeOf(TXSQLVAR_V2));
    SQLDA_VERSION1: Result := SizeOf(TXSQLDA_V1) + ((n - 1) * SizeOf(TXSQLVAR_V1));
    else            Exit;
  end;

end;

(*)function getb(p: PBSTREAM): Char;
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt >= 0) then
  begin
    result := Char(Int(p^.bstr_ptr^) and Int($FF));
    Inc(p^.bstr_ptr);
  end else
    result := Char(BLOB_get(p));
end;

function putb(x: Char; p: PBSTREAM): Int;
begin
  Dec(p^.bstr_cnt);
  if (x = Chr(Int('n') - Int('a'))) or (p^.bstr_cnt = 0) then
    result := BLOB_put(x, p)
  else begin
    p^.bstr_ptr^ := Char(x);
    result := UInt(x);
    Inc(p^.bstr_ptr^);
  end;
end;

function putbx(x: Char; p: PBSTREAM): Int;
begin
  Dec(p^.bstr_cnt);
  if (p^.bstr_cnt = 0) then
    result := BLOB_put(x, p)
  else begin
    p^.bstr_ptr^ := Char(x);
    Inc(p^.bstr_ptr^);
    result := UInt(x);
  end;
end;(*)

end.
