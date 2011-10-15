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

unit SilOfEnvironment;

{$I Defines.inc}

interface

uses
  SilBeTypes;

type
  TParseCallback = procedure(const Context; const Name: string) of object;

function GetStrings(out Value: PChar): Boolean;
procedure FreeStrings(var Value: PChar);
function GetValue(const Name: string; out Value: Variant): Boolean;
function SetValue(const Name: string; const Value: Variant): Boolean;
function GetString(const Name: string; out Value: string): Boolean;
function SetString(const Name, Value: string): Boolean;
procedure ParseStrings(const Context; const Buffer: PChar; const Callback: TParseCallback);

implementation

uses
  SysUtils,
  Libc,
  SilBtStr,
  SilBtVart;

function GetStrings(out Value: PChar): Boolean;
begin
  Value := System.envp^;
  Result := Value <> nil;
end;

procedure FreeStrings(var Value: PChar);
begin
  Value := nil;
end;

function GetValue(const Name: string; out Value: Variant): Boolean;
var
  ValueStr: string;
begin
  Result := GetString(Name, ValueStr);

  if Result then
    Value := ValueStr else
    Value := Vart.Null;
end;

function SetValue(const Name: string; const Value: Variant): Boolean;
begin
  Result := SetString(Name, Vart.ToStr(Value));
end;

function GetString(const Name: string; out Value: string): Boolean;
var
  iSize: Integer;
  pValue: PChar;
begin
  pValue := Libc.getenv(PChar(Name));
  Result := Assigned(pValue);

  if Result then
  begin
    iSize := SysUtils.StrLen(pValue);
    Result := iSize > 0;

    if Result then
      System.SetString(Value, pValue, iSize);
  end;
end;

function SetString(const Name, Value: string): Boolean;
begin
  if Length(Value) > 0 then
    Libc.setenv(PChar(Name), PChar(Value), 1 {True}) else
    Libc.unsetenv(PChar(Name));
  Result := (Libc.errno = 0);
end;

procedure ParseStrings(const Context; const Buffer: PChar; const Callback: TParseCallback);
var
  BufPtr: PChar;
  NextPtr: PChar;
  PosPtr: PChar;
  Name: string;
begin
  BufPtr := Buffer;
  repeat
    NextPtr := SysUtils.StrEnd(BufPtr) + 1;
    if NextPtr^ <> #0 then
    begin
      PosPtr := SysUtils.StrPos(BufPtr, '=');
      if PosPtr <> nil then
      begin
        System.SetString(Name, BufPtr, PosPtr - BufPtr);
        Callback(Context, Name);
      end;
    end;
    BufPtr := NextPtr;
  until BufPtr^ = #0;
end;

end.
