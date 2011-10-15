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

unit SilOfComputer;

{$I Defines.inc}

interface

uses
  SilOcTypes,
  SilBeTypes,
  SilOiComputer,
  SilOsWntNetapi32;

function DoGetComputerName: string; overload;
function DoGetUserName: string; overload;
function DoGetUserName(Format: TNameFormat): string; overload;
function DoServerTypeToComputerKind(const ServerType: TNetServerType): TComputerKinds;
function DoKindsToStr(Kinds: TComputerKinds; const Sep: string = sLineBreak): string;
function DoMacToStr(const Mac: LargeInt): string;

implementation

uses
  SilBtStr,
  SilBtMem,
  SilOsError,
  SilOdComputer,
  SilOgComputer,
  Windows;

function DoGetComputerName: string;
var
  Size: LongWord;
begin
  Size := MAX_COMPUTERNAME_LENGTH + 1;
  SetLength(Result, Size);
  OsError.Check(Windows.GetComputerName(PChar(Result), Size), 'SilOfComputer.DoGetComputerName [Windows.GetComputerName]');
  SetLength(Result, Size);
end;

function DoGetUserName: string;
var
  Buffer: array[0..1023] of Char;
  Size: DWORD;
begin
  Size := SizeOf(Buffer);
  OsError.Check(Windows.GetUserName(Buffer, Size));
  if Size > 0 then
    SetString(Result, Buffer, Size - 1) else
    SetLength(Result, 0);
end;

function DoGetUserName(Format: TNameFormat): string;
begin
  
end; 

function DoServerTypeToComputerKind(const ServerType: TNetServerType): TComputerKinds;
var
  Enum: TNetServerTypeEnum; 
begin
  Result := [];
  for Enum := Low(Enum) to High(Enum) do
    if Enum in ServerType then
      Result :=  Result + GComputerKindMap[Enum];
end;

function DoKindsToStr(Kinds: TComputerKinds; const Sep: string): string;
var
  Enum: TComputerKind; 
begin
  Result := '';
  for Enum := Low(Enum) to High(Enum) do
    if Enum in Kinds then
      Str.Add(Result, GKindStr[Enum], Sep);
end;

function DoMacToStr(const Mac: LargeInt): string;
begin
  Result := Mem.ToStr(@Mac, 6, 1, ':');
end;

end.
