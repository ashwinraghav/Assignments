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

unit SilOmRegistry;

{$I Defines.inc}

interface

uses
(*)  Windows, (*)SysUtils,
  SilLiKey, 
  SilOeRegistry,
  SilOmRegKey;

type
  TSilLinuxRegistry = class (TSilLinuxRegistryKey)
  private
    (*)function DoOpen(Source: String; Permision: TNamedKeyPermision; CanCreate: Boolean): HKey;(*)
  public
    constructor Create(const Source: String; Permision: TNamedKeyPermision; CanCreate: Boolean);
  end;

implementation

uses
  SilBtStr,
  SilOsError,
  SilOgRegistry,
  SilOmRegEnumerators;

{ TSilLinuxRegistry }

constructor TSilLinuxRegistry.Create(const Source: String; Permision: TNamedKeyPermision; CanCreate: Boolean);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxRegistry.Create']);
(*)  inherited Create(Source, False, DoOpen(Source, Permision, CanCreate));(*)
end;

(*)
function TSilLinuxRegistry.DoOpen(Source: String; Permision: TNamedKeyPermision; CanCreate: Boolean): HKey;
var
  i: Integer;
  TempKey: HKey;
  Disposition: Integer;
  sPath, sMachine: String;
begin
  sMachine := '';

  if Str.Compare(Source, '\\', 2) = 0 then
  begin
    i := Str.Pos('\', Source, 3);
    if i > 0 then sMachine := Str.Copy(Source, 1, i - 1);
    Str.Delete(Source, 1, i);
  end;

  i := Str.Pos('\', Source);

  if i > 0 then
  begin
    sPath := Str.Copy(Source, i + 1);
    Source := Str.Copy(Source, 1, i - 1);
  end else
    sPath := '';

  if Str.CompareText(Source, '$Classes', True) = 0 then
    Result := HKEY_CLASSES_ROOT
  else if Str.CompareText(Source, '$User', True) = 0 then
    Result := HKEY_CURRENT_USER
  else if Str.CompareText(Source, '$System', True) = 0 then
    Result := HKEY_LOCAL_MACHINE
  else if Str.CompareText(Source, '$Users', True) = 0 then
    Result := HKEY_USERS
  else if Str.CompareText(Source, '$PerfMon', True) = 0 then
    Result := HKEY_PERFORMANCE_DATA
  else if Str.CompareText(Source, '$Config', True) = 0 then
    Result := HKEY_CURRENT_CONFIG
  else if Str.CompareText(Source, '$Dynamic', True) = 0 then
    Result := HKEY_DYN_DATA
  else
    Result := HKEY_CURRENT_USER;

  if Str.NotEmpty(sMachine) then
    if RegConnectRegistry(PChar(sMachine), Result, TempKey) <> ERROR_SUCCESS then
      OsError.Check(False, 'TSilLinuxRegistry.Create [Linux.RegConnectRegistry]') else
      Result := TempKey;

  if RegOpenKeyEx(Result, PChar(sPath), 0, GKeyPermisions[Permision], TempKey) = ERROR_SUCCESS then
    Result := TempKey
  else if CanCreate then
    begin
      if RegCreateKeyEx(Result, PChar(sPath), 0, nil, REG_OPTION_NON_VOLATILE, KEY_ALL_ACCESS, nil, TempKey, @Disposition) <> ERROR_SUCCESS then
        OsError.Check(False, 'TSilLinuxRegistry.Create [Linux.RegCreateKeyEx]');
      Result := TempKey;
    end
  else
      OsError.Check(False, 'TSilLinuxRegistry.Create [Linux.RegOpenKeyEx]');
end;(*)

end.
