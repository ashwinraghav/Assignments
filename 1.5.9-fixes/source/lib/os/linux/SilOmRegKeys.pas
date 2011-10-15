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

unit SilOmRegKeys;

{$I Defines.inc}

interface

uses
  (*)Windows, (*)SysUtils,
  SilLiKey,
  SilLiEnumerator,
  SilLkNamedKeys,
  SilOmRegKey;

type
	TSilLinuxRegistryKeys = class(TSilNamedKeys)
	private
		FOwner: TSilLinuxRegistryKey;
		(*)FKey: HKey;(*)
	protected // TSilNamedKeys
		function DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; override;
		function DoRemove(const Name: String): Boolean; override;
    function DoAdd(const Name: String; Permision: TNamedKeyPermision): INamedKey; override;
    function DoGet(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean = false; RaiseError: Boolean = True): INamedKey; override;
		function DoExists(const Name: String): Boolean; override;
		function DoFind(const Name: String; out Key: INamedKey): Boolean; override;
	public
    constructor Create(const Owner: TSilLinuxRegistryKey);
  end;

implementation

uses
  SilOsError,
  SilOgRegistry,
  SilOmRegEnumerators;

{ TSilLinuxRegistryKeys }

constructor TSilLinuxRegistryKeys.Create(const Owner: TSilLinuxRegistryKey);
begin
  raise Exception.CreateFmt('%s: not implemented', ['TSilLinuxRegistryKeys.Create']);
	inherited Create;
  FOwner := Owner;
(*)  FKey := FOwner.Key;(*)
end;

function TSilLinuxRegistryKeys.DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
(*)var
  NumSubKeys: LongWord;(*)
begin
(*)  Result := (RegQueryInfoKey(FKey, nil, nil, nil, @NumSubKeys, nil, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS) and (NumSubKeys > 0);(*)
(*)  if Result then Enum := TRegistryKeyEnumerator.Create(Lockable, FKey);(*)
end;

function TSilLinuxRegistryKeys.DoAdd(const Name: String; Permision: TNamedKeyPermision): INamedKey;
(*)var
  TempKey: HKey;
  Disposition: Integer;(*)
begin
(*)  if RegCreateKeyEx(FKey, PChar(Name), 0, nil, REG_OPTION_NON_VOLATILE, GKeyPermisions[Permision], nil, TempKey, @Disposition) = ERROR_SUCCESS then
    Result := TSilLinuxRegistryKey.Create(Name, true, TempKey) else
    OsError.Check(False, 'TSilLinuxRegistryKeys.Add [Linux.RegCreateKeyEx]');(*)
end;

function TSilLinuxRegistryKeys.DoGet(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean; RaiseError: Boolean): INamedKey;
(*)var
  TempKey: HKey;(*)
begin
(*)  if RegOpenKeyEx(FKey, PChar(Name), 0, GKeyPermisions[Permision], TempKey) = ERROR_SUCCESS then
    Result := TSilLinuxRegistryKey.Create(Name, false, TempKey) else
  if CanCreate then
    Result := Add(Name) else
    OsError.Check(False, 'TSilLinuxRegistryKeys.Get [Linux.RegOpenKeyEx]');(*)
end;

function TSilLinuxRegistryKeys.DoRemove(const Name: String): Boolean;
begin
(*)  Result := RegDeleteKey(FKey, PChar(Name)) = ERROR_SUCCESS;(*)
end;

function TSilLinuxRegistryKeys.DoExists(const Name: String): Boolean;
var
  e: IEnumerator;
  i: String;
begin
  while Enumerate(e, i) do
    if i = Name then
    begin
      Result := true;
      Exit;
    end;

  Result := false;
end;

function TSilLinuxRegistryKeys.DoFind(const Name: String; out Key: INamedKey): Boolean;
var
  e: IEnumerator;
  i: String;
begin
  while Enumerate(e, i) do
    if i = Name then
    begin
      Result := true;
      Key := Get(Name);
      Exit;
    end;

  Result := false;
end;

end.
 