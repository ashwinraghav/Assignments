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
  Windows, SysUtils,
  SilLiKey,
  SilLiEnumerator,
  SilLkNamedKeys,
  SilOiHandle,
  SilOmRegKey;

type
  TSilWindowsRegistryKeys = class(TSilNamedKeys)
  private
    FOwner: TSilWindowsRegistryKey;
    FKeyHandle: IHandle;
  protected // TSilNamedKeys
    function DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean; override;
    function DoRemove(const Name: String): Boolean; override;
    function DoAdd(const Name: String; Permision: TNamedKeyPermision): INamedKey; override;
    function DoGet(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean = false; RaiseError: Boolean = True): INamedKey; override;
    function DoExists(const Name: String): Boolean; override;
    function DoFind(const Name: String; out Key: INamedKey): Boolean; override;
  public
    constructor Create(const Owner: TSilWindowsRegistryKey);
  end;

implementation

uses
  SilBtStr,
  SilOsError,
  SilOgRegistry,
  SilOmRegEnumerators;
  
{ TSilWindowsRegistryKeys }

constructor TSilWindowsRegistryKeys.Create(const Owner: TSilWindowsRegistryKey);
begin
  inherited Create;
  FOwner := Owner;
  FKeyHandle := FOwner.Handle;
end;

function TSilWindowsRegistryKeys.DoGetEnumerator(out Enum: IEnumerator; Locked: Boolean): Boolean;
var
  NumSubKeys: LongWord;
begin
  Result := (Windows.RegQueryInfoKey(FKeyHandle.Value, nil, nil, nil, @NumSubKeys, nil, nil, nil, nil, nil, nil, nil) = ERROR_SUCCESS) and (NumSubKeys > 0);
  if Result then Enum := TRegistryKeyEnumerator.Create(Lockable, FKeyHandle.Value);
end;

function TSilWindowsRegistryKeys.DoAdd(const Name: String; Permision: TNamedKeyPermision): INamedKey;
var
  TempKey: HKey;
  Disposition: Integer;
begin
  if Windows.RegCreateKeyEx(FKeyHandle.Value, PChar(Name), 0, nil, REG_OPTION_NON_VOLATILE, GKeyPermisions[Permision], nil, TempKey, @Disposition) = ERROR_SUCCESS then
    Result := TSilWindowsRegistryKey.Create(FOwner, Name, true, TempKey) else
    OsError.Check(Windows.GetLastError(), 'TSilWindowsRegistryKeys.Add(%s) [Windows.RegCreateKeyEx]', [Name]);
end;

function TSilWindowsRegistryKeys.DoGet(const Name: String; Permision: TNamedKeyPermision; CanCreate: Boolean; RaiseError: Boolean): INamedKey;
var
  TempKey: HKey;
begin
  if Windows.RegOpenKeyEx(FKeyHandle.Value, PChar(Name), 0, GKeyPermisions[Permision], TempKey) = ERROR_SUCCESS then
    Result := TSilWindowsRegistryKey.Create(FOwner, Name, false, TempKey) else
  if CanCreate then
    Result := Add(Name)
  else if RaiseError then
    OsError.Check(Windows.GetLastError(), 'TSilWindowsRegistryKeys.Get(%s) [Windows.RegOpenKeyEx]', [Name]);
end;

function TSilWindowsRegistryKeys.DoRemove(const Name: String): Boolean;
var
  Code: Integer;
begin
  Code := Windows.RegDeleteKey(FKeyHandle.Value, PChar(Name));
  Result := Code = ERROR_SUCCESS;
end;

function TSilWindowsRegistryKeys.DoExists(const Name: String): Boolean;
begin
  Result := DoGet(Name, kpRead, False, False) <> nil;
end;

function TSilWindowsRegistryKeys.DoFind(const Name: String; out Key: INamedKey): Boolean;
begin
  Key := DoGet(Name, kpReadWrite, False, False);
  Result := Assigned(Key);
end;

end.
 
